#' @importFrom magrittr %>%
NULL


#' Extract matrix of cues from data frame columns
#'
#' @param d data frame
#' @param cues quoted names of columns to pull out
#' @return matrix with values from named columns
#'
#' @export
cue_matrix <- function(d, cues)
  d %>%
    select_(.dots=cues) %>%
    as.matrix()


#' Train models on specified grouping variable.
#'
#' Grouping will be added to any grouping already present by default.
#'
#' @param data Data source with columns for grouping and "cues"
#' @param grouping quoted column names to group by (strings, `quote`, or `~`)
#' @param cues quoted column names containing cues
#' @param add_groups add `grouping` to existing groups, or overwrite (default TRUE)
#'
#' @return A data frame like for summarise, with columns for grouping factor, plus
#'   list columns `data` (the matrix used to train the model) and `model`. `model$mu`
#'   is the mean, and `model$Sigma` is the covariance matrix.  If `add_groups==TRUE`,
#'   original grouping will be maintained.
#'
#' @export
train_models <- function(data, grouping, cues, add_groups=TRUE) {

  existing_groups <- data %>% groups()

  models <-
    data %>%
    dplyr::group_by_(.dots = grouping, add=add_groups) %>%
    tidyr::nest() %>%
    mutate(data = map(data, ~ cue_matrix(.x, cues)),
           model = purrr::map(data,
                              ~ list(mu    = apply(., 2, mean),
                                     Sigma = cov(.))))

  ## if training groups were added, restore the original groups
  if (add_groups) {
    models %>% dplyr::group_by_(.dots = existing_groups)
  } else {
    models
  }
}

#' Split data into training and test sets
#'
#' @param d data frame
#' @param holdout quoted name of column that defines train/test splits.
#' @param groups quoted name of column(s) to group training data by. optional,
#'   will be preserved in output (and added as grouping to training data).
#' @return data frame with columns `data_train`, `data_test`, one for each level
#'   of holdout and group (if specified), which are also included. `data_test`
#'   has the corresponding subset of the input data, and `data_train` has the
#'   rest.
#'
#' @export
train_test_split <- function(d, holdout, groups=NULL) {

  d %>%
    group_by_(holdout) %>%
    summarise() %>%
    purrrlyr::by_row(~ anti_join(d, ., by=holdout) %>%
                       group_by_(.dots=groups),
                     .to = 'data_train') %>%
    inner_join(d %>%
                 group_by_(holdout, .dots=groups) %>%
                 nest(.key='data_test'),
               by = holdout)

}

#' Train indexical models with cross-validation
#'
#' Combines \code{\link{train_test_split}}, \code{\link{train_models}}, and
#' \code{\link{list_models}} to produce a list of indexical groups' models.
#'
#' Each group's indexical model is a mixture of models at the linguistic level
#' 
#' @param d data frame (a la `nsp_vows`)
#' @param groups quoted name of indexical grouping variable column (e.g.,
#'   'Dialect'). One model will be created for each level of this variable.
#' @param category quoted name of column for linguistic category. Each
#'   indexical group's model is a list of individual category models
#' @param cues quoted name(s) of column(s) with cue values
#' @param holdout ='Talker' unit to perform cross-validation on. one row per
#'   level of this variable is created with models trained after removing the
#'   corresponding level.
#' @param ... additional arguments are passed to \code{\link{train_models}}
#' @return the dataframe returned by \code{\link{train_test_split}}, plus a
#'   models list column, each entry of which is a model for one level of
#'   \code{groups} after holding out that row's Talker (or level of holdout).
#'
#' @export
train_models_indexical_with_holdout <- function(d, groups, category, cues,
                                                holdout='Talker',
                                                ...) {

  train <- purrr::partial(train_models, grouping = category, cues = cues, ...)

  # TODO: can be made much more efficient by only re-training the model for the
  # Matching dialect.
  d %>%
    train_test_split(holdout=holdout, groups=groups) %>%
    mutate(models = map(data_train,
                        ~ .x %>%
                          train() %>%
                          by_slice(~ list_models(., category), .to='model') %>%
                          list_models(groups)))

}


#' Randomly subsample data by group
#'
#' Wraps dplyr::sample_n.  Subsampling will be done at the level of the
#' specified \code{group} column, \emph{within} any grouping that already exists
#' in the input tbl.
#'
#' @param tbl tbl of data
#' @param group (quoted) grouping variable
#' @param n number of samples
#' @param ... additional arguments are passed to dplyr::sample_n.
#' @return A tbl with \code{n} levels of \code{group} randomly sampled within
#'   each existing grouping level of \code{tbl}. Any grouping of \code{tbl} is
#'   preserved in the return value.
#'
#' @export
sample_n_groups <- function(tbl, group, n, ...) {
  tbl_groups <- tbl %>% groups() %>% as.character()
  tbl %>%
    group_by_(group, add=TRUE) %>%
    summarise() %>%
    group_by_(.dots=tbl_groups) %>%
    sample_n(n, ...) %>%
    left_join(tbl, by=c(group, tbl_groups))
}


#' Train indexical group models with holdout and subsampling talkers
#'
#' Like \code{\link{train_models_indexical_with_holdout}}, but randomly
#' sub-samples training data to have specified number of talkers.
#'
#' @param d data frame (a la `nsp_vows`)
#' @param groups quoted name of indexical grouping variable column (e.g.,
#'   'Dialect'). One model will be created for each level of this variable.
#' @param n_subsample Number of holdout levels to subsample for training data.
#' @param category quoted name of column for linguistic category. Each indexical
#'   group's model is a list of individual category models
#' @param cues quoted name(s) of column(s) with cue values
#' @param holdout ='Talker' unit to perform cross-validation on. one row per
#'   level of this variable is created with models trained after removing the
#'   corresponding level.
#' @param ... additional arguments are passed to \code{\link{train_models}}
#' @return the dataframe returned by \code{\link{train_test_split}}, with
#'   data_train replaced by the subsampled version for each talker, plus a
#'   models list column, each entry of which is a model for one level of
#'   \code{groups} after holding out that row's Talker (or level of holdout).
#' 
#' @export
train_models_indexical_subsample_holdout <- function(d, groups, n_subsample,
                                                     category, cues,
                                                     holdout = 'Talker', ...) {
  d %>%
    train_test_split(holdout = holdout, groups = groups) %>%
    mutate(data_train = map(data_train,
                            . %>% sample_n_groups(holdout, n_subsample)),
           models = map(data_train,
                        ~ .x %>%
                          train_models(grouping=category, cues=cues, ...) %>%
                          by_slice(~ list_models(., category), .to='model') %>%
                          list_models(groups)))
}
