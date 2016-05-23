#' @importFrom magrittr %>%
NULL

#' Train models on specified grouping variable.
#'
#' Grouping defaults to Vowels, and will be added to any grouping already
#' present by default.
#'
#' @param data
#' @param grouping quoted column names to group by (strings, `quote`, or `~`)
#'   (default "Vowels")
#' @param formants quoted column names containing formants (or other measures).
#'   (default: c("F1", "F2"))
#' @param add_groups add `grouping` to existing groups, or overwrite (default TRUE)
#'
#' @return A data frame like for summarise, with columns for grouping factor, plus
#'   list columns `data` (the matrix used to train the model) and `model`. `model$mu`
#'   is the mean, and `model$Sigma` is the covariance matrix.  If `add_groups==TRUE`,
#'   original grouping will be maintained.
#'
#' @export
train_models <- function(data, grouping="Vowel", formants=c("F1", "F2"),
                         add_groups=TRUE) {

  existing_groups <- data %>% groups()

  models <-
    data %>%
    dplyr::group_by_(.dots = grouping, add=add_groups) %>%
    dplyr::select_(.dots = formants) %>%
    tidyr::nest() %>%
    mutate(data = purrr::map(data, as.matrix),
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
