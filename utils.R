
#' Numerically stable sum of logged nubmers
#'
#' If any element of x is \code{Inf}, returns \code{Inf}. If all elements are
#' \code{-Inf}, returns all \code{-Inf}.
#'
#' @param x a numeric vector of logged values.
#' @return log of the sum of the exponentiated entries in x.
log_sum_exp <- function(x) {
  if (any(x == Inf)) return(Inf)
  if (all(x == -Inf)) return(-Inf)
  max_x <- max(x)
  log(sum(exp(x - max_x))) + max_x
}
#' Numerically stable mean of logged numbers
#'
#' Use for, e.g., calculating marginal log-likelihood
#'
#' @param x a numeric vector of logged values
#' @return the log of the mean of the exponentiated entries in x.
log_mean_exp <- function(x) log_sum_exp(x) - log(length(x))


#' Convert joint to marginal probabilities
#'
#' @param joint data frame with (possibly un-normalized) joint probabilities in
#'   the \code{lhood} column.
#' @param marginal_vars quoted names of columns with marginal variables
#'   (retained)
#' @return a data frame with columsn for \code{marginal_vars} and any
#'   pre-existing grouping, plus marginal log likelihood.
#'
#' @export
marginalize <- function(joint, marginal_vars) {
  walk(c('lhood', marginal_vars),
       ~ assert_that(has_name(joint, .)))

  joint %>%
    group_by_(.dots = marginal_vars, add=TRUE) %>%
    mutate(log_lhood = log(lhood)) %>%
    summarise(log_lhood = log_sum_exp(log_lhood))

}

#' Aggregate observations' likelihoods into single posterior
#'
#' @param lhoods data frame with one row per observation and hypothesis
#'   combination, and column \code{log_lhood} with log-likelihood of
#'   observations given hypothesis
#' @param hypothesis_vars quoted names of columns for hypothesis variables
#' @return a data frame with one row per combination of hypothesis variables,
#'   and total log likelihood, log posterior, and posterior probabilities in
#'   \code{log_lhood}, \code{log_posterior}, and \code{posterior}, respectively
#'
#' @export
aggregate_lhood <- function(lhoods, hypothesis_vars) {
  walk(c('log_lhood', hypothesis_vars),
       ~ assert_that(has_name(lhoods, .)))

  lhoods %>%
    group_by_(hypothesis_vars) %>%
    summarise(log_lhood = sum(log_lhood)) %>%
    ungroup() %>%
    mutate(log_posterior = log_lhood - log_sum_exp(log_lhood),
           posterior = exp(log_posterior))
}

#' Normalize posterior
#'
#' @param d data frame, grouping will be preserved
#' @param prob_var quoted name of column with probability to normalize
#' @return A data frame with a column \code{posterior} that sums to 1 within
#'   each group in \code{d}. Additionally, \code{log_posterior} and
#'   \code{posterior_choice} columns are created that have the log-posterior and
#'   TRUE for the maximum within each group/FALSE elsewhere, respectively.
#' @export
normalize_probability <- function(d, prob_var) {
  d %>%
    mutate_(temp__ = lazyeval::interp(~log(var), var=as.name(prob_var))) %>%
    normalize_log_probability('temp__') %>%
    mutate_(temp__ = ~NULL)
}

#' @describeIn normalize_probability Operate on log-probability
normalize_log_probability <- function(d, prob_var) {
  mutate_(d,
          log_posterior = lazyeval::interp(~var-log_sum_exp(var),
                                           var = as.name(prob_var)),
          posterior = ~exp(log_posterior),
          posterior_choice = ~posterior == max(posterior))
}
  
