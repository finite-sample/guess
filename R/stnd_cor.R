#' Standard Guessing Correction for Learning
#'
#' Estimate of learning adjusted with standard correction for guessing.
#' Correction is based on number of options per question.
#' The function takes separate pre-test and post-test dataframes. Why do we
#' need dataframes? To accomodate multiple items. The items can carry NA
#' (missing). Items must be in the same order in each dataframe. Assumes that
#' respondents are posed same questions twice.
#' The function also takes a \code{guessing_probability} vector --- the chance of getting a
#' correct answer if guessing randomly. Each entry is 1/(number of options).
#' The function also optionally takes a vector carrying names of the items.
#' By default, the vector carrying adjusted learning estimates takes same
#' item names as the pre_test items. However you can assign a vector of names
#' separately via \code{item_names}.
#'
#' @param pre_test Required. data.frame carrying responses to pre-test questions.
#' @param post_test Required. data.frame carrying responses to post-test questions.
#' @param guessing_probability Required. A vector. Each entry is 1/(number of options)
#' @param item_names Optional. A vector carrying item names.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes it and `"error"` rejects it.
#'
#' @return   a list of three vectors, carrying pre-treatment corrected scores,
#'   post-treatment scores, and adjusted estimates of learning
#' @export
#' @examples
#' # Without DK
#' pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0))
#' post_test <- pre_test + cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 0, 1))
#' guessing_probability <- rep(.25, 2)
#' stnd_cor(pre_test, post_test, guessing_probability)
#' # With DK
#' pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0, "d", 0), item2 = c(1, NA, 0, 1, 0, "d", "d"))
#' post_test <- data.frame(item1 = c(1, 0, 0, 1, 0, "d", 1), item2 = c(1, NA, 0, 1, 0, 1, "d"))
#' guessing_probability <- rep(.25, 2)
#' stnd_cor(pre_test, post_test, guessing_probability)
stnd_cor <- function(pre_test = NULL, post_test = NULL, guessing_probability = NULL,
                     item_names = NULL, na_as = c("dk", "missing"),
                     missing_action = c("omit", "error")) {
  # Input validation using utilities
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  validate_compatible_dataframes(pre_test, post_test)
  validate_guessing_probability(guessing_probability, length(pre_test))
  response_data <- prepare_response_data(
    pre_test, post_test, na_as, missing_action
  )
  pre_test <- response_data$pre
  post_test <- response_data$post

  corrected_total <- function(x, chance) {
    sum(x == 1, na.rm = TRUE) -
      sum(x == 0, na.rm = TRUE) / (1 / chance - 1)
  }

  pre_test_cor <- mapply(
    corrected_total, pre_test, guessing_probability,
    SIMPLIFY = TRUE, USE.NAMES = FALSE
  )
  pst_test_cor <- mapply(
    corrected_total, post_test, guessing_probability,
    SIMPLIFY = TRUE, USE.NAMES = FALSE
  )

  # Names of the return vector
  if (is.null(item_names)) {
    names(pre_test_cor) <- names(pst_test_cor) <- names(pre_test)
  } else {
    names(pre_test_cor) <- names(pst_test_cor) <- item_names
  }

  # Marginal scores use all observed responses. Learning is a paired difference,
  # so both corrected totals and its denominator use the same respondents.
  n_pre <- vapply(pre_test, function(x) sum(!is.na(x)), integer(1))
  n_pst <- vapply(post_test, function(x) sum(!is.na(x)), integer(1))
  n_both <- vapply(
    seq_along(pre_test),
    function(j) sum(!is.na(pre_test[[j]]) & !is.na(post_test[[j]])),
    integer(1)
  )

  learn_cor <- vapply(
    seq_along(pre_test),
    function(j) {
      complete <- !is.na(pre_test[[j]]) & !is.na(post_test[[j]])
      corrected_total(post_test[[j]][complete], guessing_probability[j]) -
        corrected_total(pre_test[[j]][complete], guessing_probability[j])
    },
    numeric(1)
  )

  pre <- ifelse(n_pre > 0L, pre_test_cor / n_pre, NA_real_)
  pst <- ifelse(n_pst > 0L, pst_test_cor / n_pst, NA_real_)
  learn <- ifelse(n_both > 0L, learn_cor / n_both, NA_real_)

  names(pre) <- names(pst) <- names(learn) <- names(pre_test_cor)

  list(pre = pre, pst = pst, learn = learn)
}
