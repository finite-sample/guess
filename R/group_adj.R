#' Group Level Adjustment That Accounts for Propensity to Guess
#'
#' @description Adjusts observed 1s based on propensity to guess (based on
#'   observed 0s) and an item-level guessing probability.
#' You can also put in your best estimate of hidden knowledge behind don't know responses.
#'
#' @param pre_test Pre-test data frame. Required. Each vector within the data frame
#'   should only take values 0, 1, and 'd'.
#' @param post_test Post-test data frame. Required. Each vector within the data frame
#'   should only take values 0, 1, and 'd'.
#' @param guessing_probability Probability of getting the right answer without knowledge.
#' @param knowledge_given_dont_know Numeric probability of hidden knowledge
#'   conditional on an observed don't-know response. Must be between 0 and 1.
#'   Defaults to 0.03.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes it and `"error"` rejects it.
#' @return A list with `adjusted_responses`, containing `pre_test` and `post_test`
#'   data frames, and `mean_learning`, the item-level mean adjusted change.
#' @export
#' @examples
#' pre_test_var <- data.frame(item = c(1, 0, 0, 1, "d", "d", 0, 1, NA))
#' post_test_var <- data.frame(item = c(1, NA, 1, "d", 1, 0, 1, 1, "d"))
#' guessing_probability <- c(.25)
#' group_adj(pre_test_var, post_test_var, guessing_probability)
group_adj <- function(
  pre_test = NULL,
  post_test = NULL,
  guessing_probability = NULL,
  knowledge_given_dont_know = 0.03,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  # Input validation
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  validate_compatible_dataframes(pre_test, post_test)
  validate_gamma(guessing_probability)
  validate_dk(knowledge_given_dont_know)
  response_data <- prepare_response_data(pre_test, post_test, na_as, missing_action)
  pre_test <- response_data$pre
  post_test <- response_data$post

  # Adj
  t1_guess <- 1 - mapply(function(x, y) {
    (sum(x == 0, na.rm = TRUE) / (1 / y - 1)) /
      sum(x == 1, na.rm = TRUE)
  }, pre_test, guessing_probability)

  t2_guess <- 1 - mapply(function(x, y) {
    (sum(x == 0, na.rm = TRUE) / (1 / y - 1)) /
      sum(x == 1, na.rm = TRUE)
  }, post_test, guessing_probability)

  dt1_guess <- as.data.frame(mapply(
    function(x, y) ifelse(x == 1, y, x), pre_test, t1_guess
  ))
  dt2_guess <- as.data.frame(mapply(
    function(x, y) ifelse(x == 1, y, x), post_test, t2_guess
  ))

  # Replace dk
  dt1_guess_dk <- as.data.frame(sapply(dt1_guess, function(x) {
    x[x == "d"] <- knowledge_given_dont_know
    as.numeric(x)
  }))
  dt2_guess_dk <- as.data.frame(sapply(dt2_guess, function(x) {
    x[x == "d"] <- knowledge_given_dont_know
    as.numeric(x)
  }))

  adjusted_responses <- list(pre_test = dt1_guess_dk, post_test = dt2_guess_dk)
  mean_learning <- colMeans(dt2_guess_dk - dt1_guess_dk, na.rm = TRUE)

  list(adjusted_responses = adjusted_responses, mean_learning = mean_learning)
}
