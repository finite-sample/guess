#' Group Level Adjustment That Accounts for Propensity to Guess
#'
#' @description Adjusts observed 1s based on propensity to guess (based on
#'   observed 0s) and item level gamma.
#' You can also put in your best estimate of hidden knowledge behind don't know responses.
#'
#' @param pre  pre data frame. Required. Each vector within the data frame
#'   should only take values 0, 1, and 'd'.
#' @param pst  pst data frame. Required. Each vector within the data frame
#'   should only take values 0, 1, and 'd'.
#' @param gamma probability of getting the right answer without knowledge
#' @param dk    Numeric. Between 0 and 1. Hidden knowledge behind don't know
#'   responses. Default is .03.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes it and `"error"` rejects it.
#' @return nested list of pre and post adjusted responses, and adjusted learning estimates
#' @export
#' @examples
#' pre_test_var <- data.frame(pre = c(1, 0, 0, 1, "d", "d", 0, 1, NA))
#' pst_test_var <- data.frame(pst = c(1, NA, 1, "d", 1, 0, 1, 1, "d"))
#' gamma <- c(.25)
#' group_adj(pre_test_var, pst_test_var, gamma)
group_adj <- function(
  pre = NULL,
  pst = NULL,
  gamma = NULL,
  dk = 0.03,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  # Input validation
  validate_dataframe(pre, "pre")
  validate_dataframe(pst, "pst")
  validate_compatible_dataframes(pre, pst)
  validate_gamma(gamma)
  validate_dk(dk)
  response_data <- prepare_response_data(pre, pst, na_as, missing_action)
  pre <- response_data$pre
  pst <- response_data$post

  # Adj
  t1_guess <- 1 - mapply(function(x, y) {
    (sum(x == 0, na.rm = TRUE) / (1 / y - 1)) /
      sum(x == 1, na.rm = TRUE)
  }, pre, gamma)

  t2_guess <- 1 - mapply(function(x, y) {
    (sum(x == 0, na.rm = TRUE) / (1 / y - 1)) /
      sum(x == 1, na.rm = TRUE)
  }, pst, gamma)

  dt1_guess <- as.data.frame(mapply(
    function(x, y) ifelse(x == 1, y, x), pre, t1_guess
  ))
  dt2_guess <- as.data.frame(mapply(
    function(x, y) ifelse(x == 1, y, x), pst, t2_guess
  ))

  # Replace dk
  dt1_guess_dk <- as.data.frame(sapply(dt1_guess, function(x) {
    x[x == "d"] <- dk
    as.numeric(x)
  }))
  dt2_guess_dk <- as.data.frame(sapply(dt2_guess, function(x) {
    x[x == "d"] <- dk
    as.numeric(x)
  }))

  indiv_adj <- list(pre_adj = dt1_guess_dk, pst_adj = dt2_guess_dk)
  adj_learn <- colMeans(dt2_guess_dk - dt1_guess_dk, na.rm = TRUE)

  list(indiv = indiv_adj, learn = adj_learn)
}
