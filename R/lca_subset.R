#' Person Level Adjustment
#'
#' @description Adjusts observed 1s based on item level parameters of the LCA
#'   model. Currently only takes data with Don't Know. And treats don't know
#'   responses as true confessions on ignorance.
#' By default, NAs are treated as acknowledgments of ignorance.
#' @param pre_test Pre-test data frame.
#' @param post_test Post-test data frame.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes it and `"error"` rejects it.
#' @return list of pre and post adjusted responses
#' @export
#'
#' @examples
#' pre_test <- data.frame(item = c(1, 0, 0, 1, "d", "d", 0, 1, NA))
#' post_test <- data.frame(item = c(1, NA, 1, "d", 1, 0, 1, 1, "d"))
#' lca_adj(pre_test, post_test)
lca_adj <- function(
  pre_test = NULL,
  post_test = NULL,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  validate_compatible_dataframes(pre_test, post_test)

  response_data <- prepare_response_data(pre_test, post_test, na_as, missing_action)
  pre_test <- response_data$pre
  post_test <- response_data$post

  transmatrix <- count_item_transitions(pre_test, post_test, na_as = "missing")

  lca_res <- fit_item_lca_counts(transmatrix)
  param_lca <- lca_res$params

  is_dk <- nrow(param_lca) == 8L
  n_pre <- vapply(pre_test, function(x) sum(!is.na(x)), integer(1))
  n_pst <- vapply(post_test, function(x) sum(!is.na(x)), integer(1))
  correct_pre <- vapply(pre_test, function(x) sum(x == 1, na.rm = TRUE), integer(1))
  correct_pst <- vapply(post_test, function(x) sum(x == 1, na.rm = TRUE), integer(1))

  if (is_dk) {
    # Who knows the item at each wave. Knowledge is not lost over the process,
    # so the only class knowing it beforehand is kk; afterwards it is kk plus
    # those who learned it from either starting state.
    pk1 <- n_pre * param_lca["kk", ] / correct_pre

    pk2 <- n_pst *
      (param_lca["gk", ] + param_lca["kk", ] + param_lca["dk", ]) /
      correct_pst
  } else {
    pk1 <- n_pre * param_lca["kk", ] / correct_pre

    pk2 <- n_pst * (param_lca["gk", ] + param_lca["kk", ]) / correct_pst
  }

  t1adj <- as.data.frame(mapply(function(x, y) ifelse(x == 1, y, x), pre_test, pk1))
  t2adj <- as.data.frame(mapply(function(x, y) ifelse(x == 1, y, x), post_test, pk2))

  t1adj <- sapply(t1adj, function(x) {
    x[x == "d"] <- 0
    x
  })
  t2adj <- sapply(t2adj, function(x) {
    x[x == "d"] <- 0
    x
  })

  list(pre = t1adj, pst = t2adj)
}
