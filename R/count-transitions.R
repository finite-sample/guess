#' Count cross-wave response transitions
#'
#' Count paired transitions between pre-test and post-test item responses.
#' Responses may be incorrect (`0`), correct (`1`), or an observed don't-know
#' response (`"d"` or `"DK"`). Missing values are treated as don't-know
#' responses by default; use `na_as = "missing"` for structural missingness.
#'
#' @param pre_test Required vector of pre-test item responses.
#' @param post_test Required vector of post-test item responses.
#' @param ... Must be empty. Its presence requires optional arguments to be
#'   named.
#' @param subgroup Optional logical vector selecting observations to count.
#' @param na_as Classification of `NA` responses: `"dk"` treats them as
#'   observed don't-know responses and `"missing"` treats them as structural
#'   missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return A visible named integer vector. Binary schemas contain `x00`, `x01`,
#'   `x10`, and `x11`. The don't-know schema additionally contains `x0d`,
#'   `x1d`, `xd0`, `xd1`, and `xdd` in canonical row-major order.
#' @keywords internal
#'
#' @examples
#' pre_test <- c(1, 0, 0, 1, 0, 1, 0)
#' post_test <- c(1, 0, 1, 1, 0, 1, 1)
#' count_transitions(pre_test, post_test)
#'
#' pre_test <- c(1, 0, 0, 1, "d", "d", 0, 1, NA)
#' post_test <- c(1, NA, 1, "d", 1, 0, 1, 1, "d")
#' count_transitions(pre_test, post_test)
count_transitions_with_schema <- function(
  pre_test,
  post_test,
  ...,
  subgroup = NULL,
  response_schema = c("auto", "binary", "dk"),
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  validate_required(pre_test = pre_test, post_test = post_test)
  validate_equal_length(pre_test, post_test, "pre_test", "post_test")
  validate_subgroup(subgroup, length(pre_test))

  if (!is.null(subgroup)) {
    pre_test <- pre_test[subgroup]
    post_test <- post_test[subgroup]
  }

  response_schema <- match.arg(response_schema)
  na_as <- normalize_na_as(na_as)
  missing_action <- normalize_missing_action(missing_action)
  pre_test <- normalize_responses(pre_test, na_as, missing_action)
  post_test <- normalize_responses(post_test, na_as, missing_action)

  if (na_as == "missing") {
    complete <- !is.na(pre_test) & !is.na(post_test)
    pre_test <- pre_test[complete]
    post_test <- post_test[complete]
  }

  validate_transition_values(pre_test, post_test)

  has_dk <- any(pre_test == "d" | post_test == "d")
  if (response_schema == "binary" && has_dk) {
    stop(
      "Don't-know responses are not allowed when response_schema = \"binary\".",
      call. = FALSE
    )
  }

  include_dk <- response_schema == "dk" ||
    (response_schema == "auto" && has_dk)
  tabulate_transition_pairs(pre_test, post_test, include_dk = include_dk)
}

#' Count cross-wave response transitions
#'
#' Count paired transitions between pre-test and post-test item responses.
#' The response-cell schema is inferred after applying `na_as`: binary data use
#' four cells and observed don't-know responses use nine.
#'
#' @inheritParams count_transitions_with_schema
#' @return A visible named integer vector of binary or don't-know transition
#'   cells in canonical order.
#' @export
count_transitions <- function(
  pre_test,
  post_test,
  ...,
  subgroup = NULL,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  count_transitions_with_schema(
    pre_test,
    post_test,
    ...,
    subgroup = subgroup,
    response_schema = "auto",
    na_as = na_as,
    missing_action = missing_action
  )
}
