#' Count transitions for matched test items
#'
#' Apply [count_transitions()] to each named item shared by pre-test and
#' post-test data. Item columns are paired by name, not position.
#'
#' @param pre_test Data frame containing one pre-test item per column.
#' @param post_test Data frame containing the corresponding post-test items.
#' @param ... Must be empty. Its presence requires optional arguments to be
#'   named.
#' @param subgroup Optional logical vector selecting observations to count.
#' @param include_aggregate Whether to append an `aggregate` row containing
#'   column sums across items.
#' @param na_as Classification of `NA` responses: `"dk"` treats them as
#'   observed don't-know responses and `"missing"` treats them as structural
#'   missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return A visible integer matrix with items in rows and transition cells in
#'   columns. Row names preserve the item names from `pre_test`.
#' @keywords internal
#'
#' @examples
#' pre_test <- data.frame(
#'   item1 = c(1, 0, 0, 1, 0),
#'   item2 = c(1, NA, 0, 1, 0)
#' )
#' post_test <- data.frame(
#'   item1 = c(1, 1, 1, 1, 0),
#'   item2 = c(1, 1, 0, 1, 1)
#' )
#' count_item_transitions(pre_test, post_test)
count_item_transitions_impl <- function(
  pre_test,
  post_test,
  ...,
  subgroup = NULL,
  response_schema = c("auto", "binary", "dk"),
  include_aggregate = FALSE,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }

  assert_data_frame(
    pre_test,
    min.rows = 1L,
    min.cols = 1L,
    .var.name = "pre_test"
  )
  assert_data_frame(
    post_test,
    nrows = nrow(pre_test),
    min.cols = 1L,
    .var.name = "post_test"
  )
  item_names <- validate_paired_item_names(pre_test, post_test)
  validate_subgroup(subgroup, nrow(pre_test))
  assert_flag(include_aggregate, .var.name = "include_aggregate")

  if (include_aggregate && "aggregate" %in% item_names) {
    stop(
      "An item cannot be named `aggregate` when include_aggregate = TRUE.",
      call. = FALSE
    )
  }

  response_schema <- match.arg(response_schema)
  na_as <- normalize_na_as(na_as)
  missing_action <- normalize_missing_action(missing_action)

  transitions <- lapply(item_names, function(item_name) {
    count_transitions_with_schema(
      pre_test[[item_name]],
      post_test[[item_name]],
      subgroup = subgroup,
      response_schema = response_schema,
      na_as = na_as,
      missing_action = missing_action
    )
  })

  format_transition_matrix(
    transitions,
    item_names,
    include_aggregate = include_aggregate
  )
}

#' Count transitions for matched test items
#'
#' Apply [count_transitions()] to each named item shared by pre-test and
#' post-test data. The response-cell schema is inferred after applying `na_as`.
#'
#' @inheritParams count_item_transitions_impl
#' @return A visible integer matrix with items in rows and inferred transition
#'   cells in columns.
#' @export
count_item_transitions <- function(
  pre_test,
  post_test,
  ...,
  subgroup = NULL,
  include_aggregate = FALSE,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  count_item_transitions_impl(
    pre_test,
    post_test,
    ...,
    subgroup = subgroup,
    response_schema = "auto",
    include_aggregate = include_aggregate,
    na_as = na_as,
    missing_action = missing_action
  )
}
