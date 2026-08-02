#' Normalize NA Responses
#'
#' Classifies NAs as observed don't know responses by default. If NAs represent
#' structural missingness, they can instead be omitted or rejected.
#' @param vec Required. Character or numeric vector.
#' @param na_as Classification of NA responses: `"dk"` (the default) or
#'   `"missing"`.
#' @param missing_action How to handle structural missingness: `"omit"` or
#'   `"error"`.
#' @return Character vector.
#' @export
#' @importFrom checkmate assert
#' @examples
#' x <- c(NA, 1, 0)
#' nona(x)
#' x <- c(NA, "dk", 0)
#' nona(x)
nona <- function(
  vec = NULL,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  assert(
    !is.null(vec),
    .var.name = "vec",
    info = "Input vector cannot be NULL"
  )

  result <- normalize_responses(vec, na_as, missing_action)
  if (normalize_na_as(na_as) == "missing") {
    result <- result[!is.na(result)]
  }
  result
}
