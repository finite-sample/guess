#' transmat: Cross-wave transition matrix
#'
#' @description Prints Cross-wave transition matrix and returns the vector behind the matrix.
#' Missing values are treated as don't know responses by default. Set
#' `na_as = "missing"` when they instead represent structural missingness.
#' @param pre_test_var Required. A vector carrying pre-test scores of a particular item. Only
#' @param pst_test_var Required. A vector carrying post-test scores of a particular item
#' @param subgroup     Optional. A Boolean vector indicating rows of the relevant subset.
#' @param force9       Optional. There are cases where DK data doesn't have DK.
#'   But we need the entire matrix. By default it is FALSE.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#' @return a numeric vector.
#' Assume 1 denotes correct, 0 incorrect, and d/DK an observed don't know.
#' When there is no don't know option and no missing, the entries are: x00, x10, x01, x11
#' When there is a don't know option, the entries of the vector are:
#'   x00, x10, xd0, x01, x11, xd1, xd0, x1d, xdd
#' @export
#' @examples
#' pre_test_var <- c(1, 0, 0, 1, 0, 1, 0)
#' pst_test_var <- c(1, 0, 1, 1, 0, 1, 1)
#' transmat(pre_test_var, pst_test_var)
#'
#' # With NAs
#' pre_test_var <- c(1, 0, 0, 1, "d", "d", 0, 1, NA)
#' pst_test_var <- c(1, NA, 1, "d", 1, 0, 1, 1, "d")
#' transmat(pre_test_var, pst_test_var)
transmat <- function(pre_test_var, pst_test_var, subgroup = NULL,
                     force9 = FALSE, na_as = c("dk", "missing"),
                     missing_action = c("omit", "error")) {
  # Input validation
  validate_required(pre_test_var = pre_test_var, pst_test_var = pst_test_var)
  validate_equal_length(pre_test_var, pst_test_var, "pre_test_var", "pst_test_var")

  # Apply subgroup filter if provided
  validate_subgroup(subgroup, length(pre_test_var))
  if (!is.null(subgroup)) {
    pre_test_var <- pre_test_var[subgroup]
    pst_test_var <- pst_test_var[subgroup]
  }

  # Process and validate responses
  na_as <- normalize_na_as(na_as)
  missing_action <- normalize_missing_action(missing_action)
  pre_test_clean <- normalize_responses(pre_test_var, na_as, missing_action)
  pst_test_clean <- normalize_responses(pst_test_var, na_as, missing_action)

  if (na_as == "missing") {
    complete <- !is.na(pre_test_clean) & !is.na(pst_test_clean)
    pre_test_clean <- pre_test_clean[complete]
    pst_test_clean <- pst_test_clean[complete]
  }

  # Use validation utility
  validate_transition_values(pre_test_clean, pst_test_clean)

  # Count transitions using utility function
  transitions <- count_transitions(pre_test_clean, pst_test_clean)

  # Force DK matrix format if requested
  if (force9 && length(transitions) == 4) {
    # Convert 4-element to 9-element format with zeros for DK transitions
    dk_transitions <- rep(0, 9)
    names(dk_transitions) <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
    dk_transitions[c("x00", "x01", "x10", "x11")] <- transitions[c("x00", "x01", "x10", "x11")]
    transitions <- dk_transitions
  }

  invisible(transitions)
}
