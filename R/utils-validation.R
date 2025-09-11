# Input Validation Utilities
# Internal validation functions used across the package
# @keywords internal

#' Validate that input is a data frame
#' @param x input to validate
#' @param arg_name name of the argument for error messages
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_dataframe <- function(x, arg_name) {
  if (!is.data.frame(x)) {
    stop(paste("Specify", arg_name, "data.frame."))
  }
  if (nrow(x) == 0) {
    stop(paste(arg_name, "cannot be empty."))
  }
  TRUE
}

#' Validate that two data frames have compatible dimensions
#' @param pre_test pre-test data frame
#' @param pst_test post-test data frame
#' @return TRUE if valid, throws error otherwise
validate_compatible_dataframes <- function(pre_test, pst_test) {
  if (length(pre_test) != length(pst_test)) {
    stop("Lengths of pre_test and pst_test must be the same.")
  }
  if (nrow(pre_test) != nrow(pst_test)) {
    stop("Number of rows in pre_test and pst_test must be the same.")
  }
  TRUE
}

#' Validate lucky vector for standard correction
#' @param lucky vector of guessing probabilities
#' @param n_items number of items to validate against
#' @return TRUE if valid, throws error otherwise
validate_lucky_vector <- function(lucky, n_items) {
  if (is.null(lucky)) {
    stop("Specify lucky vector.")
  }
  if (length(lucky) != n_items) {
    stop("Length of input varies.\n",
         "Length of pre_test, pst_test, and lucky must be the same.")
  }
  if (any(lucky <= 0 | lucky >= 1)) {
    stop("All lucky values must be between 0 and 1 (exclusive).")
  }
  TRUE
}

#' Validate transition matrix values
#' @param pre_test_var pre-test variable vector
#' @param pst_test_var post-test variable vector
#' @return TRUE if valid, throws error otherwise
validate_transition_values <- function(pre_test_var, pst_test_var) {
  pre_test_nona <- nona(as.character(pre_test_var))
  pst_test_nona <- nona(as.character(pst_test_var))
  
  valid_values <- c(NA_character_, "1", "0", "d")
  if (!all(unique(c(pre_test_nona, pst_test_nona)) %in% valid_values)) {
    stop("The input vectors can only contain: 0, 1, NA, d")
  }
  TRUE
}

#' Validate gamma parameter
#' @param gamma probability parameter
#' @return TRUE if valid, throws error otherwise
validate_gamma <- function(gamma) {
  if (is.null(gamma)) {
    stop("Gamma parameter cannot be NULL.")
  }
  if (any(is.na(gamma))) {
    stop("Gamma parameter cannot contain NA values.")
  }
  if (any(gamma < 0 | gamma > 1)) {
    stop("Gamma parameter must be between 0 and 1.")
  }
  TRUE
}

#' Validate prior parameters
#' @param priors vector of prior parameters
#' @param expected_length expected length of priors vector
#' @param param_name name of parameter for error messages
#' @return TRUE if valid, throws error otherwise
validate_priors <- function(priors, expected_length, param_name) {
  if (!is.numeric(priors)) {
    stop(paste(param_name, "must be numeric."))
  }
  if (length(priors) != expected_length) {
    stop(paste(param_name, "must have length", expected_length))
  }
  if (any(priors < 0 | priors > 1)) {
    stop(paste("All", param_name, "values must be between 0 and 1."))
  }
  TRUE
}
