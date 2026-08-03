# Input Validation Utilities
# Internal validation functions using checkmate
#' @importFrom checkmate assert_data_frame assert_numeric assert_subset
#' @importFrom checkmate assert_matrix assert_choice assert_logical
#' @importFrom checkmate assert assert_int assert_flag
NULL

#' Validate that input is a data frame
#' @param x input to validate
#' @param arg_name name of the argument for error messages
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_dataframe <- function(x, arg_name) {
  assert_data_frame(x, min.rows = 1, .var.name = arg_name)
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
  assert_numeric(
    lucky,
    lower = 0, upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = n_items,
    null.ok = FALSE,
    .var.name = "lucky"
  )

  if (!all(lucky > 0 & lucky < 1)) {
    stop("All lucky values must be between 0 and 1 (exclusive).")
  }
  TRUE
}

#' Validate transition matrix values
#' @param pre_test_var pre-test variable vector
#' @param pst_test_var post-test variable vector
#' @return TRUE if valid, throws error otherwise
validate_transition_values <- function(pre_test_var, pst_test_var) {
  valid_values <- c("1", "0", "d")
  unique_values <- unique(c(as.character(pre_test_var), as.character(pst_test_var)))
  unique_values <- unique_values[!is.na(unique_values)]

  assert_subset(
    unique_values,
    valid_values,
    .var.name = "input vector values"
  )
  TRUE
}

#' Normalize how NA responses are classified
#' @param na_as whether NA represents an observed don't know response or
#'   structural missingness
#' @return one of "dk" or "missing"
#' @keywords internal
normalize_na_as <- function(na_as) {
  match.arg(na_as, c("dk", "missing"))
}

#' Normalize structural missingness handling
#' @param missing_action how structural missingness should be handled
#' @return one of "omit" or "error"
#' @keywords internal
normalize_missing_action <- function(missing_action) {
  match.arg(missing_action, c("omit", "error"))
}

#' Normalize raw item responses
#' @param x response vector
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return character vector containing "0", "1", "d", or NA
#' @keywords internal
normalize_responses <- function(
  x,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  na_as <- normalize_na_as(na_as)
  missing_action <- normalize_missing_action(missing_action)
  x <- as.character(x)

  missing <- is.na(x) | toupper(x) == "NA"
  is_dk <- !missing & toupper(x) %in% c("D", "DK")
  x[is_dk] <- "d"

  if (na_as == "dk") {
    x[missing] <- "d"
  } else if (missing_action == "error" && any(missing)) {
    stop("Structural missing responses are not allowed when missing_action = \"error\".")
  } else {
    x[missing] <- NA_character_
  }

  invalid <- setdiff(unique(x[!is.na(x)]), c("0", "1", "d"))
  if (length(invalid) > 0L) {
    stop(
      "Responses must be coded as 0, 1, d/DK, or NA; found: ",
      paste(invalid, collapse = ", "),
      "."
    )
  }
  x
}

#' Normalize paired response data frames
#' @param pre_test pre-test response data frame
#' @param pst_test post-test response data frame
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return list containing normalized pre-test and post-test data
#' @keywords internal
prepare_response_data <- function(
  pre_test,
  pst_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  na_as <- normalize_na_as(na_as)
  missing_action <- normalize_missing_action(missing_action)
  normalize_frame <- function(x) {
    out <- as.data.frame(
      lapply(
        x,
        normalize_responses,
        na_as = na_as,
        missing_action = missing_action
      ),
      stringsAsFactors = FALSE
    )
    names(out) <- names(x)
    out
  }

  list(
    pre = normalize_frame(pre_test),
    post = normalize_frame(pst_test),
    na_as = na_as,
    missing_action = missing_action
  )
}

#' Validate gamma parameter
#' @param gamma probability parameter
#' @return TRUE if valid, throws error otherwise
validate_gamma <- function(gamma) {
  assert_numeric(
    gamma,
    lower = 0, upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = FALSE,
    .var.name = "gamma"
  )
  TRUE
}

#' Validate prior parameters
#' @param priors vector of prior parameters
#' @param expected_length expected length of priors vector
#' @param param_name name of parameter for error messages
#' @return TRUE if valid, throws error otherwise
validate_priors <- function(priors, expected_length, param_name) {
  assert_numeric(
    priors,
    lower = 0, upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = expected_length,
    .var.name = param_name
  )
  TRUE
}

#' Validate matrix input
#' @param x input to validate
#' @param arg_name name of the argument for error messages
#' @param valid_ncols optional vector of valid column counts
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_matrix <- function(x, arg_name, valid_ncols = NULL) {
  assert_matrix(x, min.rows = 1, .var.name = arg_name)

  if (!is.null(valid_ncols)) {
    assert_choice(ncol(x), valid_ncols, .var.name = paste0(arg_name, " column count"))
  }
  TRUE
}

#' Validate subgroup parameter
#' @param subgroup logical vector for subsetting
#' @param expected_length expected length to match
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_subgroup <- function(subgroup, expected_length) {
  if (!is.null(subgroup)) {
    assert_logical(subgroup, len = expected_length, .var.name = "subgroup")
  }
  TRUE
}

#' Validate that vectors have equal length
#' @param vec1 first vector
#' @param vec2 second vector
#' @param name1 name of first vector for error messages
#' @param name2 name of second vector for error messages
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_equal_length <- function(vec1, vec2, name1 = "vector1", name2 = "vector2") {
  if (length(vec1) != length(vec2)) {
    stop(paste(name1, "and", name2, "must have the same length."))
  }
  if (length(vec1) == 0) {
    stop("Input vectors cannot be empty.")
  }
  TRUE
}

#' Validate required parameters are not NULL
#' @param ... named arguments to check
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_required <- function(...) {
  args <- list(...)
  null_args <- names(args)[sapply(args, is.null)]

  if (length(null_args) > 0) {
    if (length(null_args) == 1) {
      stop(paste(null_args, "must be provided."))
    } else {
      stop(paste("All of", paste(null_args, collapse = ", "), "must be provided."))
    }
  }
  TRUE
}

#' Validate dk parameter (knowledge behind don't know responses)
#' @param dk numeric value between 0 and 1
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_dk <- function(dk) {
  assert_numeric(
    dk,
    lower = 0, upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = 1,
    .var.name = "dk"
  )
  TRUE
}
