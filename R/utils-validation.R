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

#' Validate names of paired item data
#' @param pre_test pre-test item data frame
#' @param post_test post-test item data frame
#' @return item names in pre-test column order
#' @keywords internal
validate_paired_item_names <- function(pre_test, post_test) {
  pre_names <- names(pre_test)
  post_names <- names(post_test)

  if (anyNA(pre_names) || any(!nzchar(pre_names)) || anyDuplicated(pre_names)) {
    stop("pre_test must have unique, non-empty item names.", call. = FALSE)
  }
  if (anyNA(post_names) || any(!nzchar(post_names)) || anyDuplicated(post_names)) {
    stop("post_test must have unique, non-empty item names.", call. = FALSE)
  }
  if (!setequal(pre_names, post_names)) {
    stop("pre_test and post_test must contain the same item names.", call. = FALSE)
  }

  pre_names
}

#' Validate guessing probabilities for standard correction
#' @param guessing_probability vector of guessing probabilities
#' @param n_items number of items to validate against
#' @return TRUE if valid, throws error otherwise
validate_guessing_probability <- function(guessing_probability, n_items) {
  assert_numeric(
    guessing_probability,
    lower = 0, upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = n_items,
    null.ok = FALSE,
    .var.name = "guessing_probability"
  )

  if (!all(guessing_probability > 0 & guessing_probability < 1)) {
    stop("All guessing probabilities must be between 0 and 1 (exclusive).")
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

#' Validate and order transition counts
#' @param x transition-count matrix
#' @param arg_name argument name for errors
#' @return validated matrix in canonical cell order
#' @keywords internal
prepare_transition_counts <- function(x, arg_name = "transition_counts") {
  assert_matrix(x, min.rows = 1L, .var.name = arg_name)
  if (!is.numeric(x)) {
    stop(arg_name, " must be a numeric matrix.", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(arg_name, " must contain only finite, non-missing counts.", call. = FALSE)
  }
  if (any(x < 0)) {
    stop(arg_name, " cannot contain negative counts.", call. = FALSE)
  }
  integer_tolerance <- sqrt(.Machine$double.eps) * pmax(1, abs(x))
  if (any(abs(x - round(x)) > integer_tolerance)) {
    stop(arg_name, " must contain whole-number counts.", call. = FALSE)
  }
  x <- round(x)

  expected_cells <- switch(
    as.character(ncol(x)),
    `4` = c("x00", "x01", "x10", "x11"),
    `9` = c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd"),
    NULL
  )
  if (is.null(expected_cells)) {
    stop(arg_name, " must have 4 or 9 transition columns.", call. = FALSE)
  }

  cell_names <- colnames(x)
  if (
    is.null(cell_names) || anyNA(cell_names) || any(!nzchar(cell_names)) ||
      anyDuplicated(cell_names) || !setequal(cell_names, expected_cells)
  ) {
    stop(
      arg_name,
      " must have the canonical transition-cell names: ",
      paste(expected_cells, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  item_names <- rownames(x)
  if (
    is.null(item_names) || anyNA(item_names) || any(!nzchar(item_names)) ||
      anyDuplicated(item_names)
  ) {
    stop(arg_name, " must have unique, non-empty item names.", call. = FALSE)
  }

  x <- x[, expected_cells, drop = FALSE]
  empty_items <- item_names[rowSums(x) == 0]
  if (length(empty_items) > 0L) {
    stop(
      "Every item must contain at least one transition; empty: ",
      paste(empty_items, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  x
}

#' Validate subgroup parameter
#' @param subgroup logical vector for subsetting
#' @param expected_length expected length to match
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_subgroup <- function(subgroup, expected_length) {
  if (!is.null(subgroup)) {
    assert_logical(
      subgroup,
      any.missing = FALSE,
      len = expected_length,
      .var.name = "subgroup"
    )
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

#' Validate an item-level latent-class fit
#' @param fit fitted item-level latent-class model
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_item_lca_fit <- function(fit) {
  if (!inherits(fit, "guess_fit")) {
    stop("fit must be a guess_fit object.", call. = FALSE)
  }
  if (!identical(fit$model_type, "nodk") && !identical(fit$model_type, "dk")) {
    stop("fit$model_type must be either \"nodk\" or \"dk\".", call. = FALSE)
  }
  if (!is.matrix(fit$params) || !is.numeric(fit$params)) {
    stop("fit$params must be a numeric matrix.", call. = FALSE)
  }
  if (anyNA(fit$params) || any(!is.finite(fit$params))) {
    stop("fit$params must contain only finite values.", call. = FALSE)
  }
  parameter_names <- if (fit$model_type == "dk") {
    c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
  } else {
    c("gg", "gk", "kk", "gamma")
  }
  if (
    is.null(rownames(fit$params)) || anyDuplicated(rownames(fit$params)) ||
      !setequal(rownames(fit$params), parameter_names)
  ) {
    stop(
      "fit$params must have parameter rows: ",
      paste(parameter_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    is.null(colnames(fit$params)) || anyNA(colnames(fit$params)) ||
      any(!nzchar(colnames(fit$params))) || anyDuplicated(colnames(fit$params))
  ) {
    stop("fit$params must have unique, non-empty item names.", call. = FALSE)
  }
  params <- fit$params[parameter_names, , drop = FALSE]
  class_weights <- params[setdiff(parameter_names, "gamma"), , drop = FALSE]
  if (any(class_weights < 0 | class_weights > 1)) {
    stop("Latent-class weights in fit$params must lie between 0 and 1.", call. = FALSE)
  }
  if (any(abs(colSums(class_weights) - 1) > sqrt(.Machine$double.eps))) {
    stop("Latent-class weights in fit$params must sum to 1 for every item.", call. = FALSE)
  }
  if (any(params["gamma", ] < 0 | params["gamma", ] > 1)) {
    stop("gamma values in fit$params must lie between 0 and 1.", call. = FALSE)
  }
  TRUE
}

#' Validate a person-level latent-class fit
#' @param fit fitted person-level latent-class model
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_person_lca_fit <- function(fit) {
  if (!inherits(fit, "guess_person_fit")) {
    stop("fit must be a guess_person_fit object.", call. = FALSE)
  }
  if (!is.data.frame(fit$posterior) ||
        !identical(names(fit$posterior), paste0("P_", PERSON_CLASS_NAMES))) {
    stop(
      "fit$posterior must be a data frame with columns P_gg, P_gk, and P_kk.",
      call. = FALSE
    )
  }
  posterior <- as.matrix(fit$posterior)
  if (!is.numeric(posterior) || any(!is.finite(posterior[!is.na(posterior)])) ||
        any(posterior < 0 | posterior > 1, na.rm = TRUE)) {
    stop("fit$posterior must contain probabilities between 0 and 1.", call. = FALSE)
  }
  partly_missing <- apply(is.na(posterior), 1L, any) &
    !apply(is.na(posterior), 1L, all)
  if (any(partly_missing) ||
        any(abs(rowSums(posterior, na.rm = TRUE) - 1) > sqrt(.Machine$double.eps) &
              !apply(is.na(posterior), 1L, all))) {
    stop(
      "Each observed row of fit$posterior must sum to 1; unobserved rows must be all NA.",
      call. = FALSE
    )
  }
  TRUE
}

#' Evaluate an expression with a temporary random seed
#' @param seed integer seed or NULL
#' @param expr expression to evaluate
#' @return value of expr
#' @keywords internal
with_preserved_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  assert_int(seed, .var.name = "seed")
  rng_name <- ".Random.seed"
  had_seed <- exists(rng_name, envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(rng_name, envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit(
    if (had_seed) {
      assign(rng_name, old_seed, envir = .GlobalEnv)
    } else if (exists(rng_name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = rng_name, envir = .GlobalEnv)
    },
    add = TRUE
  )
  set.seed(seed)
  force(expr)
}
