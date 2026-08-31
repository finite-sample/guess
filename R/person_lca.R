#' Person/item response probabilities
#' @param gamma item guessing probability
#' @return matrix with response pairs in rows and latent classes in columns
#' @keywords internal
person_item_response_probs <- function(gamma) {
  matrix(
    c(
      (1 - gamma)^2, 0, 0,
      gamma * (1 - gamma), 1 - gamma, 0,
      gamma * (1 - gamma), 0, 0,
      gamma^2, gamma, 1
    ),
    nrow = 4L,
    ncol = 3L,
    byrow = TRUE,
    dimnames = list(TRANSMAT_COLS_NODK, PERSON_CLASS_NAMES)
  )
}

#' Person/item EM expectation step
#' @param pre numeric pre-test matrix
#' @param post numeric post-test matrix
#' @param complete logical matrix of complete response pairs
#' @param class_priors shared class proportions
#' @param gamma item-specific guessing probabilities
#' @return posterior probabilities and observed-data log-likelihood
#' @keywords internal
person_item_expectation <- function(pre, post, complete, class_priors, gamma) {
  observed_person <- rowSums(complete) > 0L
  log_conditional <- matrix(
    0,
    nrow = nrow(pre),
    ncol = length(PERSON_CLASS_NAMES),
    dimnames = list(NULL, PERSON_CLASS_NAMES)
  )

  for (j in seq_len(ncol(pre))) {
    rows <- which(complete[, j])
    pairs <- paste0("x", pre[rows, j], post[rows, j])
    probabilities <- person_item_response_probs(gamma[[j]])
    log_conditional[rows, ] <- log_conditional[rows, ] +
      log(probabilities[pairs, , drop = FALSE])
  }

  log_joint <- sweep(
    log_conditional[observed_person, , drop = FALSE],
    2L,
    log(class_priors),
    "+"
  )
  row_max <- apply(log_joint, 1L, max)
  if (any(!is.finite(row_max))) {
    stop("The person/item model assigns zero probability to an observed pattern.")
  }
  weights <- exp(log_joint - row_max)
  normalizers <- rowSums(weights)
  posterior <- sweep(weights, 1L, normalizers, "/")

  list(
    posterior = posterior,
    observed_person = observed_person,
    log_likelihood = sum(row_max + log(normalizers))
  )
}

#' Person/item EM maximization step
#' @param pre numeric pre-test matrix
#' @param post numeric post-test matrix
#' @param complete logical matrix of complete response pairs
#' @param expected output from person_item_expectation
#' @param gamma current item guessing probabilities
#' @return updated class proportions and guessing probabilities
#' @keywords internal
person_item_maximization <- function(
  pre,
  post,
  complete,
  expected,
  gamma
) {
  posterior_all <- matrix(
    NA_real_,
    nrow = nrow(pre),
    ncol = length(PERSON_CLASS_NAMES),
    dimnames = list(NULL, PERSON_CLASS_NAMES)
  )
  posterior_all[expected$observed_person, ] <- expected$posterior
  class_priors <- colMeans(expected$posterior)
  updated_gamma <- gamma

  for (j in seq_len(ncol(pre))) {
    rows <- which(complete[, j])
    z <- posterior_all[rows, , drop = FALSE]
    numerator <- sum(
      z[, "gg"] * (pre[rows, j] + post[rows, j]) +
        z[, "gk"] * pre[rows, j]
    )
    denominator <- sum(2 * z[, "gg"] + z[, "gk"])
    if (denominator > 0) {
      updated_gamma[[j]] <- numerator / denominator
      if (updated_gamma[[j]] < 0 || updated_gamma[[j]] > 1) {
        stop("The person-level EM update produced an invalid guessing probability.")
      }
    }
  }

  list(class_priors = class_priors, gamma = updated_gamma)
}

#' Validate a joint person-level LCA starting point
#'
#' @param start named list of class priors and item guessing probabilities
#' @param item_names response item names
#' @return validated, canonically ordered starting point
#' @keywords internal
validate_person_lca_start <- function(start, item_names) {
  if (!is.list(start) || !setequal(names(start), c("class_priors", "gamma"))) {
    stop("start must be a list containing `class_priors` and `gamma`.", call. = FALSE)
  }
  class_priors <- start$class_priors
  gamma <- start$gamma
  if (!is.numeric(class_priors) ||
        length(class_priors) != length(PERSON_CLASS_NAMES) ||
        !setequal(names(class_priors), PERSON_CLASS_NAMES) ||
        any(!is.finite(class_priors)) ||
        any(class_priors < 0 | class_priors > 1)) {
    stop(
      "start$class_priors must be a finite named probability vector for gg, gk, and kk.",
      call. = FALSE
    )
  }
  class_priors <- class_priors[PERSON_CLASS_NAMES]
  if (abs(sum(class_priors) - 1) > sqrt(.Machine$double.eps)) {
    stop("start$class_priors must sum to 1.", call. = FALSE)
  }
  if (!is.numeric(gamma) ||
        length(gamma) != length(item_names) ||
        !setequal(names(gamma), item_names) ||
        any(!is.finite(gamma)) ||
        any(gamma < 0 | gamma > 1)) {
    stop(
      "start$gamma must be a finite named probability vector for every item.",
      call. = FALSE
    )
  }
  list(class_priors = class_priors, gamma = gamma[item_names])
}

#' Make a joint person-level LCA starting point
#'
#' @param pre_test normalized binary pre-test responses
#' @param post_test normalized binary post-test responses
#' @return named list of class priors and item guessing probabilities
#' @keywords internal
make_person_lca_start <- function(pre_test, post_test) {
  item_fit <- fit_item_lca(
    pre_test,
    post_test,
    na_as = "missing",
    missing_action = "omit"
  )
  gamma <- item_fit$params["gamma", , drop = TRUE]
  names(gamma) <- colnames(item_fit$params)
  list(
    class_priors = rowMeans(
      item_fit$params[PERSON_CLASS_NAMES, , drop = FALSE]
    ),
    gamma = gamma
  )
}

#' Fit a joint person-level latent-class model
#'
#' Fits one latent transition class per person across repeated items. Class
#' proportions are shared across items, while guessing probabilities are
#' item-specific. Parameters are estimated jointly by expectation-maximization.
#' This is a package-specific joint extension of the item-level response model,
#' not the item-wise estimator developed by Cor and Sood (2016).
#'
#' The model accepts binary responses and structural missingness only. It assumes
#' each person has one `gg`, `gk`, or `kk` trajectory across all items and that
#' observed item pairs are conditionally independent given that trajectory.
#'
#' @param pre_test Data frame containing one binary pre-test item per column.
#' @param post_test Data frame containing the corresponding binary post-test
#'   items. Items are paired by name, not position.
#' @param ... Must be empty. Its presence requires optional arguments to be
#'   named.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#' @param start Optional named list with `class_priors` (named `gg`, `gk`, `kk`)
#'   and item-named `gamma`. When `NULL`, [fit_item_lca()] supplies a
#'   deterministic item-level initialization.
#' @param max_iterations Maximum EM iterations before an error is raised.
#' @param tolerance Strictly positive finite convergence tolerance for the
#'   maximum absolute parameter change.
#' @return An object of class `guess_person_fit` containing shared class
#'   proportions, item-specific guessing probabilities, person-level posterior
#'   probabilities, log-likelihood, and convergence information. `n_obs` is the
#'   number of observed item-response pairs.
#'
#' @references
#' Cor, M. K., and Sood, G. (2016). Guessing and Forgetting: A Latent Class
#' Model for Measuring Learning. *Political Analysis*, 24(2), 226--242.
#'
#' Dempster, A. P., Laird, N. M., and Rubin, D. B. (1977). Maximum Likelihood
#' from Incomplete Data via the EM Algorithm. *Journal of the Royal Statistical
#' Society: Series B*, 39(1), 1--38.
#'
#' @export
#' @examples
#' sim <- simulate_lca(n = 500, n_items = 4, seed = 123)
#' fit <- fit_person_lca(sim$pre, sim$post)
#' fit$class_priors
#' head(fit$posterior)
fit_person_lca <- function(
  pre_test,
  post_test,
  ...,
  missing_action = c("omit", "error"),
  start = NULL,
  max_iterations = 1000L,
  tolerance = sqrt(.Machine$double.eps)
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L, .var.name = "pre_test")
  assert_data_frame(
    post_test,
    nrows = nrow(pre_test),
    min.cols = 1L,
    .var.name = "post_test"
  )
  respondent_ids <- rownames(pre_test)
  item_names <- validate_paired_item_names(pre_test, post_test)
  post_test <- post_test[item_names]
  assert_int(max_iterations, lower = 1L, .var.name = "max_iterations")
  assert_numeric(
    tolerance,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    len = 1L,
    .var.name = "tolerance"
  )
  if (tolerance == 0) {
    stop("tolerance must be strictly positive.", call. = FALSE)
  }

  response_data <- prepare_response_data(
    pre_test, post_test, na_as = "missing", missing_action = missing_action
  )
  pre_test <- response_data$pre
  post_test <- response_data$post
  if (any(pre_test == "d" | post_test == "d", na.rm = TRUE)) {
    stop("fit_person_lca() supports binary responses only.", call. = FALSE)
  }

  pre <- matrix(
    as.numeric(as.matrix(pre_test)),
    nrow = nrow(pre_test),
    dimnames = dimnames(pre_test)
  )
  post <- matrix(
    as.numeric(as.matrix(post_test)),
    nrow = nrow(post_test),
    dimnames = dimnames(post_test)
  )
  complete <- !is.na(pre) & !is.na(post)
  observed_person <- rowSums(complete) > 0L
  if (!any(observed_person)) {
    stop("No complete pre/post response pairs are available.", call. = FALSE)
  }

  n_ind <- nrow(pre)
  n_items <- ncol(pre)
  if (is.null(start)) {
    start <- make_person_lca_start(pre_test, post_test)
  }
  start <- validate_person_lca_start(start, item_names)
  class_priors <- start$class_priors
  gamma <- start$gamma

  converged <- FALSE
  for (iteration in seq_len(max_iterations)) {
    expected <- person_item_expectation(
      pre, post, complete, class_priors, gamma
    )
    updated <- person_item_maximization(
      pre, post, complete, expected, gamma
    )
    change <- max(
      abs(updated$class_priors - class_priors),
      abs(updated$gamma - gamma)
    )
    class_priors <- updated$class_priors
    gamma <- updated$gamma
    if (change < tolerance) {
      converged <- TRUE
      break
    }
  }
  if (!converged) {
    stop(
      "fit_person_lca() did not converge within ", max_iterations,
      " iterations; last maximum parameter change was ", format(change, digits = 6),
      ".",
      call. = FALSE
    )
  }

  expected <- person_item_expectation(
    pre, post, complete, class_priors, gamma
  )
  posterior <- matrix(
    NA_real_,
    nrow = n_ind,
    ncol = length(PERSON_CLASS_NAMES),
    dimnames = list(respondent_ids, paste0("P_", PERSON_CLASS_NAMES))
  )
  posterior[expected$observed_person, ] <- expected$posterior
  posterior <- as.data.frame(posterior)
  names(class_priors) <- PERSON_CLASS_NAMES

  structure(
    list(
      class_priors = class_priors,
      gamma = gamma,
      posterior = posterior,
      log_likelihood = expected$log_likelihood,
      n_items = n_items,
      n_obs = sum(complete),
      iterations = iteration,
      converged = converged,
      call = match.call()
    ),
    class = "guess_person_fit"
  )
}

#' @export
print.guess_person_fit <- function(x, ...) {
  cat("Joint Person-Level LCA Fit\n")
  cat(rep("-", 40), "\n", sep = "")
  cat(
    "Items:", x$n_items,
    "| Observed pairs:", x$n_obs,
    "| Converged:", x$converged, "\n\n"
  )
  cat("Shared class proportions:\n")
  print(round(x$class_priors, 4))
  cat("\nItem guessing probabilities:\n")
  print(round(x$gamma, 4))
  invisible(x)
}

#' @export
coef.guess_person_fit <- function(object, ...) {
  gamma <- object$gamma
  names(gamma) <- paste0("gamma_", names(gamma))
  c(object$class_priors, gamma)
}
