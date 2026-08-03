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
#' @param epsilon boundary used to keep probabilities in the open interval
#' @return updated class proportions and guessing probabilities
#' @keywords internal
person_item_maximization <- function(
  pre,
  post,
  complete,
  expected,
  gamma,
  epsilon
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
    }
  }

  class_priors <- pmax(class_priors, epsilon)
  class_priors <- class_priors / sum(class_priors)
  updated_gamma <- pmin(pmax(updated_gamma, epsilon), 1 - epsilon)
  list(class_priors = class_priors, gamma = updated_gamma)
}

#' Fit a Joint Person-Level Latent Class Model
#'
#' Fits one latent transition class per person across repeated items. Class
#' proportions are shared across items, while guessing probabilities are
#' item-specific. Parameters are estimated jointly by expectation-maximization.
#'
#' This is distinct from \code{\link{item_lca_fit}}, which fits independent
#' class proportions for each item.
#'
#' @param pre_test data frame of pre-test responses
#' @param pst_test data frame of post-test responses
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @param item_fit optional item-wise fit used to initialize the EM algorithm
#' @param max_iter maximum EM iterations
#' @param tol convergence tolerance
#' @return An object of class \code{guess_person_fit} containing shared class
#'   proportions, item-specific guessing probabilities, person-level posterior
#'   probabilities, log-likelihood, and convergence information.
#' @export
#' @examples
#' sim <- simulate_lca(n = 500, n_items = 4, seed = 123)
#' fit <- person_item_lca_fit(sim$pre, sim$post)
#' fit$class_priors
#' head(fit$posterior)
person_item_lca_fit <- function(
  pre_test,
  pst_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error"),
  item_fit = NULL,
  max_iter = 1000L,
  tol = 1e-8
) {
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(pst_test, "pst_test")
  validate_compatible_dataframes(pre_test, pst_test)
  if (anyDuplicated(names(pre_test))) {
    stop("Item names must be unique.")
  }
  assert_int(max_iter, lower = 1L)
  assert_numeric(tol, lower = 0, len = 1L)

  response_data <- prepare_response_data(
    pre_test, pst_test, na_as, missing_action
  )
  pre_test <- response_data$pre
  pst_test <- response_data$post
  if (any(pre_test == "d" | pst_test == "d", na.rm = TRUE)) {
    stop("person_item_lca_fit() currently supports only the no-DK model.")
  }

  pre <- matrix(
    as.numeric(as.matrix(pre_test)),
    nrow = nrow(pre_test),
    dimnames = dimnames(pre_test)
  )
  post <- matrix(
    as.numeric(as.matrix(pst_test)),
    nrow = nrow(pst_test),
    dimnames = dimnames(pst_test)
  )
  complete <- !is.na(pre) & !is.na(post)
  observed_person <- rowSums(complete) > 0L
  if (!any(observed_person)) {
    stop("No complete pre/post response pairs are available.")
  }

  if (is.null(item_fit)) {
    item_fit <- item_lca_fit(
      pre_test, pst_test,
      na_as = "missing", missing_action = "omit"
    )
  }
  if (!inherits(item_fit, "guess_fit") || nrow(item_fit$params) != 4L) {
    stop("item_fit must be a no-DK item_lca_fit() result.")
  }

  n_ind <- nrow(pre)
  n_items <- ncol(pre)
  epsilon <- 1e-8
  class_priors <- rowMeans(
    item_fit$params[PERSON_CLASS_NAMES, , drop = FALSE]
  )
  class_priors <- pmax(class_priors, epsilon)
  class_priors <- class_priors / sum(class_priors)
  gamma_matrix <- item_fit$params["gamma", , drop = FALSE]
  gamma <- as.numeric(gamma_matrix)
  names(gamma) <- colnames(gamma_matrix)
  item_names <- names(pre_test)
  if (is.null(names(gamma)) || !setequal(names(gamma), item_names)) {
    stop("item_fit item names must match the response data.")
  }
  gamma <- gamma[item_names]
  gamma <- pmin(pmax(gamma, epsilon), 1 - epsilon)
  names(gamma) <- item_names

  converged <- FALSE
  for (iteration in seq_len(max_iter)) {
    expected <- person_item_expectation(
      pre, post, complete, class_priors, gamma
    )
    updated <- person_item_maximization(
      pre, post, complete, expected, gamma, epsilon
    )
    change <- max(
      abs(updated$class_priors - class_priors),
      abs(updated$gamma - gamma)
    )
    class_priors <- updated$class_priors
    gamma <- updated$gamma
    if (change < tol) {
      converged <- TRUE
      break
    }
  }

  expected <- person_item_expectation(
    pre, post, complete, class_priors, gamma
  )
  posterior <- matrix(
    NA_real_,
    nrow = n_ind,
    ncol = length(PERSON_CLASS_NAMES),
    dimnames = list(NULL, paste0("P_", PERSON_CLASS_NAMES))
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
