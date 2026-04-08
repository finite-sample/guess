#' Model Criticism Tools
#'
#' Functions for evaluating model fit via held-out likelihood and perplexity.
#' Two parallel sets of functions:
#' - `*_items`: Work on aggregated transition matrices, CV/evaluate over items
#' - `*_individuals`: Work on raw individual data, CV/evaluate over individuals


# =============================================================================
# Internal helpers
# =============================================================================

#' Calculate cell probabilities from parameters
#'
#' @param params numeric vector of length 4 (nodk) or 8 (dk)
#' @return numeric vector of cell probabilities
#' @keywords internal
cell_probs <- function(params) {
  assert_numeric(params, lower = 0, upper = 1, any.missing = FALSE)
  n <- length(params)
  if (!n %in% c(4L, 8L)) {
    stop("params must have length 4 (nodk) or 8 (dk), got ", n)
  }

  if (n == 4L) {
    names(params) <- c("gg", "gk", "kk", "gamma")
  } else {
    names(params) <- c("gg", "gk", "gd", "kg", "kk", "kd", "dd", "gamma")
  }

  g <- params["gamma"]

  if (n == 4L) {
    c(
      (1 - g)^2 * params["gg"],
      (1 - g) * g * params["gg"] + (1 - g) * params["gk"],
      (1 - g) * g * params["gg"],
      g^2 * params["gg"] + g * params["gk"] + params["kk"]
    )
  } else {
    c(
      (1 - g)^2 * params["gg"],
      (1 - g) * g * params["gg"] + (1 - g) * params["gk"],
      (1 - g) * params["gd"],
      (1 - g) * g * params["gg"] + params["kg"],
      g^2 * params["gg"] + g * params["gk"] + g * params["kg"] + params["kk"],
      g * params["gd"] + params["kd"],
      params["kg"],
      g * params["gk"] + params["kd"],
      params["dd"]
    )
  }
}

#' Map pre/post response pairs to cell indices
#'
#' @param pre character vector of pre-test responses ("0", "1", or "d")
#' @param post character vector of post-test responses ("0", "1", or "d")
#' @param has_dk logical; whether model includes don't know
#' @return integer vector of cell indices
#' @keywords internal
response_to_cell <- function(pre, post, has_dk = FALSE) {
  pre <- as.character(pre)
  post <- as.character(post)

  pre[is.na(pre) | pre == "NA"] <- "0"
  post[is.na(post) | post == "NA"] <- "0"

  if (!has_dk) {
    pre[pre == "d"] <- "0"
    post[post == "d"] <- "0"
    cells <- c("00", "01", "10", "11")
  } else {
    cells <- c("00", "01", "0d", "10", "11", "1d", "d0", "d1", "dd")
  }

  match(paste0(pre, post), cells)
}

#' Extract parameter matrix from lca_result
#'
#' @param lca_result output from lca_cor/lca_fit or numeric vector
#' @return matrix of parameters (rows = params, cols = items)
#' @keywords internal
extract_params <- function(lca_result) {
  if (inherits(lca_result, "guess_fit")) {
    lca_result$params
  } else if (is.numeric(lca_result)) {
    matrix(lca_result, ncol = 1L)
  } else {
    stop("lca_result must be output from lca_cor()/lca_fit() or a numeric vector")
  }
}

# =============================================================================
# Core likelihood function
# =============================================================================

#' Calculate log-likelihood for transition data
#'
#' @param params numeric vector of length 4 (nodk) or 8 (dk)
#' @param data numeric vector of transition counts
#' @return scalar log-likelihood
#' @export
#' @examples
#' params <- c(0.4, 0.3, 0.3, 0.25)
#' data <- c(x00 = 10, x01 = 5, x10 = 3, x11 = 12)
#' log_likelihood(params, data)
log_likelihood <- function(params, data) {
  assert_numeric(params, min.len = 4L, max.len = 8L, any.missing = FALSE)
  assert_numeric(data, any.missing = FALSE)

  probs <- cell_probs(params)

  if (length(probs) != length(data)) {
    stop("params length (", length(params), ") incompatible with data length (",
         length(data), ")")
  }

  if (any(probs <= 0)) {
    return(-Inf)
  }

  sum(data * log(probs))
}

# =============================================================================
# Item-level functions
# =============================================================================

#' Calculate perplexity from aggregated item data
#'
#' Lower perplexity indicates better model fit.
#'
#' @param lca_result output from lca_cor() or numeric parameter vector
#' @param transmatrix numeric matrix of transition counts (items x cells)
#' @param item optional integer; specific item index (NULL = aggregate)
#' @return numeric scalar perplexity
#' @export
#' @examples
#' \dontrun{
#' transmatrix <- multi_transmat(pre_test, pst_test)
#' res <- lca_cor(transmatrix)
#' perplexity_items(res, transmatrix)
#' }
perplexity_items <- function(lca_result, transmatrix, item = NULL) {
  assert_matrix(transmatrix, mode = "numeric", min.rows = 1L)

  param_matrix <- extract_params(lca_result)

  if (!is.null(item)) {
    assert_int(item, lower = 1L, upper = ncol(param_matrix))
    ll <- log_likelihood(param_matrix[, item], transmatrix[item, ])
    n <- sum(transmatrix[item, ])
  } else {
    n_items <- nrow(transmatrix)
    total_ll <- 0
    total_n <- 0L

    for (i in seq_len(n_items)) {
      params_i <- if (i <= ncol(param_matrix)) {
        param_matrix[, i]
      } else {
        param_matrix[, ncol(param_matrix)]
      }
      ll_i <- log_likelihood(params_i, transmatrix[i, ])

      if (is.finite(ll_i)) {
        total_ll <- total_ll + ll_i
        total_n <- total_n + sum(transmatrix[i, ])
      }
    }
    ll <- total_ll
    n <- total_n
  }

  if (n == 0L || !is.finite(ll)) NA_real_ else exp(-ll / n)
}

#' K-fold cross-validation over items
#'
#' Splits items into k folds, fits on training items, evaluates on held-out items.
#'
#' @param transmatrix numeric matrix from multi_transmat()
#' @param k integer number of folds
#' @param priors optional numeric vector of starting parameters
#' @param seed optional integer random seed
#' @return list with fold_results, mean_ll, total_ll, perplexity, se
#' @export
cv_items <- function(transmatrix, k = 5L, priors = NULL, seed = NULL) {
  assert_matrix(transmatrix, mode = "numeric", min.rows = 1L)
  assert_int(k, lower = 2L)
  if (!is.null(seed)) assert_int(seed)

  n_items <- nrow(transmatrix)
  n_cols <- ncol(transmatrix)

  if (n_items < k) {
    stop("n_items (", n_items, ") must be >= k (", k, ")")
  }

  if (!is.null(seed)) set.seed(seed)
  fold_ids <- sample(rep(seq_len(k), length.out = n_items))

  is_dk <- n_cols == 9L
  if (is.null(priors)) {
    priors <- if (is_dk) {
      c(0.3, 0.1, 0.2, 0.05, 0.1, 0.1, 0.05, 0.25)
    } else {
      c(0.3, 0.1, 0.1, 0.25)
    }
  }

  lik_fn <- if (is_dk) guessdk_lik else guess_lik
  eq_fn <- if (is_dk) eq1dk else eqn1
  n_params <- length(priors)

  results <- vector("list", k)


  for (fold in seq_len(k)) {
    test_idx <- which(fold_ids == fold)
    train_idx <- which(fold_ids != fold)
    train_data <- colSums(transmatrix[train_idx, , drop = FALSE])

    fit <- tryCatch(
      Rsolnp::solnp(priors, lik_fn, eqfun = eq_fn, eqB = 1,
                    LB = rep(0, n_params), UB = rep(1, n_params),
                    data = train_data, control = list(trace = 0)),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      results[[fold]] <- list(
        fold = fold, train_items = length(train_idx), test_items = length(test_idx),
        train_ll = NA_real_, test_ll = NA_real_, test_n = NA_integer_,
        test_ll_per_obs = NA_real_
      )
      next
    }

    params <- fit$pars
    train_ll <- -fit$values[length(fit$values)]

    test_ll <- 0
    test_n <- 0L
    for (i in test_idx) {
      ll_i <- log_likelihood(params, transmatrix[i, ])
      if (is.finite(ll_i)) {
        test_ll <- test_ll + ll_i
        test_n <- test_n + sum(transmatrix[i, ])
      }
    }

    results[[fold]] <- list(
      fold = fold, train_items = length(train_idx), test_items = length(test_idx),
      train_ll = train_ll, test_ll = test_ll, test_n = test_n,
      test_ll_per_obs = if (test_n > 0L) test_ll / test_n else NA_real_
    )
  }

  fold_results <- do.call(rbind.data.frame, results)
  valid <- !is.na(fold_results$test_ll)

  total_ll <- sum(fold_results$test_ll[valid])
  total_n <- sum(fold_results$test_n[valid])
  mean_ll <- if (total_n > 0L) total_ll / total_n else NA_real_
  perplexity <- if (!is.na(mean_ll)) exp(-mean_ll) else NA_real_
  se <- if (sum(valid) > 1L) {
    stats::sd(fold_results$test_ll_per_obs[valid]) / sqrt(sum(valid))
  } else {
    NA_real_
  }

  new_guess_cv(
    fold_results = fold_results,
    mean_ll = mean_ll,
    total_ll = total_ll,
    perplexity = perplexity,
    se = se,
    cv_type = "items",
    k = k,
    call = match.call()
  )
}

# =============================================================================
# Individual-level functions
# =============================================================================

#' Fit LCA model from individual-level data
#'
#' Convenience wrapper: creates transition matrix and fits model.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param ... passed to lca_cor()
#' @return output from lca_cor()
#' @export
lca_fit <- function(pre_test, pst_test, ...) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))

  lca_cor(multi_transmat(pre_test, pst_test), ...)
}

#' Calculate per-individual log-likelihood
#'
#' @param lca_result output from lca_cor() or lca_fit()
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @return numeric vector of log-likelihoods (length = n individuals)
#' @keywords internal
individual_log_likelihood <- function(lca_result, pre_test, pst_test) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))

  param_matrix <- extract_params(lca_result)
  has_dk <- nrow(param_matrix) == 8L

  n_ind <- nrow(pre_test)
  n_items <- ncol(pre_test)

  vapply(seq_len(n_ind), function(i) {
    ll <- 0
    for (j in seq_len(n_items)) {
      params <- param_matrix[, min(j, ncol(param_matrix))]
      probs <- cell_probs(params)
      cell <- response_to_cell(pre_test[i, j], pst_test[i, j], has_dk)

      if (is.na(cell) || probs[cell] <= 0) return(-Inf)
      ll <- ll + log(probs[cell])
    }
    ll
  }, numeric(1L))
}

#' Calculate perplexity from individual-level data
#'
#' @param lca_result output from lca_cor() or lca_fit()
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param per_individual logical; return per-individual perplexity?
#' @return numeric scalar or vector
#' @export
perplexity_individuals <- function(lca_result, pre_test, pst_test,
                                   per_individual = FALSE) {
  assert_flag(per_individual)

  ind_ll <- individual_log_likelihood(lca_result, pre_test, pst_test)
  n_items <- ncol(pre_test)

  if (per_individual) {
    exp(-ind_ll / n_items)
  } else {
    valid <- is.finite(ind_ll)
    if (!any(valid)) return(NA_real_)
    exp(-sum(ind_ll[valid]) / (sum(valid) * n_items))
  }
}

#' K-fold cross-validation over individuals
#'
#' Splits individuals into k folds, fits on training, evaluates on held-out.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param k integer number of folds
#' @param priors optional numeric vector of starting parameters
#' @param seed optional integer random seed
#' @return list with fold_results, mean_ll, total_ll, perplexity, se
#' @export
cv_individuals <- function(pre_test, pst_test, k = 5L, priors = NULL,
                           seed = NULL) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))
  assert_int(k, lower = 2L)
  if (!is.null(seed)) assert_int(seed)

  n_ind <- nrow(pre_test)
  n_items <- ncol(pre_test)

  if (n_ind < k) {
    stop("n_individuals (", n_ind, ") must be >= k (", k, ")")
  }

  if (!is.null(seed)) set.seed(seed)
  fold_ids <- sample(rep(seq_len(k), length.out = n_ind))

  test_tm <- multi_transmat(pre_test[1L, , drop = FALSE],
                            pst_test[1L, , drop = FALSE])
  is_dk <- ncol(test_tm) == 9L

  if (is.null(priors)) {
    priors <- if (is_dk) {
      c(0.3, 0.1, 0.2, 0.05, 0.1, 0.1, 0.05, 0.25)
    } else {
      c(0.3, 0.1, 0.1, 0.25)
    }
  }

  results <- vector("list", k)

  for (fold in seq_len(k)) {
    test_idx <- which(fold_ids == fold)
    train_idx <- which(fold_ids != fold)

    fit <- tryCatch({
      tm <- multi_transmat(pre_test[train_idx, , drop = FALSE],
                           pst_test[train_idx, , drop = FALSE])
      lca_cor(tm, nodk_priors = priors[seq_len(min(4L, length(priors)))],
              dk_priors = priors)
    }, error = function(e) NULL)

    if (is.null(fit)) {
      results[[fold]] <- list(
        fold = fold, train_n = length(train_idx), test_n = length(test_idx),
        train_ll = NA_real_, test_ll = NA_real_, test_ll_per_obs = NA_real_
      )
      next
    }

    train_ll <- sum(individual_log_likelihood(
      fit, pre_test[train_idx, , drop = FALSE], pst_test[train_idx, , drop = FALSE]
    ))

    test_ll_vec <- individual_log_likelihood(
      fit, pre_test[test_idx, , drop = FALSE], pst_test[test_idx, , drop = FALSE]
    )
    valid_test <- is.finite(test_ll_vec)
    test_ll <- sum(test_ll_vec[valid_test])
    n_valid <- sum(valid_test)

    results[[fold]] <- list(
      fold = fold, train_n = length(train_idx), test_n = length(test_idx),
      train_ll = train_ll, test_ll = test_ll,
      test_ll_per_obs = if (n_valid > 0L) test_ll / (n_valid * n_items) else NA_real_
    )
  }

  fold_results <- do.call(rbind.data.frame, results)
  valid <- !is.na(fold_results$test_ll)

  total_ll <- sum(fold_results$test_ll[valid])
  total_obs <- sum(fold_results$test_n[valid]) * n_items
  mean_ll <- if (total_obs > 0L) total_ll / total_obs else NA_real_
  perplexity <- if (!is.na(mean_ll)) exp(-mean_ll) else NA_real_
  se <- if (sum(valid) > 1L) {
    stats::sd(fold_results$test_ll_per_obs[valid]) / sqrt(sum(valid))
  } else {
    NA_real_
  }

  new_guess_cv(
    fold_results = fold_results,
    mean_ll = mean_ll,
    total_ll = total_ll,
    perplexity = perplexity,
    se = se,
    cv_type = "individuals",
    k = k,
    call = match.call()
  )
}

# =============================================================================
# Posterior class probability functions
# =============================================================================

#' Class-conditional likelihood for single item
#'
#' Returns P(response | class, gamma) for each latent class.
#'
#' @param pre numeric (0 or 1) pre-test response
#' @param post numeric (0 or 1) post-test response
#' @param gamma numeric guessing probability
#' @return named numeric vector of length 3 (P for gg, gk, kk)
#' @keywords internal
class_conditional_item <- function(pre, post, gamma) {
  g <- gamma
  g2 <- g * g
  g1g <- g * (1 - g)
  og2 <- (1 - g)^2

  if (pre == 0 && post == 0) {
    c(gg = og2, gk = 0, kk = 0)
  } else if (pre == 0 && post == 1) {
    c(gg = g1g, gk = 1 - g, kk = 0)
  } else if (pre == 1 && post == 0) {
    c(gg = g1g, gk = 0, kk = 0)
  } else {
    c(gg = g2, gk = g, kk = 1)
  }
}

#' Compute posterior class probabilities
#'
#' Uses Bayes' rule to compute P(class | data) for each individual.
#' The LCA model uses the joint transition structure across all items
#' to separate true learning from lucky guessing.
#'
#' @param lca_result output from lca_cor() or lca_fit()
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @return data.frame with columns P_gg, P_gk, P_kk (rows = individuals)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' fit <- lca_fit(sim$pre, sim$post)
#' posteriors <- posterior_class_probs(fit, sim$pre, sim$post)
#' head(posteriors)
posterior_class_probs <- function(lca_result, pre_test, pst_test) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))

  param_matrix <- extract_params(lca_result)
  if (nrow(param_matrix) != 4L) {
    stop("posterior_class_probs() only supports no-DK model (4 parameters)")
  }

  n_ind <- nrow(pre_test)
  n_items <- ncol(pre_test)

  priors <- c(
    gg = mean(param_matrix["gg", ]),
    gk = mean(param_matrix["gk", ]),
    kk = mean(param_matrix["kk", ])
  )

  posteriors <- matrix(NA_real_, nrow = n_ind, ncol = 3L)
  colnames(posteriors) <- c("P_gg", "P_gk", "P_kk")

  for (i in seq_len(n_ind)) {
    log_lik <- log(priors)

    for (j in seq_len(n_items)) {
      gamma_j <- param_matrix["gamma", min(j, ncol(param_matrix))]
      pre_ij <- as.numeric(pre_test[i, j])
      post_ij <- as.numeric(pst_test[i, j])

      ccl <- class_conditional_item(pre_ij, post_ij, gamma_j)
      ccl[ccl <= 0] <- 1e-100
      log_lik <- log_lik + log(ccl)
    }

    max_ll <- max(log_lik)
    lik <- exp(log_lik - max_ll)
    posteriors[i, ] <- lik / sum(lik)
  }

  as.data.frame(posteriors)
}

#' Compute posterior probability of learning
#'
#' Returns P(gk | data) for each individual, representing the probability
#' that the individual truly learned (vs. guessing or already knowing).
#'
#' @param lca_result output from lca_cor() or lca_fit()
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @return numeric vector of P(learned | data) for each individual
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' fit <- lca_fit(sim$pre, sim$post)
#' p_learned <- posterior_learned(fit, sim$pre, sim$post)
#' cor(p_learned, sim$learned)
posterior_learned <- function(lca_result, pre_test, pst_test) {
  posteriors <- posterior_class_probs(lca_result, pre_test, pst_test)
  posteriors$P_gk
}

# =============================================================================
# Cross-sectional baseline functions
# =============================================================================

#' Estimate ability from single timepoint (cross-sectional)
#'
#' Estimates person ability using simple proportion correct (logit-transformed)
#' or Rasch-style IRT. Ignores the transition structure between time points.
#'
#' @param responses data.frame of binary responses (0/1)
#' @param method character: "logit" (default) or "rasch"
#' @param difficulty numeric vector of item difficulties (for rasch method)
#' @return numeric vector of ability estimates (length = n individuals)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, seed = 123)
#' theta_pre <- estimate_ability(sim$pre)
#' theta_post <- estimate_ability(sim$post)
estimate_ability <- function(responses, method = "logit", difficulty = NULL) {
  assert_data_frame(responses, min.rows = 1L, min.cols = 1L)
  assert_choice(method, c("logit", "rasch"))

  responses <- as.matrix(responses)
  n_ind <- nrow(responses)
  n_items <- ncol(responses)

  if (method == "logit") {
    p_correct <- rowMeans(responses, na.rm = TRUE)
    p_correct <- pmax(pmin(p_correct, 0.9999), 0.0001)
    qlogis(p_correct)
  } else {
    if (is.null(difficulty)) {
      p_item <- colMeans(responses, na.rm = TRUE)
      p_item <- pmax(pmin(p_item, 0.9999), 0.0001)
      difficulty <- -qlogis(p_item)
    }
    assert_numeric(difficulty, len = n_items)

    theta <- numeric(n_ind)
    for (i in seq_len(n_ind)) {
      resp_i <- responses[i, ]
      valid <- !is.na(resp_i)
      if (sum(valid) == 0) {
        theta[i] <- NA_real_
        next
      }

      n_correct <- sum(resp_i[valid])
      total <- sum(valid)

      if (n_correct == 0) {
        theta[i] <- -mean(difficulty[valid]) - 2
      } else if (n_correct == total) {
        theta[i] <- -mean(difficulty[valid]) + 2
      } else {
        theta[i] <- qlogis(n_correct / total) - mean(difficulty[valid])
      }
    }
    theta
  }
}

#' Cross-sectional learning estimate
#'
#' Estimates learning as the difference in ability between post and pre test.
#' This ignores the transition structure that the LCA model uses.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param method character: "logit" (default) or "rasch"
#' @return numeric vector of learning estimates (theta_post - theta_pre)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' learning_cs <- cross_sectional_learning(sim$pre, sim$post)
#' cor(learning_cs, sim$learned)
cross_sectional_learning <- function(pre_test, pst_test, method = "logit") {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))

  theta_pre <- estimate_ability(pre_test, method = method)
  theta_post <- estimate_ability(pst_test, method = method)

  theta_post - theta_pre
}

#' Cross-sectional IRT learning probability
#'
#' Converts the difference in ability estimates to a probability scale
#' using the logistic function. Provides a comparable metric to
#' posterior_learned() but without using the transition structure.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param method character: "logit" (default) or "rasch"
#' @param scale numeric scaling factor for ability difference (default 1)
#' @return numeric vector of learning probabilities in [0, 1]
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' p_learned_cs <- cross_sectional_irt(sim$pre, sim$post)
#' cor(p_learned_cs, sim$learned)
cross_sectional_irt <- function(pre_test, pst_test, method = "logit", scale = 1) {
  learning <- cross_sectional_learning(pre_test, pst_test, method = method)
  plogis(learning * scale)
}


# =============================================================================
# Monte Carlo validation
# =============================================================================

#' Validate Parameter Recovery via Monte Carlo Simulation
#'
#' Performs Monte Carlo simulations to assess parameter recovery of the
#' LCA model. Useful for validating estimator performance.
#'
#' @param true_params Named numeric vector of true parameters.
#'   For no-DK model: c(gg=, gk=, kk=, gamma=)
#'   For DK model: c(gg=, gk=, gd=, kg=, kk=, kd=, dd=, gamma=)
#' @param n Integer. Sample size per simulation. Default 500.
#' @param n_items Integer. Number of items. Default 2.
#' @param n_sims Integer. Number of Monte Carlo simulations. Default 100.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return Data frame with one row per parameter containing columns:
#'   parameter (name), true_value, mean_estimate, bias (mean estimate minus true),
#'   rmse (root mean squared error), se (standard deviation of estimates),
#'   and coverage_95 (proportion of times 95% CI contains true value).
#'
#' @export
#' @examples
#' \dontrun{
#' # Validate no-DK model recovery
#' results <- validate_recovery(
#'   c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
#'   n = 500, n_sims = 50
#' )
#' print(results)
#'
#' # Validate DK model recovery
#' results_dk <- validate_recovery(
#'   c(gg = 0.25, gk = 0.15, gd = 0.10, kg = 0.10,
#'     kk = 0.15, kd = 0.10, dd = 0.15, gamma = 0.25),
#'   n = 500, n_sims = 50
#' )
#' }
validate_recovery <- function(true_params, n = 500, n_items = 2,
                              n_sims = 100, seed = NULL) {

  assert_numeric(true_params, min.len = 4L, max.len = 8L, any.missing = FALSE)
  assert_int(n, lower = 10L)
  assert_int(n_items, lower = 1L)
  assert_int(n_sims, lower = 1L)

  n_params <- length(true_params)
  is_dk <- n_params == 8L

  if (is_dk) {
    expected_names <- c("gg", "gk", "gd", "kg", "kk", "kd", "dd", "gamma")
  } else {
    expected_names <- c("gg", "gk", "kk", "gamma")
  }

  if (is.null(names(true_params))) {
    names(true_params) <- expected_names
  }

  if (!is.null(seed)) set.seed(seed)

  estimates <- matrix(NA, nrow = n_sims, ncol = n_params)
  colnames(estimates) <- expected_names

  for (sim in seq_len(n_sims)) {
    if (is_dk) {
      sim_data <- simulate_lca_dk(
        n = n, n_items = n_items,
        gg = true_params["gg"], gk = true_params["gk"], gd = true_params["gd"],
        kg = true_params["kg"], kk = true_params["kk"], kd = true_params["kd"],
        dd = true_params["dd"], gamma = true_params["gamma"]
      )
    } else {
      sim_data <- simulate_lca(
        n = n, n_items = n_items,
        gg = true_params["gg"], gk = true_params["gk"], kk = true_params["kk"],
        gamma = true_params["gamma"]
      )
    }

    tryCatch({
      fit <- lca_fit(sim_data$pre, sim_data$post)
      estimates[sim, ] <- fit$params[, 1]
    }, error = function(e) {
      estimates[sim, ] <- rep(NA, n_params)
    })
  }

  results <- data.frame(
    parameter = expected_names,
    true_value = as.numeric(true_params[expected_names]),
    stringsAsFactors = FALSE
  )

  results$mean_estimate <- colMeans(estimates, na.rm = TRUE)
  results$bias <- results$mean_estimate - results$true_value
  results$rmse <- sqrt(colMeans((estimates - matrix(results$true_value,
                                                    nrow = n_sims, ncol = n_params,
                                                    byrow = TRUE))^2, na.rm = TRUE))
  results$se <- apply(estimates, 2, sd, na.rm = TRUE)

  se_est <- results$se
  coverage <- numeric(n_params)
  for (j in seq_len(n_params)) {
    ci_lower <- estimates[, j] - 1.96 * se_est[j]
    ci_upper <- estimates[, j] + 1.96 * se_est[j]
    coverage[j] <- mean(results$true_value[j] >= ci_lower &
                          results$true_value[j] <= ci_upper, na.rm = TRUE)
  }
  results$coverage_95 <- coverage

  results
}
