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
    nodk_cell_probs(params[1], params[2], params[3], params[4])
  } else {
    names(params) <- c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
    dk_cell_probs(
      params[1], params[2], params[3], params[4],
      params[5], params[6], params[7], params[8]
    )
  }
}

#' Map pre/post response pairs to cell indices
#'
#' @param pre character vector of pre-test responses ("0", "1", or "d")
#' @param post character vector of post-test responses ("0", "1", or "d")
#' @param has_dk logical; whether model includes don't know
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return integer vector of cell indices
#' @keywords internal
response_to_cell <- function(
  pre,
  post,
  has_dk = FALSE,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  pre <- normalize_responses(pre, na_as, missing_action)
  post <- normalize_responses(post, na_as, missing_action)

  if (!has_dk) {
    if (any(pre == "d" | post == "d", na.rm = TRUE)) {
      stop("Don't know responses require a DK model.")
    }
    cells <- c("00", "01", "10", "11")
  } else {
    cells <- c("00", "01", "0d", "10", "11", "1d", "d0", "d1", "dd")
  }

  match(paste0(pre, post), cells)
}

#' Extract parameter matrix from lca_result
#'
#' @param lca_result output from lca_cor/item_lca_fit or numeric vector
#' @return matrix of parameters (rows = params, cols = items)
#' @keywords internal
extract_params <- function(lca_result) {
  if (inherits(lca_result, "guess_fit")) {
    lca_result$params
  } else if (is.numeric(lca_result)) {
    matrix(lca_result, ncol = 1L)
  } else {
    stop("lca_result must be output from lca_cor()/item_lca_fit() or a numeric vector")
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
    stop(
      "params length (", length(params), ") incompatible with data length (",
      length(data), ")"
    )
  }

  observed <- data > 0
  if (any(probs[observed] <= 0)) {
    return(-Inf)
  }

  sum(data[observed] * log(probs[observed]))
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
      total_ll <- total_ll + ll_i
      total_n <- total_n + sum(transmatrix[i, ])
    }
    ll <- total_ll
    n <- total_n
  }

  if (n == 0L || is.na(ll)) NA_real_ else exp(-ll / n)
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
      Rsolnp::solnp(priors, lik_fn,
        eqfun = eq_fn, eqB = 1,
        LB = rep(0, n_params), UB = rep(1, n_params),
        data = train_data, control = list(trace = 0)
      ),
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
      test_ll <- test_ll + ll_i
      test_n <- test_n + sum(transmatrix[i, ])
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
  finite_rates <- is.finite(fold_results$test_ll_per_obs)
  se <- if (sum(finite_rates) > 1L) {
    stats::sd(fold_results$test_ll_per_obs[finite_rates]) /
      sqrt(sum(finite_rates))
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

#' Fit Independent Item-Wise LCA Models
#'
#' Creates one transition matrix per item and fits independent class
#' proportions and guessing probabilities for every item.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @param ... passed to lca_cor()
#' @return output from lca_cor()
#' @export
item_lca_fit <- function(
  pre_test,
  pst_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error"),
  ...
) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))

  lca_cor(
    multi_transmat(
      pre_test, pst_test,
      na_as = na_as, missing_action = missing_action
    ),
    ...
  )
}

#' Calculate per-individual log-likelihood
#'
#' @param lca_result output from lca_cor() or item_lca_fit()
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return numeric vector of log-likelihoods (length = n individuals)
#' @keywords internal
individual_likelihood_details <- function(
  lca_result,
  pre_test,
  pst_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))
  response_data <- prepare_response_data(
    pre_test, pst_test, na_as, missing_action
  )
  pre_test <- response_data$pre
  pst_test <- response_data$post

  param_matrix <- extract_params(lca_result)
  has_dk <- nrow(param_matrix) == 8L
  if (!has_dk && any(pre_test == "d" | pst_test == "d", na.rm = TRUE)) {
    stop("Don't know responses require a DK model.")
  }

  n_ind <- nrow(pre_test)
  n_items <- ncol(pre_test)
  n_obs <- integer(n_ind)
  log_likelihoods <- numeric(n_ind)

  for (i in seq_len(n_ind)) {
    ll <- 0
    for (j in seq_len(n_items)) {
      params <- param_matrix[, min(j, ncol(param_matrix))]
      probs <- cell_probs(params)
      cell <- response_to_cell(
        pre_test[i, j], pst_test[i, j], has_dk,
        na_as = "missing", missing_action = "omit"
      )

      if (is.na(cell)) next
      n_obs[i] <- n_obs[i] + 1L
      if (probs[cell] <= 0) {
        ll <- -Inf
        break
      }
      ll <- ll + log(probs[cell])
    }
    log_likelihoods[i] <- ll
  }

  list(log_likelihood = log_likelihoods, n_obs = n_obs)
}

#' @rdname individual_likelihood_details
#' @keywords internal
individual_log_likelihood <- function(
  lca_result,
  pre_test,
  pst_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  individual_likelihood_details(
    lca_result, pre_test, pst_test, na_as, missing_action
  )$log_likelihood
}

#' Calculate perplexity from individual-level data
#'
#' @param lca_result output from lca_cor() or item_lca_fit()
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param per_individual logical; return per-individual perplexity?
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return numeric scalar or vector
#' @export
perplexity_individuals <- function(lca_result, pre_test, pst_test,
                                   per_individual = FALSE,
                                   na_as = c("dk", "missing"),
                                   missing_action = c("omit", "error")) {
  assert_flag(per_individual)

  details <- individual_likelihood_details(
    lca_result, pre_test, pst_test, na_as, missing_action
  )
  ind_ll <- details$log_likelihood
  n_obs <- details$n_obs

  if (per_individual) {
    result <- rep(NA_real_, length(ind_ll))
    observed <- n_obs > 0L
    result[observed] <- exp(-ind_ll[observed] / n_obs[observed])
    result
  } else {
    total_obs <- sum(n_obs)
    if (total_obs == 0L) {
      return(NA_real_)
    }
    exp(-sum(ind_ll) / total_obs)
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
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return list with fold_results, mean_ll, total_ll, perplexity, se
#' @export
cv_individuals <- function(pre_test, pst_test, k = 5L, priors = NULL,
                           seed = NULL, na_as = c("dk", "missing"),
                           missing_action = c("omit", "error")) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))
  assert_int(k, lower = 2L)
  if (!is.null(seed)) assert_int(seed)

  n_ind <- nrow(pre_test)

  if (n_ind < k) {
    stop("n_individuals (", n_ind, ") must be >= k (", k, ")")
  }

  if (!is.null(seed)) set.seed(seed)
  fold_ids <- sample(rep(seq_len(k), length.out = n_ind))

  test_tm <- multi_transmat(
    pre_test, pst_test,
    na_as = na_as, missing_action = missing_action
  )
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

    fit <- tryCatch(
      {
        # force9 = is_dk keeps every fold on the schema the full data implies.
        # Without it, a training fold that happens to contain no DK response --
        # which is ordinary when DK is rare, or concentrated in the respondents
        # being held out -- yields a four-cell matrix and a no-DK fit. Scoring
        # that fold then meets a held-out "d" and raises "Don't know responses
        # require a DK model." outside this tryCatch, aborting the whole run.
        tm <- multi_transmat(pre_test[train_idx, , drop = FALSE],
          pst_test[train_idx, , drop = FALSE],
          force9 = is_dk,
          na_as = na_as, missing_action = missing_action
        )
        lca_cor(tm,
          nodk_priors = priors[seq_len(min(4L, length(priors)))],
          dk_priors = priors
        )
      },
      error = function(e) NULL
    )

    if (is.null(fit)) {
      results[[fold]] <- list(
        fold = fold, train_n = length(train_idx), test_n = length(test_idx),
        train_ll = NA_real_, test_ll = NA_real_, test_obs = NA_integer_,
        test_ll_per_obs = NA_real_
      )
      next
    }

    train_details <- individual_likelihood_details(
      fit,
      pre_test[train_idx, , drop = FALSE],
      pst_test[train_idx, , drop = FALSE],
      na_as, missing_action
    )
    train_ll <- sum(train_details$log_likelihood)

    test_details <- individual_likelihood_details(
      fit,
      pre_test[test_idx, , drop = FALSE],
      pst_test[test_idx, , drop = FALSE],
      na_as, missing_action
    )
    test_ll <- sum(test_details$log_likelihood)
    test_obs <- sum(test_details$n_obs)

    results[[fold]] <- list(
      fold = fold, train_n = length(train_idx), test_n = length(test_idx),
      train_ll = train_ll, test_ll = test_ll, test_obs = test_obs,
      test_ll_per_obs = if (test_obs > 0L) test_ll / test_obs else NA_real_
    )
  }

  fold_results <- do.call(rbind.data.frame, results)
  valid <- !is.na(fold_results$test_ll)

  total_ll <- sum(fold_results$test_ll[valid])
  total_obs <- sum(fold_results$test_obs[valid])
  mean_ll <- if (total_obs > 0L) total_ll / total_obs else NA_real_
  perplexity <- if (!is.na(mean_ll)) exp(-mean_ll) else NA_real_
  finite_rates <- is.finite(fold_results$test_ll_per_obs)
  se <- if (sum(finite_rates) > 1L) {
    stats::sd(fold_results$test_ll_per_obs[finite_rates]) /
      sqrt(sum(finite_rates))
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
  pair <- paste0("x", pre, post)
  person_item_response_probs(gamma)[pair, ]
}

#' Compute posterior class probabilities
#'
#' Extracts P(class | response vector) for each individual from an explicitly
#' fitted person/item model.
#'
#' @param object output from person_item_lca_fit()
#' @return data.frame with columns P_gg, P_gk, P_kk (rows = individuals)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' fit <- person_item_lca_fit(sim$pre, sim$post)
#' posteriors <- posterior_class_probs(fit)
#' head(posteriors)
posterior_class_probs <- function(object) {
  if (!inherits(object, "guess_person_fit")) {
    stop("object must be a person_item_lca_fit() result.")
  }
  object$posterior
}

#' Compute posterior probability of learning
#'
#' Returns P(gk | data) for each individual, representing the probability
#' that the individual truly learned (vs. guessing or already knowing).
#'
#' @param object output from person_item_lca_fit()
#' @return numeric vector of P(learned | data) for each individual
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' fit <- person_item_lca_fit(sim$pre, sim$post)
#' p_learned <- posterior_learned(fit)
#' cor(p_learned, sim$learned)
posterior_learned <- function(object) {
  posterior_class_probs(object)$P_gk
}

# =============================================================================
# Cross-sectional baseline functions
# =============================================================================

#' Estimate a Cross-Sectional Logit Score
#'
#' Computes the logit of each person's proportion correct. This is a descriptive
#' score, not a fitted item-response model.
#'
#' @param responses data.frame of binary responses (0/1)
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return numeric vector of logit scores (length = n individuals)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, seed = 123)
#' score_pre <- estimate_logit_score(sim$pre)
#' score_post <- estimate_logit_score(sim$post)
#' @details Observed d/DK responses are scored as incorrect in this binary
#'   correctness baseline. They remain a distinct response category in the LCA
#'   functions.
estimate_logit_score <- function(
  responses,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  assert_data_frame(responses, min.rows = 1L, min.cols = 1L)

  responses <- as.data.frame(
    lapply(
      responses,
      normalize_responses,
      na_as = na_as,
      missing_action = missing_action
    ),
    stringsAsFactors = FALSE
  )
  responses[responses == "d"] <- "0"
  responses <- matrix(
    as.numeric(as.matrix(responses)),
    nrow = nrow(responses),
    dimnames = dimnames(responses)
  )
  p_correct <- rowMeans(responses, na.rm = TRUE)
  p_correct <- pmax(pmin(p_correct, 0.9999), 0.0001)
  qlogis(p_correct)
}

#' Cross-sectional learning estimate
#'
#' Estimates learning as the difference in logit scores between post and pre.
#' This ignores the transition structure that the LCA model uses.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return numeric vector of learning scores (post - pre)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' learning_cs <- cross_sectional_learning(sim$pre, sim$post)
#' cor(learning_cs, sim$learned)
cross_sectional_learning <- function(
  pre_test,
  pst_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  assert_data_frame(pre_test, min.rows = 1L, min.cols = 1L)
  assert_data_frame(pst_test, nrows = nrow(pre_test), ncols = ncol(pre_test))
  response_data <- prepare_response_data(
    pre_test, pst_test, na_as, missing_action
  )
  pre_test <- response_data$pre
  pst_test <- response_data$post

  if (response_data$na_as == "missing") {
    for (j in seq_len(ncol(pre_test))) {
      complete <- !is.na(pre_test[[j]]) & !is.na(pst_test[[j]])
      pre_test[[j]][!complete] <- NA_character_
      pst_test[[j]][!complete] <- NA_character_
    }
  }

  score_pre <- estimate_logit_score(
    pre_test,
    na_as = "missing", missing_action = "omit"
  )
  score_post <- estimate_logit_score(
    pst_test,
    na_as = "missing", missing_action = "omit"
  )

  score_post - score_pre
}

#' Cross-Sectional Learning Score
#'
#' Applies the logistic function to the difference in cross-sectional logit
#' scores. The result is bounded in [0, 1], but is not a calibrated probability
#' of learning and is not an IRT estimate.
#'
#' @param pre_test data.frame of pre-test responses
#' @param pst_test data.frame of post-test responses
#' @param scale numeric scaling factor for the score difference (default 1)
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return numeric vector of learning scores in [0, 1]
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' learning_score <- cross_sectional_learning_score(sim$pre, sim$post)
#' cor(learning_score, sim$learned)
cross_sectional_learning_score <- function(
  pre_test,
  pst_test,
  scale = 1,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  learning <- cross_sectional_learning(
    pre_test, pst_test,
    na_as = na_as, missing_action = missing_action
  )
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
#'   For DK model: c(gg=, gk=, gd=, kk=, dg=, dk=, dd=, gamma=)
#' @param n Integer. Sample size per simulation. Default 500.
#' @param n_items Integer. Number of items. Default 2.
#' @param n_sims Integer. Number of Monte Carlo simulations. Default 100.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return Data frame with one row per parameter containing columns:
#'   parameter (name), true_value, mean_estimate, bias (mean estimate minus true),
#'   rmse (root mean squared error), and se (Monte Carlo standard deviation of
#'   estimates).
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
#'   c(
#'     gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.20,
#'     dg = 0.10, dk = 0.10, dd = 0.10, gamma = 0.25
#'   ),
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
    expected_names <- c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
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
        kk = true_params["kk"], dg = true_params["dg"], dk = true_params["dk"],
        dd = true_params["dd"], gamma = true_params["gamma"]
      )
    } else {
      sim_data <- simulate_lca(
        n = n, n_items = n_items,
        gg = true_params["gg"], gk = true_params["gk"], kk = true_params["kk"],
        gamma = true_params["gamma"]
      )
    }

    tryCatch(
      {
        fit <- item_lca_fit(sim_data$pre, sim_data$post)
        estimates[sim, ] <- rowMeans(fit$params)
      },
      error = function(e) {
        estimates[sim, ] <- rep(NA, n_params)
      }
    )
  }

  results <- data.frame(
    parameter = expected_names,
    true_value = as.numeric(true_params[expected_names]),
    stringsAsFactors = FALSE
  )

  results$mean_estimate <- colMeans(estimates, na.rm = TRUE)
  results$bias <- results$mean_estimate - results$true_value
  true_matrix <- matrix(
    results$true_value,
    nrow = n_sims,
    ncol = n_params,
    byrow = TRUE
  )
  results$rmse <- sqrt(
    colMeans((estimates - true_matrix)^2, na.rm = TRUE)
  )
  results$se <- apply(estimates, 2, sd, na.rm = TRUE)

  results
}
