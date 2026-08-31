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

# =============================================================================
# Core likelihood function
# =============================================================================

# Internal log-likelihood for transition counts.
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
# Item-level scoring
# =============================================================================

#' Score an item-level latent-class fit against transition counts
#'
#' @description Computes log scores and perplexity for each named item and in
#' aggregate. The supplied counts may be the fit data or independent evaluation
#' data; this function does not infer that provenance. Scores are only
#' out-of-sample when the counts were not used to estimate `fit`.
#'
#' @param fit A `guess_fit` object returned by [fit_item_lca()] or
#'   [fit_item_lca_counts()].
#' @param transition_counts Named matrix of nonnegative whole-number transition
#'   counts with exactly the same item names as `fit`.
#'
#' @return A `guess_item_score` object containing `item_scores`, a data frame of
#'   item-level log likelihood, observation count, mean log likelihood, and
#'   perplexity; plus aggregate `total_log_likelihood`, `n_observations`,
#'   `mean_log_likelihood`, and `perplexity`.
#'
#' @references
#' Cor, M. K., and Sood, G. (2016). Guessing and Forgetting: A Latent Class
#' Model for Measuring Learning. *Political Analysis*, 24(2), 226--242.
#'
#' @export
#' @examples
#' sim <- simulate_lca(n = 500, n_items = 2, seed = 123)
#' transition_counts <- count_item_transitions(sim$pre, sim$post)
#' fit <- fit_item_lca_counts(transition_counts)
#' score_item_lca(fit, transition_counts)
score_item_lca <- function(fit, transition_counts) {
  validate_item_lca_fit(fit)
  transition_counts <- prepare_transition_counts(transition_counts)
  if ("aggregate" %in% rownames(transition_counts)) {
    stop(
      "transition_counts must contain item rows only, not an aggregate row.",
      call. = FALSE
    )
  }
  if (!identical(ncol(transition_counts) == 9L, fit$model_type == "dk")) {
    stop(
      "transition_counts must use the cell schema implied by fit$model_type.",
      call. = FALSE
    )
  }
  item_names <- rownames(transition_counts)
  if (!setequal(item_names, colnames(fit$params))) {
    stop(
      "transition_counts must contain the same item names as fit$params.",
      call. = FALSE
    )
  }

  params <- fit$params[, item_names, drop = FALSE]
  n_observations <- rowSums(transition_counts)
  item_log_likelihood <- vapply(item_names, function(item_name) {
    log_likelihood(
      params[, item_name],
      transition_counts[item_name, , drop = TRUE]
    )
  }, numeric(1L))
  mean_log_likelihood <- item_log_likelihood / n_observations
  item_perplexity <- exp(-mean_log_likelihood)
  total_log_likelihood <- sum(item_log_likelihood)
  total_observations <- sum(n_observations)
  total_mean_log_likelihood <- total_log_likelihood / total_observations

  structure(
    list(
      item_scores = data.frame(
        log_likelihood = item_log_likelihood,
        n_observations = n_observations,
        mean_log_likelihood = mean_log_likelihood,
        perplexity = item_perplexity,
        row.names = item_names
      ),
      total_log_likelihood = total_log_likelihood,
      n_observations = total_observations,
      mean_log_likelihood = total_mean_log_likelihood,
      perplexity = exp(-total_mean_log_likelihood),
      call = match.call()
    ),
    class = "guess_item_score"
  )
}

# =============================================================================
# Individual-level functions
# =============================================================================

#' Calculate per-individual log-likelihood details
#'
#' @param fit A `guess_fit` object returned by [fit_item_lca_counts()] or
#'   [fit_item_lca()].
#' @param pre_test data.frame of pre-test responses
#' @param post_test data.frame of post-test responses
#' @param na_as classification of NA responses
#' @param missing_action structural missingness handling
#' @return List with per-individual log likelihoods and observed-pair counts.
#' @keywords internal
individual_likelihood_details <- function(
  fit,
  pre_test,
  post_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  validate_item_lca_fit(fit)
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  item_names <- validate_paired_item_names(pre_test, post_test)
  if (!setequal(item_names, colnames(fit$params))) {
    stop(
      "pre_test and post_test must contain the same item names as fit$params.",
      call. = FALSE
    )
  }
  post_test <- post_test[item_names]
  response_data <- prepare_response_data(
    pre_test, post_test, na_as, missing_action
  )
  pre_test <- response_data$pre
  post_test <- response_data$post

  param_matrix <- fit$params[, item_names, drop = FALSE]
  has_dk <- fit$model_type == "dk"
  if (!has_dk && any(pre_test == "d" | post_test == "d", na.rm = TRUE)) {
    stop("Don't know responses require a DK model.")
  }

  n_ind <- nrow(pre_test)
  n_items <- ncol(pre_test)
  n_obs <- integer(n_ind)
  log_likelihoods <- numeric(n_ind)

  for (i in seq_len(n_ind)) {
    ll <- 0
    for (j in seq_len(n_items)) {
      params <- param_matrix[, j]
      probs <- cell_probs(params)
      cell <- response_to_cell(
        pre_test[i, j], post_test[i, j], has_dk,
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

#' Score an item LCA fit on individual responses
#'
#' Evaluates a fitted item-level latent-class model on paired individual
#' responses. The aggregate likelihood equals [score_item_lca()] applied to
#' transition counts made from the same responses.
#'
#' @param fit A `guess_fit` object returned by [fit_item_lca_counts()] or
#'   [fit_item_lca()].
#' @param pre_test Data frame containing one pre-test item per column.
#' @param post_test Data frame containing the corresponding post-test items.
#' @param ... Must be empty.
#' @param na_as Classification of `NA` responses: `"dk"` treats them as
#'   observed don't-know responses and `"missing"` treats them as structural
#'   missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return A `guess_individual_score` object. `individual_scores` contains one
#'   row per respondent; rows with no observed item pairs have `NA` mean score
#'   and perplexity. Aggregate fields are observation-weighted across all
#'   observed item pairs.
#' @export
score_individual_lca <- function(
  fit,
  pre_test,
  post_test,
  ...,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  details <- individual_likelihood_details(
    fit, pre_test, post_test, na_as, missing_action
  )
  n_observations <- details$n_obs
  mean_log_likelihood <- rep(NA_real_, length(n_observations))
  observed <- n_observations > 0L
  mean_log_likelihood[observed] <-
    details$log_likelihood[observed] / n_observations[observed]
  individual_perplexity <- exp(-mean_log_likelihood)

  total_log_likelihood <- sum(details$log_likelihood)
  total_observations <- sum(n_observations)
  total_mean_log_likelihood <- if (total_observations > 0L) {
    total_log_likelihood / total_observations
  } else {
    NA_real_
  }

  structure(
    list(
      individual_scores = data.frame(
        log_likelihood = details$log_likelihood,
        n_observations = n_observations,
        mean_log_likelihood = mean_log_likelihood,
        perplexity = individual_perplexity,
        row.names = rownames(pre_test)
      ),
      total_log_likelihood = total_log_likelihood,
      n_observations = total_observations,
      mean_log_likelihood = total_mean_log_likelihood,
      perplexity = exp(-total_mean_log_likelihood),
      call = match.call()
    ),
    class = "guess_individual_score"
  )
}

fold_assignment <- function(n, k, seed = NULL) {
  with_preserved_seed(seed, sample(rep(seq_len(k), length.out = n)))
}

#' Cross-validate an item LCA fit over individuals
#'
#' Splits respondents into `k` folds, fits each model on the remaining
#' respondents, and scores the held-out respondents. Each fold must fit and
#' converge; a failed fold stops with its fold number rather than silently
#' changing the estimand by omitting its held-out responses.
#'
#' @param pre_test Data frame containing one pre-test item per column.
#' @param post_test Data frame containing the corresponding post-test items.
#' @param ... Must be empty.
#' @param k Number of folds; an integer from 2 through the number of rows.
#' @param seed Optional integer seed used only to assign folds. It does not
#'   alter the caller's random-number-generator state.
#' @param start Optional named feasible starting vector passed to
#'   [fit_item_lca_counts()].
#' @param control List passed to the `control` argument of
#'   [fit_item_lca_counts()].
#' @param na_as Classification of `NA` responses: `"dk"` treats them as
#'   observed don't-know responses and `"missing"` treats them as structural
#'   missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return A `guess_cv` object. `fold_results` records every fold and `fold_id`
#'   maps each input row to its held-out fold. Aggregate likelihood and
#'   perplexity are weighted by held-out observed item pairs. No standard error
#'   is reported because fold scores are dependent training-set refits, not
#'   independent replicate estimates.
#' @export
cv_individual_lca <- function(
  pre_test,
  post_test,
  ...,
  k = 5L,
  seed = NULL,
  start = NULL,
  control = list(),
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  item_names <- validate_paired_item_names(pre_test, post_test)
  post_test <- post_test[item_names]
  assert_int(k, lower = 2L)
  if (!is.null(seed)) assert_int(seed)
  if (!is.list(control)) stop("control must be a list.", call. = FALSE)

  n_ind <- nrow(pre_test)

  if (n_ind < k) {
    stop("n_individuals (", n_ind, ") must be >= k (", k, ")")
  }

  fold_ids <- fold_assignment(n_ind, k, seed)

  schema_counts <- count_item_transitions(
    pre_test, post_test,
    na_as = na_as, missing_action = missing_action
  )
  response_schema <- if (ncol(schema_counts) == 9L) "dk" else "binary"

  results <- vector("list", k)

  for (fold in seq_len(k)) {
    test_idx <- which(fold_ids == fold)
    train_idx <- which(fold_ids != fold)

    fit <- tryCatch({
      tm <- count_item_transitions_impl(
        pre_test[train_idx, , drop = FALSE],
        post_test[train_idx, , drop = FALSE],
        response_schema = response_schema,
        na_as = na_as,
        missing_action = missing_action
      )
      fit_item_lca_counts(tm, start = start, control = control)
    }, error = function(error) {
      stop(
        "Fold ", fold, " failed: ", conditionMessage(error),
        call. = FALSE
      )
    })

    train_details <- individual_likelihood_details(
      fit,
      pre_test[train_idx, , drop = FALSE],
      post_test[train_idx, , drop = FALSE],
      na_as, missing_action
    )
    train_ll <- sum(train_details$log_likelihood)

    test_details <- individual_likelihood_details(
      fit,
      pre_test[test_idx, , drop = FALSE],
      post_test[test_idx, , drop = FALSE],
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
  total_ll <- sum(fold_results$test_ll)
  total_obs <- sum(fold_results$test_obs)
  mean_ll <- if (total_obs > 0L) total_ll / total_obs else NA_real_
  perplexity <- if (!is.na(mean_ll)) exp(-mean_ll) else NA_real_

  new_guess_cv(
    fold_results = fold_results,
    mean_ll = mean_ll,
    total_ll = total_ll,
    perplexity = perplexity,
    se = NA_real_,
    cv_type = "individuals",
    k = k,
    fold_id = fold_ids,
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
#' @param fit output from fit_person_lca()
#' @return data.frame with columns P_gg, P_gk, P_kk (rows = individuals)
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' fit <- fit_person_lca(sim$pre, sim$post)
#' posteriors <- posterior_class_probs(fit)
#' head(posteriors)
posterior_class_probs <- function(fit) {
  validate_person_lca_fit(fit)
  fit$posterior
}

#' Compute posterior probability of learning
#'
#' Returns P(gk | data) for each individual, representing the probability
#' that the individual truly learned (vs. guessing or already knowing).
#'
#' @param fit output from fit_person_lca()
#' @return numeric vector of P(learned | data) for each individual
#' @export
#' @examples
#' sim <- simulate_lca(n = 100, gk = 0.30, seed = 123, return_classes = TRUE)
#' fit <- fit_person_lca(sim$pre, sim$post)
#' p_learned <- posterior_learned(fit)
#' cor(p_learned, sim$learned)
posterior_learned <- function(fit) {
  posterior_class_probs(fit)$P_gk
}

# =============================================================================
# Cross-sectional baseline functions
# =============================================================================

#' Estimate a Cross-Sectional Logit Score
#'
#' Computes the empirical logit of each person's proportion correct. The
#' denominator-aware half-count correction keeps all-observed scores finite.
#' This is a descriptive score, not a fitted item-response model.
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
#'   functions. The empirical-logit correction is from Cox and Snell (1989,
#'   Section 2.1.6); respondents with no observed responses receive `NA`.
#'
#' @references
#' Cox, D. R., and Snell, E. J. (1989). *Analysis of Binary Data* (2nd ed.).
#' Chapman and Hall.
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
  n_observed <- rowSums(!is.na(responses))
  n_correct <- rowSums(responses == 1, na.rm = TRUE)
  p_correct <- (n_correct + 0.5) / (n_observed + 1)
  p_correct[n_observed == 0L] <- NA_real_
  qlogis(p_correct)
}

#' Cross-sectional learning estimate
#'
#' Estimates learning as the difference in logit scores between post and pre.
#' This ignores the transition structure that the LCA model uses.
#'
#' @param pre_test data.frame of pre-test responses
#' @param post_test data.frame of post-test responses
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
  post_test,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  item_names <- validate_paired_item_names(pre_test, post_test)
  post_test <- post_test[item_names]
  response_data <- prepare_response_data(
    pre_test, post_test, na_as, missing_action
  )
  pre_test <- response_data$pre
  post_test <- response_data$post

  if (response_data$na_as == "missing") {
    for (j in seq_len(ncol(pre_test))) {
      complete <- !is.na(pre_test[[j]]) & !is.na(post_test[[j]])
      pre_test[[j]][!complete] <- NA_character_
      post_test[[j]][!complete] <- NA_character_
    }
  }

  score_pre <- estimate_logit_score(
    pre_test,
    na_as = "missing", missing_action = "omit"
  )
  score_post <- estimate_logit_score(
    post_test,
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
#' @param post_test data.frame of post-test responses
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
  post_test,
  scale = 1,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error")
) {
  assert_numeric(
    scale,
    len = 1L,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "scale"
  )
  if (scale == 0) {
    stop("scale must be nonzero.", call. = FALSE)
  }
  learning <- cross_sectional_learning(
    pre_test, post_test,
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
#' @param true_params Named numeric vector of strictly interior true parameters.
#'   For no-DK model: c(gg=, gk=, kk=, gamma=)
#'   For DK model: c(gg=, gk=, gd=, kk=, dg=, dk=, dd=, gamma=)
#' @param n Integer. Sample size per simulation. Default 500.
#' @param n_items Integer. Number of items. Default 2.
#' @param n_sims Integer. Number of Monte Carlo simulations. Default 100.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return Data frame with one row per parameter containing columns:
#'   parameter (name), true_value, mean_estimate, bias (mean estimate minus true),
#'   rmse (root mean squared error), estimate_sd (Monte Carlo standard deviation
#'   of estimates), and mcse (Monte Carlo standard error of the mean estimate).
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
  assert_numeric(
    true_params,
    min.len = 4L,
    max.len = 8L,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "true_params"
  )
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

  if (is.null(names(true_params)) || !setequal(names(true_params), expected_names)) {
    stop(
      "true_params must be a named vector with: ",
      paste(expected_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  true_params <- true_params[expected_names]
  class_weights <- true_params[setdiff(expected_names, "gamma")]
  if (any(class_weights < 0 | class_weights > 1) ||
        abs(sum(class_weights) - 1) > sqrt(.Machine$double.eps)) {
    stop("Latent-class weights in true_params must be probabilities that sum to 1.", call. = FALSE)
  }
  if (any(class_weights == 0 | class_weights == 1)) {
    stop(
      paste(
        "Latent-class weights in true_params must be strictly between 0 and 1",
        "for recovery validation."
      ),
      call. = FALSE
    )
  }
  if (true_params[["gamma"]] <= 0 || true_params[["gamma"]] >= 1) {
    stop(
      "true_params$gamma must be strictly between 0 and 1 for recovery validation.",
      call. = FALSE
    )
  }

  estimates <- with_preserved_seed(seed, {
    output <- matrix(NA_real_, nrow = n_sims, ncol = n_params)
    colnames(output) <- expected_names

    for (sim in seq_len(n_sims)) {
      tryCatch({
        sim_data <- if (is_dk) {
          simulate_lca_dk(
            n = n, n_items = n_items,
            gg = true_params[["gg"]], gk = true_params[["gk"]],
            gd = true_params[["gd"]], kk = true_params[["kk"]],
            dg = true_params[["dg"]], dk = true_params[["dk"]],
            dd = true_params[["dd"]], gamma = true_params[["gamma"]]
          )
        } else {
          simulate_lca(
            n = n, n_items = n_items,
            gg = true_params[["gg"]], gk = true_params[["gk"]],
            kk = true_params[["kk"]], gamma = true_params[["gamma"]]
          )
        }
        fit <- fit_item_lca(sim_data$pre, sim_data$post)
        output[sim, ] <- rowMeans(fit$params)
      }, error = function(error) {
        stop(
          "Simulation ", sim, " failed: ", conditionMessage(error),
          call. = FALSE
        )
      })
    }
    output
  })

  results <- data.frame(
    parameter = expected_names,
    true_value = as.numeric(true_params[expected_names]),
    stringsAsFactors = FALSE
  )

  results$mean_estimate <- colMeans(estimates)
  results$bias <- results$mean_estimate - results$true_value
  true_matrix <- matrix(
    results$true_value,
    nrow = n_sims,
    ncol = n_params,
    byrow = TRUE
  )
  results$rmse <- sqrt(
    colMeans((estimates - true_matrix)^2)
  )
  results$estimate_sd <- apply(estimates, 2, stats::sd)
  results$mcse <- results$estimate_sd / sqrt(n_sims)

  results
}
