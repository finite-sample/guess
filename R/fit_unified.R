#' Assess goodness of fit for an item-level latent-class model
#'
#' @description Computes per-item Pearson diagnostics between paired observed
#'   responses and the cell counts implied by a [fit_item_lca()] or
#'   [fit_item_lca_counts()] result. It is an in-sample specification
#'   diagnostic, not a measure of held-out predictive performance. Use
#'   [score_item_lca()] with independent transition counts or
#'   [cv_individual_lca()] for held-out evaluation.
#'
#' The binary model is saturated: its three free cell probabilities are exactly
#' accounted for by its three free parameters, so it has zero degrees of freedom
#' and no p-value. The don't-know model has one remaining over-identifying
#' restriction and therefore one degree of freedom.
#'
#' @param fit A `guess_fit` object returned by [fit_item_lca()] or
#'   [fit_item_lca_counts()].
#' @param pre_test Data frame containing one pre-test item per column.
#' @param post_test Data frame containing the corresponding post-test items.
#' @param ... Must be empty. Its presence requires optional arguments to be
#'   named.
#' @param na_as Classification of `NA` responses: `"dk"` treats them as
#'   observed don't-know responses and `"missing"` treats them as structural
#'   missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return A `guess_gof` object with `statistics`, a data frame containing the
#'   unrounded Pearson statistic, degrees of freedom, p-value, and observation
#'   count for each item; plus `observed`, `expected`, and `residuals` matrices.
#'
#' @references
#' Pearson, K. (1900). On the criterion that a given system of deviations from
#' the probable in the case of a correlated system of variables is such that it
#' can be reasonably supposed to have arisen from random sampling. *Philosophical
#' Magazine*, 50(302), 157--175. doi:10.1080/14786440009463897.
#'
#' Cor, M. K., and Sood, G. (2016). Guessing and Forgetting: A Latent Class
#' Model for Measuring Learning. *Political Analysis*, 24(2), 226--242.
#'
#' @export
#' @examples
#' sim <- simulate_lca_dk(n = 500, n_items = 2, seed = 123)
#' fit <- fit_item_lca(sim$pre, sim$post)
#' assess_item_lca_fit(fit, sim$pre, sim$post)
assess_item_lca_fit <- function(
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
  validate_item_lca_fit(fit)
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  input_items <- validate_paired_item_names(pre_test, post_test)
  fit_items <- colnames(fit$params)
  if (!setequal(input_items, fit_items)) {
    stop(
      "pre_test and post_test must contain the same item names as fit$params.",
      call. = FALSE
    )
  }

  post_test <- post_test[input_items]
  fit_params <- fit$params[, input_items, drop = FALSE]
  response_schema <- if (fit$model_type == "dk") "dk" else "binary"
  observed <- count_item_transitions_impl(
    pre_test,
    post_test,
    response_schema = response_schema,
    na_as = na_as,
    missing_action = missing_action
  )
  expected <- matrix(
    NA_real_,
    nrow = nrow(observed),
    ncol = ncol(observed),
    dimnames = dimnames(observed)
  )
  residuals <- expected
  statistic <- rep(NA_real_, nrow(observed))
  p_value <- rep(NA_real_, nrow(observed))
  n_observations <- rowSums(observed)
  df <- if (fit$model_type == "dk") 1L else 0L

  for (item_index in seq_len(nrow(observed))) {
    item_name <- rownames(observed)[item_index]
    params <- fit_params[, item_name]
    expected_item <- calculate_expected_values(
      gamma_i = params["gamma"],
      params = params[names(params) != "gamma"],
      total_obs = n_observations[item_index],
      model_type = fit$model_type
    )
    names(expected_item) <- colnames(observed)
    expected[item_index, ] <- expected_item

    nonzero_expected <- expected_item > 0
    observed_item <- observed[item_index, ]
    residual_item <- rep(NA_real_, length(observed_item))
    residual_item[nonzero_expected] <- (
      observed_item[nonzero_expected] - expected_item[nonzero_expected]
    ) / sqrt(expected_item[nonzero_expected])
    residual_item[!nonzero_expected & observed_item > 0] <- Inf
    residuals[item_index, ] <- residual_item

    if (df > 0L) {
      if (any(!nonzero_expected & observed_item > 0)) {
        statistic[item_index] <- Inf
        p_value[item_index] <- 0
      } else {
        statistic[item_index] <- sum(residual_item[nonzero_expected]^2)
        p_value[item_index] <- stats::pchisq(
          statistic[item_index],
          df = df,
          lower.tail = FALSE
        )
      }
    }
  }

  structure(
    list(
      statistics = data.frame(
        statistic = statistic,
        df = rep.int(df, nrow(observed)),
        p_value = p_value,
        n_observations = n_observations,
        row.names = rownames(observed)
      ),
      observed = observed,
      expected = expected,
      residuals = residuals,
      call = match.call()
    ),
    class = "guess_gof"
  )
}
