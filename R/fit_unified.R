#' Unified Goodness of Fit Statistics
#'
#' @title Goodness of fit statistics for transition matrix data
#' 
#' @description Pearson chi-square goodness of fit between the observed
#' transition counts and those the fitted model implies. Handles data with and
#' without don't know responses automatically.
#'
#' Degrees of freedom are the free cell probabilities less the parameters
#' estimated from the same counts. The don't-know model leaves 1 degree of
#' freedom. The model without don't know is saturated -- 3 free parameters
#' against 3 free cell probabilities -- so no test is possible and both rows
#' are `NA`.
#'
#' @param pre_test data.frame carrying pre_test items
#' @param pst_test data.frame carrying pst_test items 
#' @param g estimates of gamma produced from \code{\link{lca_cor}}
#' @param est_param estimated parameters produced from \code{\link{lca_cor}}
#' @param force9 Optional. Force 9-column format even if no DK responses. Default is FALSE.
#' @return matrix with two rows: top row carrying chi-square value, bottom row p-values
#' @export
#' @examples
#' \dontrun{
#' # Fit model first
#' transmatrix <- multi_transmat(pre_test, pst_test)
#' res <- lca_cor(transmatrix)
#' 
#' # Calculate goodness of fit
#' fit_stats <- fit_model(pre_test, pst_test, res$params[nrow(res$params), ],
#'                        res$params[-nrow(res$params), ])
#' }

fit_model <- function(pre_test, pst_test, g, est_param, force9 = FALSE) {
  
  # Input validation
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(pst_test, "pst_test")
  validate_compatible_dataframes(pre_test, pst_test)
  
  validate_required(g = g, est_param = est_param)
  
  # Generate transition matrix
  data <- multi_transmat(pre_test, pst_test, force9 = force9)
  
  # Remove aggregate row if present
  if ("agg" %in% rownames(data)) {
    data <- data[rownames(data) != "agg", , drop = FALSE]
  }
  
  # Determine model type
  model_type <- if (ncol(data) == 9) "dk" else "nodk"
  
  # Initialize results matrix
  n_items <- nrow(data)
  fit_results <- matrix(nrow = 2, ncol = n_items)
  colnames(fit_results) <- rownames(data)
  rownames(fit_results) <- c("chi-square", "p-value")
  
  # Calculate fit statistics for each item
  for (i in seq_len(n_items)) {
    
    # Get item-specific gamma and parameters
    gamma_i <- if (is.list(g)) g[[i]] else g[i]
    params_i <- if (is.matrix(est_param)) est_param[, i] else est_param
    
    # Calculate expected values using utility function
    total_obs <- sum(data[i, ])
    expected <- calculate_expected_values(gamma_i, params_i, total_obs, model_type)
    
    # Validate expected values before chi-square test
    if (any(!is.finite(expected)) || any(expected < 0)) {
      # Handle invalid expected values
      fit_results[1, i] <- NA
      fit_results[2, i] <- NA
      next
    }
    
    # Pearson goodness of fit, against the right degrees of freedom.
    #
    # chisq.test(observed, p = expected_probs) charges df = cells - 1, which
    # ignores every parameter estimated from these same counts. The no-DK model
    # has 3 free parameters (gg, gk, kk sum to 1, plus gamma) against 3 free
    # cell probabilities: it is saturated, df = 0, and there is nothing to test.
    # The DK model has 7 free parameters against 8 free cell probabilities,
    # leaving df = 1 -- the single over-identifying restriction x1d/x0d =
    # x10/x00. Charging df = 8 there made the test almost incapable of
    # rejecting.
    observed <- as.numeric(data[i, ])
    expected_counts <- expected / sum(expected) * total_obs

    n_free <- if (model_type == "dk") 7L else 3L
    df <- length(observed) - 1L - n_free

    if (df <= 0L) {
      fit_results[1, i] <- NA
      fit_results[2, i] <- NA
      next
    }

    nonzero <- expected_counts > 0
    if (any(observed[!nonzero] > 0)) {
      stat <- Inf
    } else {
      stat <- sum(
        (observed[nonzero] - expected_counts[nonzero])^2 / expected_counts[nonzero]
      )
    }

    fit_results[1, i] <- round(stat, 3)
    fit_results[2, i] <- round(stats::pchisq(stat, df = df, lower.tail = FALSE), 3)
  }
  
  fit_results
}

# Maintain backward compatibility with existing function names
#' @rdname fit_model
#' @export
fit_dk <- function(pre_test, pst_test, g, est_param, force9 = FALSE) {
  fit_model(pre_test, pst_test, g, est_param, force9 = force9)
}

#' @rdname fit_model  
#' @export
fit_nodk <- function(pre_test, pst_test, g, est_param) {
  fit_model(pre_test, pst_test, g, est_param, force9 = FALSE)
}
