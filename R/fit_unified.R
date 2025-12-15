#' Unified Goodness of Fit Statistics
#'
#' @title Goodness of fit statistics for transition matrix data
#' 
#' @description Chi-square goodness of fit between true and model based multivariate distribution.
#' Handles both data with and without don't know responses automatically.
#'
#' @param pre_test data.frame carrying pre_test items
#' @param pst_test data.frame carrying pst_test items 
#' @param g estimates of gamma produced from \code{\link{lca_cor}}
#' @param est.param estimated parameters produced from \code{\link{lca_cor}}
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
#' fit_stats <- fit_model(pre_test, pst_test, res$param.lca[nrow(res$param.lca), ], 
#'                        res$param.lca[-nrow(res$param.lca), ])
#' }

fit_model <- function(pre_test, pst_test, g, est.param, force9 = FALSE) {
  
  # Input validation
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(pst_test, "pst_test")
  validate_compatible_dataframes(pre_test, pst_test)
  
  validate_required(g = g, est.param = est.param)
  
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
    params_i <- if (is.matrix(est.param)) est.param[, i] else est.param
    
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
    
    # Perform chi-square test
    observed_probs <- data[i, ] / total_obs
    chi_test <- suppressWarnings(
      chisq.test(expected, p = observed_probs)
    )
    
    # Store results
    fit_results[1, i] <- round(chi_test$statistic, 3)
    fit_results[2, i] <- round(chi_test$p.value, 3)
  }
  
  fit_results
}

# Maintain backward compatibility with existing function names
#' @rdname fit_model
#' @export
fit_dk <- function(pre_test, pst_test, g, est.param, force9 = FALSE) {
  fit_model(pre_test, pst_test, g, est.param, force9 = force9)
}

#' @rdname fit_model  
#' @export
fit_nodk <- function(pre_test, pst_test, g, est.param) {
  fit_model(pre_test, pst_test, g, est.param, force9 = FALSE)
}
