#' S3 Methods for guess Objects
#'
#' Print, summary, coef, vcov, and confint methods for guess model fits
#' and cross-validation results.

# =============================================================================
# guess_fit class (model fit results)
# =============================================================================

#' Create a guess_fit object
#'
#' @param params parameter matrix (rows = parameters, cols = items)
#' @param learning learning estimates vector
#' @param n_items number of items
#' @param n_obs total observations
#' @param model_type "nodk" or "dk"
#' @param diagnostics optimizer diagnostics by item
#' @param aggregate optional separately fitted aggregate counts
#' @param call original function call
#' @return object of class "guess_fit"
#' @keywords internal
new_guess_fit <- function(params, learning, n_items, n_obs,
                          model_type, diagnostics = NULL,
                          aggregate = NULL, call = NULL) {
  if (is.null(names(learning)) && length(learning) == ncol(params)) {
    names(learning) <- colnames(params)
  }
  structure(
    list(
      params = params,
      learning = learning,
      n_items = n_items,
      n_obs = n_obs,
      model_type = model_type,
      diagnostics = diagnostics,
      aggregate = aggregate,
      call = call
    ),
    class = "guess_fit"
  )
}

#' Print method for guess_fit
#'
#' @param x guess_fit object
#' @param ... ignored
#' @return invisible(x)
#' @export
print.guess_fit <- function(x, ...) {
  cat("LCA Model Fit\n")
  cat(rep("-", 40), "\n", sep = "")
  cat("Items:", x$n_items, " | Observations:", x$n_obs, "\n")
  cat("Model:", if (x$model_type == "dk") "With Don't Know" else "Without Don't Know", "\n\n")

  cat("Learning estimates:\n")
  learning <- as.numeric(x$learning)
  names(learning) <- colnames(x$params)
  print(round(learning, 3))

  cat("\nUse summary() for parameter details, coef() to extract parameters.\n")
  invisible(x)
}

#' Summary method for guess_fit
#'
#' @param object guess_fit object
#' @param ... ignored
#' @return invisible summary object
#' @export
summary.guess_fit <- function(object, ...) {
  cat("LCA Model Summary\n")
  cat(rep("=", 50), "\n", sep = "")
  cat("Items:", object$n_items, " | Observations:", object$n_obs, "\n")
  cat("Model:", if (object$model_type == "dk") "With Don't Know" else "Without Don't Know", "\n\n")

  cat("Parameter Estimates:\n")
  cat(rep("-", 50), "\n", sep = "")

  params <- object$params
  print(round(params, 4))

  cat("\nLearning Estimates (gk", if (object$model_type == "dk") " + dk" else "", "):\n", sep = "")
  cat(rep("-", 50), "\n", sep = "")
  learning <- as.numeric(object$learning)
  names(learning) <- colnames(params)
  print(round(learning, 4))

  cat("\nMean learning:", round(mean(learning, na.rm = TRUE), 4), "\n")

  invisible(object)
}

#' Print method for item-level goodness-of-fit diagnostics
#'
#' @param x A `guess_gof` object.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.guess_gof <- function(x, ...) {
  cat("Item-level Pearson goodness-of-fit diagnostics\n")
  print(x$statistics)
  cat("\nUse $observed, $expected, and $residuals for cell-level diagnostics.\n")
  invisible(x)
}

#' Extract coefficients from guess_fit
#'
#' @param object guess_fit object
#' @param ... ignored
#' @return parameter matrix
#' @export
coef.guess_fit <- function(object, ...) {
  object$params
}

# =============================================================================
# guess_cv class (cross-validation results)
# =============================================================================

#' Create a guess_cv object
#'
#' @param fold_results data.frame of per-fold results
#' @param mean_ll mean log-likelihood
#' @param total_ll total log-likelihood
#' @param perplexity perplexity
#' @param se standard error of perplexity across folds, if available
#' @param cv_type cross-validation unit
#' @param k number of folds
#' @param fold_id integer vector mapping each input row to its held-out fold
#' @param call original function call
#' @return object of class "guess_cv"
#' @keywords internal
new_guess_cv <- function(fold_results, mean_ll, total_ll, perplexity, se,
                         cv_type, k, fold_id = NULL, call = NULL) {
  structure(
    list(
      fold_results = fold_results,
      mean_ll = mean_ll,
      total_ll = total_ll,
      perplexity = perplexity,
      se = se,
      cv_type = cv_type,
      k = k,
      fold_id = fold_id,
      call = call
    ),
    class = "guess_cv"
  )
}

#' Print method for guess_cv
#'
#' @param x guess_cv object
#' @param ... ignored
#' @return invisible(x)
#' @export
print.guess_cv <- function(x, ...) {
  cat(x$k, "-Fold Cross-Validation (over ", x$cv_type, ")\n", sep = "")
  cat(rep("-", 45), "\n", sep = "")

  cat("\nHeld-out Performance:\n")
  cat("  Perplexity:     ", format(round(x$perplexity, 3), nsmall = 3), "\n", sep = "")
  cat("  Mean LL/obs:    ", format(round(x$mean_ll, 4), nsmall = 4), "\n", sep = "")
  if (!is.na(x$se)) {
    cat("  SE:             ", format(round(x$se, 4), nsmall = 4), "\n", sep = "")
  }

  cat("\nInterpretation: Lower perplexity = better fit.\n")
  cat("Use $fold_results for per-fold details.\n")

  invisible(x)
}

#' Summary method for guess_cv
#'
#' @param object guess_cv object
#' @param ... ignored
#' @return invisible summary
#' @export
summary.guess_cv <- function(object, ...) {
  cat(object$k, "-Fold Cross-Validation Summary (over ", object$cv_type, ")\n", sep = "")
  cat(rep("=", 55), "\n", sep = "")

  cat("\nOverall Results:\n")
  cat("  Perplexity:       ", format(round(object$perplexity, 4), nsmall = 4), "\n", sep = "")
  cat("  Mean LL/obs:      ", format(round(object$mean_ll, 5), nsmall = 5), "\n", sep = "")
  cat("  Total LL:         ", format(round(object$total_ll, 2), nsmall = 2), "\n", sep = "")
  cat("  SE (across folds):", format(round(object$se, 5), nsmall = 5), "\n", sep = "")

  cat("\nPer-Fold Results:\n")
  cat(rep("-", 55), "\n", sep = "")
  print(object$fold_results, row.names = FALSE)

  invisible(object)
}
