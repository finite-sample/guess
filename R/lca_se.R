#' Bootstrapped Standard Errors
#'
#' @title Bootstrapped standard errors of effect size estimates
#'
#' @param pre_test data.frame carrying pre_test items
#' @param post_test data.frame carrying post-test items
#' @param n_resamples  number of resamples, default is 100
#' @param seed Optional integer seed. When `NULL`, resampling uses the current
#'   random-number-generator state.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return  list with:
#'   \item{parameter_standard_error}{standard errors of parameters by item}
#'   \item{mean_learning}{mean learning estimates}
#'   \item{learning_standard_error}{standard error of learning by item}
#'
#' @export
#'
#' @examples
#' pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0))
#' post_test <- data.frame(
#'   item1 = pre_test[, 1] + c(0, 1, 1, 0, 0),
#'   item2 = pre_test[, 2] + c(0, 1, 0, 0, 1)
#' )
#' \dontrun{
#' lca_se(pre_test, post_test, n_resamples = 10, seed = 123)
#' }
lca_se <- function(pre_test = NULL, post_test = NULL, n_resamples = 100,
                   seed = NULL,
                   na_as = c("dk", "missing"),
                   missing_action = c("omit", "error")) {
  validate_dataframe(pre_test, "pre_test")
  validate_dataframe(post_test, "post_test")
  item_names <- validate_paired_item_names(pre_test, post_test)
  post_test <- post_test[item_names]
  assert_int(n_resamples, lower = 2L, .var.name = "n_resamples")
  n_items <- ncol(pre_test)
  transmatrix <- count_item_transitions(
    pre_test, post_test,
    na_as = na_as, missing_action = missing_action
  )
  bootstrap_schema <- if (ncol(transmatrix) == 9L) "dk" else "binary"
  n_params <- ifelse(ncol(transmatrix) == 4, 4, 8)

  resamp_results <- list()
  resamp_effects <- matrix(ncol = n_items + 1, nrow = n_resamples)
  parameter_standard_error <- matrix(ncol = n_items, nrow = n_params)
  learning_standard_error <- matrix(ncol = n_items + 1, nrow = 1)
  mean_learning <- matrix(ncol = n_items + 1, nrow = 1)
  resamp_params <- rep(list(matrix(nrow = n_resamples, ncol = n_params)), n_items)

  resamples <- with_preserved_seed(seed, lapply(seq_len(n_resamples), function(i) {
    sample(seq_len(nrow(pre_test)), replace = TRUE)
  }))

  for (i in seq_along(resamples)) {
    if (getOption("guess.verbose", FALSE)) {
      if (i %% 10 == 0) message("Bootstrap iteration: ", i, "/", length(resamples))
    }
    resample_rows <- resamples[[i]]
    transmatrix_i <- count_item_transitions_impl(
      pre_test[resample_rows, , drop = FALSE],
      post_test[resample_rows, , drop = FALSE],
      response_schema = bootstrap_schema,
      include_aggregate = TRUE,
      na_as = na_as, missing_action = missing_action
    )
    resamp_results[[i]] <- fit_item_lca_counts(transmatrix_i)
    resamp_effects[i, seq_len(n_items)] <- resamp_results[[i]]$learning
    resamp_effects[i, n_items + 1L] <- resamp_results[[i]]$aggregate$learning

    for (j in seq_len(n_items)) {
      resamp_params[[j]][i, ] <- resamp_results[[i]]$params[, j]
    }
  }

  learning_standard_error[1, ] <- sapply(
    as.data.frame(resamp_effects), stats::sd, na.rm = TRUE
  )
  mean_learning[1, ] <- sapply(as.data.frame(resamp_effects), mean, na.rm = TRUE)

  for (j in seq_len(n_items)) {
    parameter_standard_error[, j] <- sapply(
      as.data.frame(resamp_params[[j]]), stats::sd, na.rm = TRUE
    )
  }

  row.names(mean_learning) <- row.names(learning_standard_error) <- "lca"

  if (nrow(parameter_standard_error) == 8) {
    row.names(parameter_standard_error) <- c(
      "gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma"
    )
  } else {
    row.names(parameter_standard_error) <- c("gg", "gk", "kk", "gamma")
  }

  list(
    parameter_standard_error = parameter_standard_error,
    mean_learning = mean_learning,
    learning_standard_error = learning_standard_error
  )
}
