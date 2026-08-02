#' Bootstrapped Standard Errors
#'
#' @title Bootstrapped standard errors of effect size estimates
#'
#' @param pre_test data.frame carrying pre_test items
#' @param pst_test data.frame carrying pst_test items
#' @param n_resamples  number of resamples, default is 100
#' @param seed    random seed, default is 31415
#' @param force9 Optional. Force 9-column format even if no DK responses.
#'   Default is FALSE.
#' @param na_as Classification of NA responses: `"dk"` (the default) treats
#'   them as observed don't know responses; `"missing"` treats them as
#'   structural missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#'
#' @return  list with:
#'   \item{se_params}{standard errors of parameters by item}
#'   \item{avg_effects}{mean learning estimates}
#'   \item{se_effects}{standard error of learning by item}
#'
#' @export
#'
#' @examples
#' pre_test <- data.frame(pre_item1 = c(1, 0, 0, 1, 0), pre_item2 = c(1, NA, 0, 1, 0))
#' pst_test <- data.frame(
#'   pst_item1 = pre_test[, 1] + c(0, 1, 1, 0, 0),
#'   pst_item2 = pre_test[, 2] + c(0, 1, 0, 0, 1)
#' )
#' \dontrun{
#' lca_se(pre_test, pst_test, n_resamples = 10, seed = 31415)
#' }
lca_se <- function(pre_test = NULL, pst_test = NULL, n_resamples = 100,
                   seed = 31415, force9 = FALSE,
                   na_as = c("dk", "missing"),
                   missing_action = c("omit", "error")) {
  df <- data.frame(cbind(pre_test, pst_test))
  n_items <- ncol(df) / 2
  transmatrix <- multi_transmat(
    pre_test, pst_test,
    force9 = force9,
    na_as = na_as, missing_action = missing_action
  )
  n_params <- ifelse(ncol(transmatrix) == 4, 4, 8)

  resamp_results <- list()
  resamp_effects <- matrix(ncol = n_items + 1, nrow = n_resamples)
  se_params <- matrix(ncol = n_items, nrow = n_params)
  se_effects <- matrix(ncol = n_items + 1, nrow = 1)
  avg_effects <- matrix(ncol = n_items + 1, nrow = 1)
  resamp_params <- rep(list(matrix(nrow = n_resamples, ncol = n_params)), n_items)

  set.seed(seed)
  resamples <- lapply(seq_len(n_resamples), function(i) {
    df[sample(seq_len(nrow(df)), replace = TRUE), ]
  })

  for (i in seq_along(resamples)) {
    if (getOption("guess.verbose", FALSE)) {
      if (i %% 10 == 0) message("Bootstrap iteration: ", i, "/", length(resamples))
    }
    transmatrix_i <- multi_transmat(
      resamples[[i]][, seq_len(n_items)],
      resamples[[i]][, (n_items + 1):(2 * n_items)],
      force9 = force9, agg = TRUE,
      na_as = na_as, missing_action = missing_action
    )
    resamp_results[[i]] <- lca_cor(transmatrix_i)
    resamp_effects[i, ] <- resamp_results[[i]]$learning
    # A `resamp_agg` matrix used to be filled here and never read again. It was
    # allocated 2 * n_items wide while a transition row is 4 wide (or 9 with
    # don't-know responses), so the assignment errored unless those happened to
    # be multiples of each other. That made lca_se() fail outright on 3 items,
    # 5 items, and on every item count tested when the data contained DK
    # responses -- while silently recycling the row at 4 and 8 items. It also
    # read row `n_items`, which is the last item rather than the aggregate row
    # at `n_items + 1`. Since nothing consumed it, it is gone.

    for (j in seq_len(n_items)) {
      resamp_params[[j]][i, ] <- resamp_results[[i]]$params[, j]
    }
  }

  se_effects[1, ] <- sapply(as.data.frame(resamp_effects), sd, na.rm = TRUE)
  avg_effects[1, ] <- sapply(as.data.frame(resamp_effects), mean, na.rm = TRUE)

  for (j in seq_len(n_items)) {
    se_params[, j] <- sapply(as.data.frame(resamp_params[[j]]), sd, na.rm = TRUE)
  }

  row.names(avg_effects) <- row.names(se_effects) <- "lca"

  if (nrow(se_params) == 8) {
    row.names(se_params) <- c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
  } else {
    row.names(se_params) <- c("gg", "gk", "kk", "gamma")
  }

  list(
    se_params = se_params,
    avg_effects = avg_effects,
    se_effects = se_effects
  )
}
