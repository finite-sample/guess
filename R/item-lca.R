#' Fit item-wise latent-class models to transition counts
#'
#' Estimate the constrained latent-class model developed by Cor and Sood
#' (2016) independently for each item. The model is fitted with
#' [Rsolnp::solnp()] and requires its reported convergence code to be zero.
#'
#' @param transition_counts Named matrix of nonnegative whole-number transition
#'   counts returned by [count_item_transitions()].
#' @param ... Must be empty. Its presence requires optional arguments to be
#'   named.
#' @param start Optional named feasible starting vector. When `NULL`, latent
#'   class weights start uniformly and `gamma` starts at its empirical
#'   transition-ratio estimate. A boundary estimate is moved inward by machine
#'   tolerance; an unidentified ratio starts at the admissible midpoint.
#' @param control List passed to the `control` argument of [Rsolnp::solnp()].
#'
#' @return A `guess_fit` object. `diagnostics` records optimizer results by
#'   item. If `transition_counts` contains an `aggregate` row, its fit is stored
#'   separately in `aggregate` and excluded from item metadata.
#'
#' @references
#' Cor, M. K., and Sood, G. (2016). Guessing and Forgetting: A Latent Class
#' Model for Measuring Learning. *Political Analysis*, 24(2), 226--242.
#'
#' Galanos, A., and Ye, Y. (2025). *Rsolnp: General Non-Linear Optimization*.
#' R package version 2.0.1.
#'
#' @export
#' @examples
#' pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0))
#' post_test <- data.frame(item1 = c(1, 0, 1, 1, 0))
#' transition_counts <- count_item_transitions(pre_test, post_test)
#' fit_item_lca_counts(transition_counts)
fit_item_lca_counts <- function(
  transition_counts,
  ...,
  start = NULL,
  control = list()
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  if (!is.list(control)) {
    stop("control must be a list.", call. = FALSE)
  }
  control <- utils::modifyList(list(trace = 0L), control)

  transition_counts <- prepare_transition_counts(transition_counts)
  is_dk <- ncol(transition_counts) == 9L
  parameter_names <- if (is_dk) {
    c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
  } else {
    c("gg", "gk", "kk", "gamma")
  }
  start <- validate_lca_start(start, parameter_names)

  aggregate_counts <- NULL
  if ("aggregate" %in% rownames(transition_counts)) {
    aggregate_counts <- transition_counts["aggregate", , drop = TRUE]
    transition_counts <- transition_counts[
      rownames(transition_counts) != "aggregate",
      ,
      drop = FALSE
    ]
  }
  if (nrow(transition_counts) == 0L) {
    stop("transition_counts must contain at least one non-aggregate item.", call. = FALSE)
  }

  item_fits <- lapply(rownames(transition_counts), function(item_name) {
    fit_lca_count_row(
      transition_counts[item_name, , drop = TRUE],
      item_name,
      is_dk,
      start,
      control
    )
  })
  names(item_fits) <- rownames(transition_counts)

  params <- vapply(item_fits, `[[`, numeric(length(parameter_names)), "params")
  rownames(params) <- parameter_names
  learning <- vapply(item_fits, `[[`, numeric(1L), "learning")
  diagnostics <- do.call(rbind, lapply(item_fits, `[[`, "diagnostics"))

  aggregate_fit <- NULL
  if (!is.null(aggregate_counts)) {
    aggregate_fit <- fit_lca_count_row(
      aggregate_counts,
      "aggregate",
      is_dk,
      start,
      control
    )
  }

  new_guess_fit(
    params = params,
    learning = learning,
    n_items = nrow(transition_counts),
    n_obs = sum(transition_counts),
    model_type = if (is_dk) "dk" else "nodk",
    diagnostics = diagnostics,
    aggregate = aggregate_fit,
    call = match.call()
  )
}

#' Fit item-wise latent-class models to paired responses
#'
#' Creates item transition counts from paired responses, then estimates the
#' constrained latent-class model of Cor and Sood (2016) independently for
#' each item. This is equivalent to calling [count_item_transitions()] followed
#' by [fit_item_lca_counts()].
#'
#' @param pre_test Data frame containing one pre-test item per column.
#' @param post_test Data frame containing the corresponding post-test items.
#' @param ... Must be empty. Its presence requires optional arguments to be
#'   named.
#' @param subgroup Optional logical vector selecting observations to fit.
#' @param include_aggregate Whether to fit the item-total transition counts and
#'   store that fit separately in `aggregate`.
#' @param na_as Classification of `NA` responses: `"dk"` treats them as
#'   observed don't-know responses and `"missing"` treats them as structural
#'   missingness.
#' @param missing_action How to handle structural missingness: `"omit"`
#'   excludes incomplete pairs and `"error"` rejects them.
#' @param start Optional named feasible starting vector passed to
#'   [fit_item_lca_counts()].
#' @param control List passed to the `control` argument of [Rsolnp::solnp()].
#'
#' @return A `guess_fit` object. `diagnostics` records optimizer results by
#'   item. An aggregate fit, when requested, is stored separately and excluded
#'   from item metadata.
#'
#' @references
#' Cor, M. K., and Sood, G. (2016). Guessing and Forgetting: A Latent Class
#' Model for Measuring Learning. *Political Analysis*, 24(2), 226--242.
#'
#' Galanos, A., and Ye, Y. (2025). *Rsolnp: General Non-Linear Optimization*.
#' R package version 2.0.1.
#'
#' @export
#' @examples
#' sim <- simulate_lca(n = 500, n_items = 2, seed = 123)
#' fit_item_lca(sim$pre, sim$post)
fit_item_lca <- function(
  pre_test,
  post_test,
  ...,
  subgroup = NULL,
  include_aggregate = FALSE,
  na_as = c("dk", "missing"),
  missing_action = c("omit", "error"),
  start = NULL,
  control = list()
) {
  if (length(list(...)) > 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  call <- match.call()
  transition_counts <- count_item_transitions(
    pre_test,
    post_test,
    subgroup = subgroup,
    include_aggregate = include_aggregate,
    na_as = na_as,
    missing_action = missing_action
  )
  fit <- fit_item_lca_counts(
    transition_counts,
    start = start,
    control = control
  )
  fit$call <- call
  fit
}
