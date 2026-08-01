#' guesstimate

#' @title Calculate item level and aggregate learning
#' @param transmatrix  transition matrix returned from \code{\link{multi_transmat}}
#' @param nodk_priors Optional. Vector of length 4. Priors for the parameters
#'   for model that fits data without Don't Knows
#' @param dk_priors  Optional. Vector of length 8. Priors for the parameters
#'   for model that fits data with Don't Knows
#' @return list with two items: parameter estimates and estimates of learning
#' @export
#' @examples
#' # Without DK
#' pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0)) 
#' pst_test <- pre_test + cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 0, 1))
#' transmatrix <- multi_transmat(pre_test, pst_test)
#' res <- lca_cor(transmatrix)

lca_cor <- function(transmatrix = NULL,
                    nodk_priors = c(0.3, 0.1, 0.1, 0.25),
                    dk_priors = c(0.30, 0.15, 0.10, 0.20, 0.05, 0.10, 0.10, 0.25)) {

  validate_matrix(transmatrix, "transmatrix", valid_ncols = c(4, 9))

  is_dk <- ncol(transmatrix) == 9L
  n_params <- if (is_dk) 8L else 4L

  if (is_dk) {
    validate_priors(dk_priors, 8, "dk_priors")
  } else {
    validate_priors(nodk_priors, 4, "nodk_priors")
  }

  n_items <- nrow(transmatrix)
  est_opt <- matrix(ncol = n_items, nrow = n_params)
  effects <- matrix(ncol = n_items, nrow = 1)

  if (is_dk) {
    row.names(est_opt) <- c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
    for (i in seq_len(n_items)) {
      est_opt[, i] <- tryCatch(
        solnp(dk_priors, guessdk_lik, eqfun = eq1dk, eqB = 1,
              LB = rep(0, 8), UB = rep(1, 8), data = transmatrix[i, ],
              control = list(trace = 0))[[1]],
        error = function(e) rep(NA, 8)
      )
    }
    effects[, seq_len(n_items)] <- est_opt["gk", ] + est_opt["dk", ]
  } else {
    row.names(est_opt) <- c("gg", "gk", "kk", "gamma")
    for (i in seq_len(n_items)) {
      est_opt[, i] <- tryCatch(
        solnp(nodk_priors, guess_lik, eqfun = eqn1, eqB = 1,
              LB = rep(0, 4), UB = rep(1, 4), data = transmatrix[i, ],
              control = list(trace = 0))[[1]],
        error = function(e) NULL
      )
    }
    effects[, seq_len(n_items)] <- est_opt["gk", ]
  }

  item_names <- rownames(transmatrix)
  if (is.null(item_names)) {
    item_names <- paste0("item", seq_len(n_items))
  }
  colnames(est_opt) <- item_names
  colnames(effects) <- item_names

  n_obs <- sum(transmatrix)
  model_type <- if (is_dk) "dk" else "nodk"

  new_guess_fit(
    params = est_opt,
    learning = as.numeric(effects),
    n_items = n_items,
    n_obs = n_obs,
    model_type = model_type,
    call = match.call()
  )
}

#' IRT-Parameterized LCA Estimation
#'
#' @title Estimate LCA model with IRT difficulty parameterization
#' @description Fits an LCA model where item difficulty is parameterized using
#'   IRT-style difficulty parameters instead of raw gamma (guessing probability).
#'   This allows difficulty to be unbounded on the real line, which can improve
#'   optimization and makes difficulty parameters more interpretable.
#'
#' @param transmatrix Transition matrix returned from \code{\link{multi_transmat}}
#' @param base_rate Numeric. Minimum guessing probability (random chance). Default 0.25
#'   (1/4 for 4-choice items). This is the floor for gamma when difficulty -> +Inf.
#' @param nodk_priors Optional. Vector of length 4. Starting values for
#'   (gg, gk, kk, difficulty). First 3 must sum to 1.
#' @param dk_priors Optional. Vector of length 8. Starting values for DK model.
#'   First 7 must sum to 1.
#'
#' @return A guess_fit object with additional components:
#'   \item{params}{Parameter matrix with "difficulty" row instead of "gamma"}
#'   \item{gamma}{Derived gamma values from difficulty (added for convenience)}
#'   \item{learning}{Learning estimates (gk or gk + kd)}
#'
#' @details
#' The relationship between difficulty (d) and gamma is:
#' \deqn{\gamma = base\_rate + (1 - base\_rate) \cdot logistic(-d)}
#'
#' Where logistic(x) = 1/(1+exp(-x)). This means:
#' \itemize{
#'   \item d = 0: gamma = base_rate + 0.5*(1-base_rate) (middle difficulty)
#'   \item d -> +Inf: gamma -> base_rate (hard item, random guessing)
#'   \item d -> -Inf: gamma -> 1 (easy item, always correct even when guessing)
#' }
#'
#' @importFrom Rsolnp solnp
#' @importFrom stats plogis
#' @export
#' @examples
#' # Simulate data with known difficulty
#' sim <- simulate_lca(n = 500, n_items = 3, difficulty = c(1, 0, -1), seed = 123)
#' transmatrix <- multi_transmat(sim$pre, sim$post)
#'
#' # Fit with IRT parameterization
#' fit_irt <- lca_irt(transmatrix)
#' fit_irt$params["difficulty", ]  # Should recover approximately c(1, 0, -1)
#' fit_irt$gamma                   # Derived gamma values

lca_irt <- function(transmatrix = NULL, base_rate = 0.25,
                    nodk_priors = c(0.35, 0.30, 0.35, 0),
                    dk_priors = c(0.25, 0.15, 0.10, 0.20, 0.10, 0.10, 0.10, 0)) {

  validate_matrix(transmatrix, "transmatrix", valid_ncols = c(4, 9))
  assert_numeric(base_rate, lower = 0, upper = 1, len = 1L)

  is_dk <- ncol(transmatrix) == 9L
  n_params <- if (is_dk) 8L else 4L

  if (is_dk) {
    if (length(dk_priors) != 8L) {
      stop("dk_priors must have length 8")
    }
  } else {
    if (length(nodk_priors) != 4L) {
      stop("nodk_priors must have length 4")
    }
  }

  n_items <- nrow(transmatrix)
  est_opt <- matrix(ncol = n_items, nrow = n_params)
  effects <- matrix(ncol = n_items, nrow = 1)
  gamma_vec <- numeric(n_items)

  if (is_dk) {
    row.names(est_opt) <- c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "difficulty")
    lik_fn <- make_guessdk_lik_irt(base_rate)
    for (i in seq_len(n_items)) {
      result <- tryCatch({
        solnp(dk_priors, lik_fn, eqfun = eq1dk, eqB = 1,
              LB = c(rep(0, 7), -10), UB = c(rep(1, 7), 10),
              data = transmatrix[i, ], control = list(trace = 0))[[1]]
      }, error = function(e) rep(NA, 8))
      est_opt[, i] <- result
      gamma_vec[i] <- difficulty_to_gamma(result[8], base_rate)
    }
    effects[, seq_len(n_items)] <- est_opt["gk", ] + est_opt["dk", ]
  } else {
    row.names(est_opt) <- c("gg", "gk", "kk", "difficulty")
    lik_fn <- make_guess_lik_irt(base_rate)
    for (i in seq_len(n_items)) {
      result <- tryCatch({
        solnp(nodk_priors, lik_fn, eqfun = eqn1, eqB = 1,
              LB = c(rep(0, 3), -10), UB = c(rep(1, 3), 10),
              data = transmatrix[i, ], control = list(trace = 0))[[1]]
      }, error = function(e) rep(NA, 4))
      est_opt[, i] <- result
      gamma_vec[i] <- difficulty_to_gamma(result[4], base_rate)
    }
    effects[, seq_len(n_items)] <- est_opt["gk", ]
  }

  item_names <- rownames(transmatrix)
  if (is.null(item_names)) {
    item_names <- paste0("item", seq_len(n_items))
  }
  colnames(est_opt) <- item_names
  colnames(effects) <- item_names
  names(gamma_vec) <- item_names

  n_obs <- sum(transmatrix)
  model_type <- if (is_dk) "dk" else "nodk"

  fit <- new_guess_fit(
    params = est_opt,
    learning = as.numeric(effects),
    n_items = n_items,
    n_obs = n_obs,
    model_type = model_type,
    call = match.call()
  )

  fit$gamma <- gamma_vec
  fit$base_rate <- base_rate
  class(fit) <- c("guess_irt_fit", class(fit))

  fit
}
