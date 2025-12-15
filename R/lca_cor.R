#' guesstimate

#' @title Calculate item level and aggregate learning
#' @param transmatrix  transition matrix returned from \code{\link{multi_transmat}}
#' @param nodk_priors Optional. Vector of length 4. Priors for the parameters for model that fits data without Don't Knows
#' @param dk_priors  Optional. Vector of length 8. Priors for the parameters for model that fits data with Don't Knows
#' @return list with two items: parameter estimates and estimates of learning
#' @export
#' @examples
#' # Without DK
#' pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0)) 
#' pst_test <- pre_test + cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 0, 1))
#' transmatrix <- multi_transmat(pre_test, pst_test)
#' res <- lca_cor(transmatrix)

lca_cor <- function(transmatrix = NULL, nodk_priors = c(0.3, 0.1, 0.1, 0.25),
                   dk_priors = c(0.3, 0.1, 0.2, 0.05, 0.1, 0.1, 0.05, 0.25)) {
  
  # Input validation
  validate_matrix(transmatrix, "transmatrix", valid_ncols = c(4, 9))
  
  # Validate priors
  if (ncol(transmatrix) == 4) {
    validate_priors(nodk_priors, 4, "nodk_priors")
  } else if (ncol(transmatrix) == 9) {
    validate_priors(dk_priors, 8, "dk_priors")
  }

  # Initialize results mat
  nitems  <- nrow(transmatrix)
  nparams <- ifelse(ncol(transmatrix) == 4, 4, 8)
  est.opt <- matrix(ncol = nitems, nrow = nparams)

  # Use appropriate priors based on model type

  # effects
  effects  <- matrix(ncol = nitems, nrow = 1)

  # calculating parameter estimates
  if (nparams == 4) {
    for (i in seq_len(nitems)) {
      est.opt[, i]   <- tryCatch(solnp(nodk_priors,
                                       guess_lik,
                                       eqfun = eqn1,
                                       eqB = c(1),
                                       LB = rep(0, 4),
                                       UB = rep(1, 4),
                                       data = transmatrix[i, ])[[1]],
                                       error = function(e) NULL)
    }

    effects[, 1:nitems] <- est.opt[2, ]

  } else {
    for (i in seq_len(nitems)) {
      est.opt[, i]   <- tryCatch(solnp(dk_priors,
                                       guessdk_lik,
                                       eqfun = eq1dk,
                                       eqB = c(1),
                                       LB = rep(0, 8),
                                       UB = rep(1, 8),
                                       data = transmatrix[i, ])[[1]],
                                       error = function(e) rep(NA, 8))
    }

    effects[, 1:nitems]   <- est.opt[2, ] + est.opt[6, ]
  }

  # Assign row names based on model type
  if (nparams == 8) {
    row.names(est.opt) <- c("lgg", "lgk", "lgc", "lkk", "lcg", "lck", "lcc", "gamma")
  } else {
    row.names(est.opt) <- c("lgg", "lgk", "lkk", "gamma")
  }

  list(param.lca = est.opt, est.learning = effects)
}
