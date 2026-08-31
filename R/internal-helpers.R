# Internal helper functions used by the main fitting functions
#' @importFrom stats qlogis plogis
NULL

#' Sum to 1 constraint (no DK)
#' @description Constraints that some params sum to 1. Used Internally. For data without DK.
#' Functions for constraining lambdas to sum to 1 and to bound params between 0 and 1
#' @keywords internal
#'
#' @param x    gg, gk, kk
#' @param g1   guess
#' @param data transition matrix

eqn1 <- function(x, g1 = NA, data) {
  sum(x[1:3])
}

#' Constraints: Sum to 1
#' @description Constraints that some params sum to 1. Used Internally. For data with DK.
#' Functions for constraining lambdas to sum to 1 and to bound params between 0 and 1
#' @keywords internal
#'
#' @param x    gg, gk, gd, kk, dg, dk, dd
#' @param g1   guess
#' @param data transition matrix

eq1dk <- function(x, g1 = NA, data) {
  sum(x[1:7])
}

#' Cell probabilities for the model without Don't Know
#'
#' @description The multinomial cell probabilities implied by the latent class
#'   transition parameters. This is the single definition of the model; every
#'   likelihood, expected-count and goodness-of-fit routine calls it rather than
#'   restating the algebra.
#'
#'   Latent classes are named pre-state then post-state over guess (g) and know
#'   (k), so `gk` is guess at the pretest and know at the posttest. Knowledge is
#'   assumed not to be lost over the process, so the know-to-guess class is
#'   identically zero and does not appear.
#'
#' @param gg proportion guess -> guess
#' @param gk proportion guess -> know
#' @param kk proportion know -> know
#' @param g1 probability a guess is correct (gamma)
#' @return numeric vector of length 4, ordered x00, x01, x10, x11
#' @keywords internal

nodk_cell_probs <- function(gg, gk, kk, g1) {
  vec <- numeric(4)
  vec[CELL_00] <- (1 - g1) * (1 - g1) * gg
  vec[CELL_01] <- (1 - g1) * g1 * gg + (1 - g1) * gk
  vec[CELL_10] <- (1 - g1) * g1 * gg
  vec[CELL_11] <- g1 * g1 * gg + g1 * gk + kk
  vec
}

#' Cell probabilities for the model with Don't Know
#'
#' @description The multinomial cell probabilities implied by the latent class
#'   transition parameters, as in equation (2) of Cor and Sood. The nine latent
#'   transitions among guess (g), know (k) and don't know (d) are reduced to
#'   seven by the identifying assumption that people do not lose knowledge over
#'   the course of a short informative process: the know-to-guess and
#'   know-to-don't-know classes are identically zero and do not appear.
#'
#'   The seven remaining proportions sum to 1, and so do the nine cell
#'   probabilities returned here. Given those probabilities the parameters are
#'   recoverable in closed form -- gamma / (1 - gamma) is x10 / x00 -- with one
#'   over-identifying restriction left over, x1d / x0d = x10 / x00.
#'
#' @param gg proportion guess -> guess
#' @param gk proportion guess -> know
#' @param gd proportion guess -> don't know
#' @param kk proportion know -> know
#' @param dg proportion don't know -> guess
#' @param dk proportion don't know -> know
#' @param dd proportion don't know -> don't know
#' @param g1 probability a guess is correct (gamma)
#' @return numeric vector of length 9, ordered x00, x01, x0d, x10, x11, x1d,
#'   xd0, xd1, xdd
#' @keywords internal

dk_cell_probs <- function(gg, gk, gd, kk, dg, dk, dd, g1) {
  vec <- numeric(9)
  vec[CELL_00_DK] <- (1 - g1) * (1 - g1) * gg
  vec[CELL_01_DK] <- (1 - g1) * g1 * gg + (1 - g1) * gk
  vec[CELL_0D] <- (1 - g1) * gd
  vec[CELL_10_DK] <- g1 * (1 - g1) * gg
  vec[CELL_11_DK] <- g1 * g1 * gg + g1 * gk + kk
  vec[CELL_1D] <- g1 * gd
  vec[CELL_D0] <- (1 - g1) * dg
  vec[CELL_D1] <- g1 * dg + dk
  vec[CELL_DD] <- dd
  vec
}

#' guess_lik
#' @description Likelihood function for data without Don't Know. Used Internally.
#' @keywords internal
#'
#' @param x    gg, gk, kk
#' @param g1   guess
#' @param data transition matrix

guess_lik <- function(x, g1 = x[4], data) {
  multinomial_nll(
    data,
    nodk_cell_probs(x[1], x[2], x[3], g1)
  )
}

#' guessdk_lik
#' @description Likelihood function for data with Don't Know. Used Internally.
#' @keywords internal
#'
#' @param x     gg, gk, gd, kk, dg, dk, dd
#' @param g1    guess
#' @param data  transition matrix

guessdk_lik <- function(x, g1 = x[8], data) {
  multinomial_nll(
    data,
    dk_cell_probs(x[1], x[2], x[3], x[4], x[5], x[6], x[7], g1)
  )
}

#' Multinomial negative log-likelihood
#' @param data observed cell counts
#' @param probs model-implied cell probabilities
#' @return scalar negative log-likelihood
#' @keywords internal
multinomial_nll <- function(data, probs) {
  observed <- data > 0
  if (any(probs[observed] <= 0)) {
    return(Inf)
  }
  -sum(data[observed] * log(probs[observed]))
}

#' Validate an optional LCA starting vector
#' @param start optional named numeric vector
#' @param parameter_names required parameter names
#' @return reordered starting vector or NULL
#' @keywords internal
validate_lca_start <- function(start, parameter_names) {
  if (is.null(start)) {
    return(NULL)
  }
  if (
    !is.numeric(start) || anyNA(start) || any(!is.finite(start)) ||
      length(start) != length(parameter_names) || is.null(names(start)) ||
      anyDuplicated(names(start)) || !setequal(names(start), parameter_names)
  ) {
    stop(
      "start must be a finite named numeric vector with names: ",
      paste(parameter_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  start <- start[parameter_names]
  if (any(start < 0 | start > 1)) {
    stop("start values must lie between 0 and 1.", call. = FALSE)
  }
  class_values <- start[parameter_names != "gamma"]
  if (abs(sum(class_values) - 1) > sqrt(.Machine$double.eps)) {
    stop("Latent-class start values must sum to 1.", call. = FALSE)
  }
  start
}

#' Construct a deterministic feasible LCA starting vector
#' @param counts named transition counts
#' @param parameter_names parameter names for the selected model
#' @return named numeric vector
#' @keywords internal
make_lca_start <- function(counts, parameter_names) {
  class_names <- parameter_names[parameter_names != "gamma"]
  denominator <- counts[["x00"]] + counts[["x10"]]
  gamma <- if (denominator > 0) counts[["x10"]] / denominator else 0.5
  tolerance <- sqrt(.Machine$double.eps)
  gamma <- min(max(gamma, tolerance), 1 - tolerance)

  start <- rep(1 / length(class_names), length(class_names))
  names(start) <- class_names
  c(start, gamma = gamma)
}

#' Fit one transition-count row
#' @param counts named transition counts
#' @param item_name item label for diagnostics and errors
#' @param is_dk whether the model includes don't-know states
#' @param start optional user-supplied starting vector
#' @param control Rsolnp control list
#' @return list with parameters, learning, and diagnostics
#' @keywords internal
fit_lca_count_row <- function(counts, item_name, is_dk, start, control) {
  parameter_names <- if (is_dk) {
    c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
  } else {
    c("gg", "gk", "kk", "gamma")
  }
  row_start <- if (is.null(start)) {
    make_lca_start(counts, parameter_names)
  } else {
    start
  }
  objective <- if (is_dk) guessdk_lik else guess_lik
  equality <- if (is_dk) eq1dk else eqn1

  result <- tryCatch(
    solnp(
      row_start,
      objective,
      eqfun = equality,
      eqB = 1,
      LB = rep(0, length(parameter_names)),
      UB = rep(1, length(parameter_names)),
      data = counts,
      control = control
    ),
    error = function(error) {
      stop(
        "Optimization failed for item `",
        item_name,
        "`: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  if (!is.numeric(result$convergence) || length(result$convergence) != 1L) {
    stop("Optimizer returned no convergence code for item `", item_name, "`.", call. = FALSE)
  }
  if (result$convergence != 0) {
    stop(
      "Optimization did not converge for item `",
      item_name,
      "` (code ",
      result$convergence,
      ").",
      call. = FALSE
    )
  }

  params <- result$pars
  tolerance <- sqrt(.Machine$double.eps)
  if (
    !is.numeric(params) || length(params) != length(parameter_names) ||
      anyNA(params) || any(!is.finite(params)) ||
      any(params < -tolerance | params > 1 + tolerance)
  ) {
    stop("Optimizer returned invalid parameters for item `", item_name, "`.", call. = FALSE)
  }
  names(params) <- parameter_names
  if (abs(sum(params[parameter_names != "gamma"]) - 1) > tolerance) {
    stop("Optimizer violated the class constraint for item `", item_name, "`.", call. = FALSE)
  }
  params <- pmin(pmax(params, 0), 1)

  final_objective <- utils::tail(result$values, 1L)
  if (length(final_objective) != 1L || !is.finite(final_objective)) {
    stop("Optimizer returned an invalid objective for item `", item_name, "`.", call. = FALSE)
  }
  learning <- if (is_dk) params[["gk"]] + params[["dk"]] else params[["gk"]]

  list(
    params = params,
    learning = unname(learning),
    diagnostics = data.frame(
      convergence = as.integer(result$convergence),
      objective = as.numeric(final_objective),
      evaluations = as.integer(result$nfuneval),
      iterations = as.integer(result$outer.iter),
      row.names = item_name
    )
  )
}

#' Interleave vectors
#' @description Interleaves two vectors. Used internally.
#' @keywords internal
#'
#' @param a first vector
#' @param b second vector

interleave <- function(a, b) {
  shorter <- min(length(a), length(b))
  result <- vector(mode = typeof(a), length = length(a) + length(b))

  # Fill interleaved portion
  for (i in seq_len(shorter)) {
    result[2 * i - 1] <- a[i]
    result[2 * i] <- b[i]
  }

  # Add remaining elements
  if (length(a) > shorter) {
    result[(2 * shorter + 1):length(result)] <- a[(shorter + 1):length(a)]
  } else if (length(b) > shorter) {
    result[(2 * shorter + 1):length(result)] <- b[(shorter + 1):length(b)]
  }

  result
}

#' Constrain vector to [0,1] range
#' @description Constrains values in a vector to be between 0 and 1. Used internally.
#' @keywords internal
#'
#' @param x numeric vector to constrain
#' @return numeric vector with values constrained to [0,1]

zero1 <- function(x) {
  pmax(0, pmin(1, x))
}
