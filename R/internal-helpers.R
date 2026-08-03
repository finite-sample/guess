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

#' Create difficulty-parameterized likelihood function (no DK)
#' @description Factory function that creates a likelihood function parameterized with
#'   an unbounded difficulty score instead of gamma. Used internally by
#'   lca_difficulty().
#' @keywords internal
#'
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return A function that takes x (parameters) and data (transition matrix)

make_guess_lik_difficulty <- function(base_rate = 0.25) {
  function(x, g1 = NA, data) {
    g1 <- base_rate + (1 - base_rate) * plogis(-x[4])
    multinomial_nll(
      data,
      nodk_cell_probs(x[1], x[2], x[3], g1)
    )
  }
}

#' Create difficulty-parameterized likelihood function (DK)
#' @description Factory function that creates a likelihood function parameterized with
#'   an unbounded difficulty score instead of gamma. Used internally by
#'   lca_difficulty().
#' @keywords internal
#'
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return A function that takes x (parameters) and data (transition matrix)

make_guessdk_lik_difficulty <- function(base_rate = 0.25) {
  function(x, g1 = NA, data) {
    g1 <- base_rate + (1 - base_rate) * plogis(-x[8])
    multinomial_nll(
      data,
      dk_cell_probs(x[1], x[2], x[3], x[4], x[5], x[6], x[7], g1)
    )
  }
}

#' Transform difficulty to gamma
#' @description Convert a difficulty score to a guessing probability.
#' @keywords internal
#'
#' @param difficulty numeric vector of difficulty parameters
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return numeric vector of gamma values

difficulty_to_gamma <- function(difficulty, base_rate = 0.25) {
  base_rate + (1 - base_rate) * plogis(-difficulty)
}

#' Transform gamma to difficulty
#' @description Convert guessing probability to a difficulty score.
#' @keywords internal
#'
#' @param gamma numeric vector of guessing probabilities
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return numeric vector of difficulty values

gamma_to_difficulty <- function(gamma, base_rate = 0.25) {
  p <- (gamma - base_rate) / (1 - base_rate)
  p <- pmax(pmin(p, 0.9999), 0.0001)
  -qlogis(p)
}
