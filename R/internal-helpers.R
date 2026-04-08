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
#' @param x    gg, gk, gd, kg, kk, kd, dd
#' @param g1   guess
#' @param data transition matrix

eq1dk <- function(x, g1 = NA, data) {
  sum(x[1:7])
}

#' guess_lik
#' @description Likelihood function for data without Don't Know. Used Internally.
#' @keywords internal
#'
#' @param x    gg, gk, kk
#' @param g1   guess
#' @param data transition matrix

guess_lik <- function(x, g1 = x[4], data) {
  gg <- x[1]
  gk <- x[2]
  kk <- x[3]

  vec <- numeric(4)
  vec[CELL_00] <- (1 - g1) * (1 - g1) * gg
  vec[CELL_01] <- (1 - g1) * g1 * gg + (1 - g1) * gk
  vec[CELL_10] <- (1 - g1) * g1 * gg
  vec[CELL_11] <- g1 * g1 * gg + g1 * gk + kk

  -sum(data * log(vec))
}

#' guessdk_lik
#' @description Likelihood function for data with Don't Know. Used Internally.
#' @keywords internal
#'
#' @param x     gg, gk, gd, kg, kk, kd, dd
#' @param g1    guess
#' @param data  transition matrix

guessdk_lik <- function(x, g1 = x[8], data) {
  gg <- x[1]
  gk <- x[2]
  gd <- x[3]
  kg <- x[4]
  kk <- x[5]
  kd <- x[6]
  dd <- x[7]

  vec <- numeric(9)
  vec[CELL_00_DK] <- (1 - g1) * (1 - g1) * gg
  vec[CELL_01_DK] <- (1 - g1) * g1 * gg + (1 - g1) * gk
  vec[CELL_0D] <- (1 - g1) * gd
  vec[CELL_10_DK] <- (1 - g1) * g1 * gg + kg
  vec[CELL_11_DK] <- g1 * g1 * gg + g1 * gk + g1 * kg + kk
  vec[CELL_1D] <- g1 * gd + kd
  vec[CELL_D0] <- kg
  vec[CELL_D1] <- g1 * gk + kd
  vec[CELL_DD] <- dd

  -sum(data * log(vec))
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

#' Create IRT-parameterized likelihood function (no DK)
#' @description Factory function that creates a likelihood function parameterized with
#'   IRT difficulty instead of gamma. Used internally by lca_irt().
#' @keywords internal
#'
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return A function that takes x (parameters) and data (transition matrix)

make_guess_lik_irt <- function(base_rate = 0.25) {
  function(x, g1 = NA, data) {
    gg <- x[1]
    gk <- x[2]
    kk <- x[3]
    d <- x[4]

    g1 <- base_rate + (1 - base_rate) * plogis(-d)

    vec <- numeric(4)
    vec[CELL_00] <- (1 - g1) * (1 - g1) * gg
    vec[CELL_01] <- (1 - g1) * g1 * gg + (1 - g1) * gk
    vec[CELL_10] <- (1 - g1) * g1 * gg
    vec[CELL_11] <- g1 * g1 * gg + g1 * gk + kk

    -sum(data * log(vec))
  }
}

#' Create IRT-parameterized likelihood function (DK)
#' @description Factory function that creates a likelihood function parameterized with
#'   IRT difficulty instead of gamma. Used internally by lca_irt().
#' @keywords internal
#'
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return A function that takes x (parameters) and data (transition matrix)

make_guessdk_lik_irt <- function(base_rate = 0.25) {
  function(x, g1 = NA, data) {
    gg <- x[1]
    gk <- x[2]
    gd <- x[3]
    kg <- x[4]
    kk <- x[5]
    kd <- x[6]
    dd <- x[7]
    d <- x[8]

    g1 <- base_rate + (1 - base_rate) * plogis(-d)

    vec <- numeric(9)
    vec[CELL_00_DK] <- (1 - g1) * (1 - g1) * gg
    vec[CELL_01_DK] <- (1 - g1) * g1 * gg + (1 - g1) * gk
    vec[CELL_0D] <- (1 - g1) * gd
    vec[CELL_10_DK] <- (1 - g1) * g1 * gg + kg
    vec[CELL_11_DK] <- g1 * g1 * gg + g1 * gk + g1 * kg + kk
    vec[CELL_1D] <- g1 * gd + kd
    vec[CELL_D0] <- kg
    vec[CELL_D1] <- g1 * gk + kd
    vec[CELL_DD] <- dd

    -sum(data * log(vec))
  }
}

#' Transform difficulty to gamma
#' @description Convert IRT difficulty parameter to guessing probability.
#' @keywords internal
#'
#' @param difficulty numeric vector of difficulty parameters
#' @param base_rate minimum guessing probability (1/K for K-choice items)
#' @return numeric vector of gamma values

difficulty_to_gamma <- function(difficulty, base_rate = 0.25) {
  base_rate + (1 - base_rate) * plogis(-difficulty)
}

#' Transform gamma to difficulty
#' @description Convert guessing probability to IRT difficulty parameter.
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
