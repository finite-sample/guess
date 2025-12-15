# Internal helper functions used by the main fitting functions

#' Constraints: Sum to 1
#' @description Constraints that some params sum to 1. Used Internally. For data without DK.
#' Functions for constraining lambdas to sum to 1 and to bound params between 0 and 1
#' @keywords internal
#' 
#' @param x    lgg, lgk, lkk
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
#' @param x    lgg, lgk, lkk
#' @param g1   guess
#' @param data transition matrix

eq1dk <- function(x, g1 = NA, data) {
  sum(x[1:7])
}

#' guess_lik
#' @description Likelihood function for data without Don't Know. Used Internally.
#' @keywords internal
#' 
#' @param x    lgg, lgk, lkk
#' @param g1   guess
#' @param data transition matrix

guess_lik <- function(x, g1 = x[4], data) {

  lgg <- x[1]
  lgk <- x[2]
  lkk <- x[3]

  vec <- NA
  vec[1] <- (1 - g1) * (1 - g1) * lgg
  vec[2] <- (1 - g1) * g1 * lgg + (1 - g1) * lgk
  vec[3] <- (1 - g1) * g1 * lgg
  vec[4] <- g1 * g1 * lgg + g1 * lgk + lkk

  -sum(data * log(vec))
}

#' guessdk_lik
#' @description Likelihood function for data with Don't Know. Used Internally.
#' @keywords internal
#' 
#' @param x     lgg, lgk, lgd, lkg, lkk, lkd, ldd
#' @param g1    guess
#' @param data  transition matrix

guessdk_lik <- function(x, g1 = x[8], data) {

  lgg <- x[1]
  lgk <- x[2]
  lgd <- x[3]
  lkg <- x[4]
  lkk <- x[5]
  lkd <- x[6]
  ldd <- x[7]

  vec <- NA
  vec[1] <- (1 - g1) * (1 - g1) * lgg
  vec[2] <- (1 - g1) * g1 * lgg + (1 - g1) * lgk
  vec[3] <- (1 - g1) * lgd
  vec[4] <- (1 - g1) * g1 * lgg + lkg
  vec[5] <- g1 * g1 * lgg + g1 * lgk + g1 * lkg + lkk
  vec[6] <- g1 * lgd + lkd
  vec[7] <- lkg
  vec[8] <- g1 * lgk + lkd
  vec[9] <- ldd

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
    result[2*i - 1] <- a[i]
    result[2*i] <- b[i]
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