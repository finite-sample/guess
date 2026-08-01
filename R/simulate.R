#' Simulation Functions for LCA Models
#'
#' Functions to generate simulated pre/post test data from known LCA parameters
#' for validation and parameter recovery studies.

#' Simulate Pre-Post Test Data (No DK Model)
#'
#' Generates simulated pre/post test data from a latent class model with known
#' parameters. Useful for parameter recovery validation studies.
#'
#' @importFrom stats rbinom plogis cor
#'
#' @param n Integer. Number of individuals to simulate.
#' @param n_items Integer. Number of test items. Default 1.
#' @param gg Numeric. Proportion in guess->guess state (stable ignorance). Default 0.35.
#' @param gk Numeric. Proportion in guess->know state (LEARNED). Default 0.30.
#' @param kk Numeric. Proportion in know->know state (stable knowledge). Default 0.35.
#' @param gamma Numeric. Probability of guessing correctly. Can be scalar (same for
#'   all items) or vector of length n_items. Default 0.25.
#' @param difficulty Numeric vector. Optional IRT difficulty parameters. If provided,
#'   gamma is computed as base_rate + (1 - base_rate) * plogis(-difficulty).
#'   Higher difficulty = harder item (lower gamma). Ignored if NULL.
#' @param base_rate Numeric. Minimum guessing probability (random chance). Used when
#'   difficulty is specified. Default 0.25 (1/4 for 4-choice items).
#' @param seed Optional integer. Random seed for reproducibility.
#' @param return_classes Logical. If TRUE, also return true latent class assignments.
#'   Default FALSE for backward compatibility.
#'
#' @return List with components:
#'   \item{pre}{Data frame of pre-test responses (0/1 for each item)}
#'   \item{post}{Data frame of post-test responses (0/1 for each item)}
#'   \item{true_class}{(If return_classes=TRUE) Factor with levels "gg", "gk", "kk"}
#'   \item{learned}{(If return_classes=TRUE) Logical vector: TRUE if individual is in gk class}
#'
#' @details
#' The model simulates three latent classes:
#' - **gg (guess->guess)**: Don't know at both times. Responses are random guesses.
#' - **gk (guess->know)**: Learned between tests. Random guess pre, correct post.
#' - **kk (know->know)**: Know at both times. Correct responses at both times.
#'
#' Parameters must satisfy: gg + gk + kk = 1 (constraint enforced automatically).
#'
#' When difficulty is specified, gamma values are derived using an IRT-like
#' transformation: gamma_i = base_rate + (1 - base_rate) * plogis(-difficulty_i).
#' This means:
#' - difficulty = 0: gamma = base_rate + 0.5 * (1 - base_rate) (middle)
#' - difficulty -> +Inf: gamma -> base_rate (hard item, random guessing)
#' - difficulty -> -Inf: gamma -> 1 (easy item, always correct)
#'
#' @export
#' @examples
#' # Simulate data with 30% learning
#' sim <- simulate_lca(n = 500, gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25, seed = 123)
#' fit <- lca_fit(sim$pre, sim$post)
#' fit$params["gk", ]  # Should be close to 0.30
#'
#' # Multi-item simulation
#' sim_multi <- simulate_lca(n = 500, n_items = 3, seed = 456)
#'
#' # Item-specific gamma (vector)
#' sim_vec <- simulate_lca(n = 500, n_items = 3, gamma = c(0.2, 0.25, 0.3), seed = 789)
#'
#' # IRT-style difficulty parameters
#' sim_irt <- simulate_lca(n = 500, n_items = 3, difficulty = c(1, 0, -1), seed = 101)
#'
#' # Return true class assignments for validation
#' sim_classes <- simulate_lca(n = 500, gk = 0.30, seed = 123, return_classes = TRUE)
#' table(sim_classes$true_class)
#' mean(sim_classes$learned)  # Should be close to 0.30
simulate_lca <- function(n, n_items = 1, gg = 0.35, gk = 0.30, kk = 0.35,
                         gamma = 0.25, difficulty = NULL, base_rate = 0.25,
                         seed = NULL, return_classes = FALSE) {

  assert_int(n, lower = 1L)
  assert_int(n_items, lower = 1L)
  assert_numeric(gg, lower = 0, upper = 1, len = 1L)
  assert_numeric(gk, lower = 0, upper = 1, len = 1L)
  assert_numeric(kk, lower = 0, upper = 1, len = 1L)
  assert_numeric(base_rate, lower = 0, upper = 1, len = 1L)
  assert_flag(return_classes)

  if (!is.null(difficulty)) {
    assert_numeric(difficulty, any.missing = FALSE)
    if (length(difficulty) != n_items) {
      stop("difficulty must have length n_items (", n_items, ")")
    }
    gamma_vec <- base_rate + (1 - base_rate) * plogis(-difficulty)
  } else if (length(gamma) == 1L) {
    assert_numeric(gamma, lower = 0, upper = 1, len = 1L)
    gamma_vec <- rep(gamma, n_items)
  } else {
    assert_numeric(gamma, lower = 0, upper = 1)
    if (length(gamma) != n_items) {
      stop("gamma must be scalar or have length n_items (", n_items, ")")
    }
    gamma_vec <- gamma
  }

  total <- gg + gk + kk
  if (abs(total - 1) > 1e-6) {
    gg <- gg / total
    gk <- gk / total
    kk <- kk / total
    warning("Parameters normalized to sum to 1")
  }

  if (!is.null(seed)) set.seed(seed)

  classes <- sample(1:3, n, replace = TRUE, prob = c(gg, gk, kk))

  pre_list <- vector("list", n_items)
  post_list <- vector("list", n_items)

  for (item in seq_len(n_items)) {
    pre <- numeric(n)
    post <- numeric(n)
    g <- gamma_vec[item]

    for (i in seq_len(n)) {
      if (classes[i] == 1) {
        pre[i] <- rbinom(1, 1, g)
        post[i] <- rbinom(1, 1, g)
      } else if (classes[i] == 2) {
        pre[i] <- rbinom(1, 1, g)
        post[i] <- 1
      } else {
        pre[i] <- 1
        post[i] <- 1
      }
    }

    pre_list[[item]] <- pre
    post_list[[item]] <- post
  }

  pre_df <- as.data.frame(pre_list)
  post_df <- as.data.frame(post_list)
  names(pre_df) <- paste0("item", seq_len(n_items))
  names(post_df) <- paste0("item", seq_len(n_items))

  result <- list(pre = pre_df, post = post_df)

  if (return_classes) {
    class_labels <- c("gg", "gk", "kk")
    result$true_class <- factor(class_labels[classes], levels = class_labels)
    result$learned <- classes == 2L
  }

  result
}


#' Simulate Pre-Post Test Data (DK Model)
#'
#' Generates simulated pre/post test data from a latent class model with
#' Don't Know responses.
#'
#' @param n Integer. Number of individuals to simulate.
#' @param n_items Integer. Number of test items. Default 1.
#' @param gg Numeric. Proportion: guess->guess (stable ignorance). Default 0.25.
#' @param gk Numeric. Proportion: guess->know (learned). Default 0.15.
#' @param gd Numeric. Proportion: guess->dk. Default 0.10.
#' @param kk Numeric. Proportion: know->know (stable knowledge). Default 0.15.
#' @param dg Numeric. Proportion: dk->guess. Default 0.10.
#' @param dk Numeric. Proportion: dk->know (learned). Default 0.10.
#' @param dd Numeric. Proportion: dk->dk. Default 0.15.
#' @param gamma Numeric. Probability of guessing correctly. Can be scalar (same for
#'   all items) or vector of length n_items. Default 0.25.
#' @param difficulty Numeric vector. Optional IRT difficulty parameters. If provided,
#'   gamma is computed as base_rate + (1 - base_rate) * plogis(-difficulty).
#'   Higher difficulty = harder item (lower gamma). Ignored if NULL.
#' @param base_rate Numeric. Minimum guessing probability (random chance). Used when
#'   difficulty is specified. Default 0.25 (1/4 for 4-choice items).
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return List with two data frames:
#'   \item{pre}{Pre-test responses (character: "0", "1", or "d")}
#'   \item{post}{Post-test responses (character: "0", "1", or "d")}
#'
#' @details
#' The DK model has 7 latent classes representing transitions between
#' guess (g), know (k), and don't know (d) states:
#' - **gg**: guess both times
#' - **gk**: guess -> know (learned)
#' - **gd**: guess -> dk
#' - **kk**: know -> know
#' - **dg**: dk -> guess
#' - **dk**: dk -> know (learned)
#' - **dd**: dk -> dk
#'
#' The know -> guess and know -> dk classes are absent by design. The model is
#' identified by the assumption that people do not lose knowledge over a short
#' informative process, which sets both to zero. Learning is gk + dk.
#'
#' Parameters must sum to 1 (constraint enforced automatically).
#'
#' When difficulty is specified, gamma values are derived using an IRT-like
#' transformation: gamma_i = base_rate + (1 - base_rate) * plogis(-difficulty_i).
#'
#' @export
#' @examples
#' # Simulate DK data
#' sim <- simulate_lca_dk(n = 5000, gk = 0.15, seed = 123)
#' fit <- lca_fit(sim$pre, sim$post)
#' fit$params["gk", ]  # Should be close to 0.15
#'
#' # Item-specific gamma (vector)
#' sim_vec <- simulate_lca_dk(n = 500, n_items = 3, gamma = c(0.2, 0.25, 0.3), seed = 456)
#'
#' # IRT-style difficulty parameters
#' sim_irt <- simulate_lca_dk(n = 500, n_items = 3, difficulty = c(1, 0, -1), seed = 789)
simulate_lca_dk <- function(n, n_items = 1,
                            gg = 0.25, gk = 0.15, gd = 0.10,
                            kk = 0.15, dg = 0.10, dk = 0.10,
                            dd = 0.15, gamma = 0.25, difficulty = NULL,
                            base_rate = 0.25, seed = NULL) {

  assert_int(n, lower = 1L)
  assert_int(n_items, lower = 1L)
  assert_numeric(gg, lower = 0, upper = 1, len = 1L)
  assert_numeric(gk, lower = 0, upper = 1, len = 1L)
  assert_numeric(gd, lower = 0, upper = 1, len = 1L)
  assert_numeric(kk, lower = 0, upper = 1, len = 1L)
  assert_numeric(dg, lower = 0, upper = 1, len = 1L)
  assert_numeric(dk, lower = 0, upper = 1, len = 1L)
  assert_numeric(dd, lower = 0, upper = 1, len = 1L)
  assert_numeric(base_rate, lower = 0, upper = 1, len = 1L)

  if (!is.null(difficulty)) {
    assert_numeric(difficulty, any.missing = FALSE)
    if (length(difficulty) != n_items) {
      stop("difficulty must have length n_items (", n_items, ")")
    }
    gamma_vec <- base_rate + (1 - base_rate) * plogis(-difficulty)
  } else if (length(gamma) == 1L) {
    assert_numeric(gamma, lower = 0, upper = 1, len = 1L)
    gamma_vec <- rep(gamma, n_items)
  } else {
    assert_numeric(gamma, lower = 0, upper = 1)
    if (length(gamma) != n_items) {
      stop("gamma must be scalar or have length n_items (", n_items, ")")
    }
    gamma_vec <- gamma
  }

  lambdas <- c(gg, gk, gd, kk, dg, dk, dd)
  total <- sum(lambdas)
  if (abs(total - 1) > 1e-6) {
    lambdas <- lambdas / total
    warning("Parameters normalized to sum to 1")
  }

  if (!is.null(seed)) set.seed(seed)

  classes <- sample(1:7, n, replace = TRUE, prob = lambdas)

  pre_list <- vector("list", n_items)
  post_list <- vector("list", n_items)

  for (item in seq_len(n_items)) {
    pre <- character(n)
    post <- character(n)
    g <- gamma_vec[item]

    # Classes are, in order: gg, gk, gd, kk, dg, dk, dd. A guesser answers
    # correctly with probability gamma; someone who knows always answers
    # correctly; someone who confesses ignorance answers "d".
    for (i in seq_len(n)) {
      cl <- classes[i]
      if (cl == 1) {
        pre[i] <- ifelse(rbinom(1, 1, g) == 1, "1", "0")
        post[i] <- ifelse(rbinom(1, 1, g) == 1, "1", "0")
      } else if (cl == 2) {
        pre[i] <- ifelse(rbinom(1, 1, g) == 1, "1", "0")
        post[i] <- "1"
      } else if (cl == 3) {
        pre[i] <- ifelse(rbinom(1, 1, g) == 1, "1", "0")
        post[i] <- "d"
      } else if (cl == 4) {
        pre[i] <- "1"
        post[i] <- "1"
      } else if (cl == 5) {
        pre[i] <- "d"
        post[i] <- ifelse(rbinom(1, 1, g) == 1, "1", "0")
      } else if (cl == 6) {
        pre[i] <- "d"
        post[i] <- "1"
      } else {
        pre[i] <- "d"
        post[i] <- "d"
      }
    }

    pre_list[[item]] <- pre
    post_list[[item]] <- post
  }

  pre_df <- as.data.frame(pre_list, stringsAsFactors = FALSE)
  post_df <- as.data.frame(post_list, stringsAsFactors = FALSE)
  names(pre_df) <- paste0("item", seq_len(n_items))
  names(post_df) <- paste0("item", seq_len(n_items))

  list(pre = pre_df, post = post_df)
}
