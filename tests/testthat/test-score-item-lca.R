make_known_binary_fit <- function() {
  structure(
    list(
      params = matrix(
        c(gg = 0.4, gk = 0.3, kk = 0.3, gamma = 0.25),
        ncol = 1L,
        dimnames = list(c("gg", "gk", "kk", "gamma"), "item_a")
      ),
      learning = c(item_a = 0.3),
      n_items = 1L,
      n_obs = 40L,
      model_type = "nodk"
    ),
    class = "guess_fit"
  )
}

test_that("score_item_lca returns exact log scores for named transition counts", {
  fit <- make_known_binary_fit()
  transition_counts <- matrix(
    c(9L, 12L, 3L, 16L),
    nrow = 1L,
    dimnames = list("item_a", c("x00", "x01", "x10", "x11"))
  )

  score <- score_item_lca(fit, transition_counts)
  expected_log_likelihood <- sum(
    c(9L, 12L, 3L, 16L) * log(c(0.225, 0.3, 0.075, 0.4))
  )

  expect_s3_class(score, "guess_item_score")
  expect_equal(score$item_scores["item_a", "log_likelihood"], expected_log_likelihood)
  expect_equal(score$total_log_likelihood, expected_log_likelihood)
  expect_equal(score$n_observations, 40L)
  expect_equal(score$perplexity, exp(-expected_log_likelihood / 40))
})

test_that("score_item_lca matches fitted parameters and counts by item name", {
  sim <- simulate_lca(n = 2500, n_items = 2, gamma = c(0.2, 0.4), seed = 842)
  transition_counts <- count_item_transitions(sim$pre, sim$post)
  fit <- fit_item_lca_counts(transition_counts)
  reordered_fit <- fit
  reordered_fit$params <- fit$params[, 2:1, drop = FALSE]
  reordered_counts <- transition_counts[2:1, , drop = FALSE]

  original <- score_item_lca(fit, transition_counts)
  reordered_fit_score <- score_item_lca(reordered_fit, transition_counts)
  reordered_count_score <- score_item_lca(fit, reordered_counts)

  expect_equal(
    reordered_fit_score$item_scores,
    original$item_scores,
    tolerance = 1e-12
  )
  expect_equal(
    reordered_count_score$item_scores[rownames(original$item_scores), ],
    original$item_scores,
    tolerance = 1e-12
  )
  expect_equal(
    reordered_fit_score$total_log_likelihood,
    original$total_log_likelihood
  )
  expect_equal(
    reordered_count_score$total_log_likelihood,
    original$total_log_likelihood
  )
})

test_that("score_item_lca rejects malformed or incompatible inputs", {
  fit <- make_known_binary_fit()
  transition_counts <- matrix(
    c(9L, 12L, 3L, 16L),
    nrow = 1L,
    dimnames = list("item_a", c("x00", "x01", "x10", "x11"))
  )
  invalid_fit <- fit
  invalid_fit$params["gamma", ] <- -0.1
  negative_counts <- transition_counts
  negative_counts[1, 1] <- -1L
  other_item <- transition_counts
  rownames(other_item) <- "item_b"

  expect_error(score_item_lca(invalid_fit, transition_counts), "gamma")
  expect_error(score_item_lca(fit, negative_counts), "negative")
  expect_error(score_item_lca(fit, other_item), "same item names")
})

test_that("superseded item scoring APIs are not exported", {
  exports <- getNamespaceExports("guess")
  expect_false("perplexity_items" %in% exports)
  expect_false("cv_items" %in% exports)
  expect_false("log_likelihood" %in% exports)
})
