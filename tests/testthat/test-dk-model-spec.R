# The DK model as specified in Cor and Sood, equation (2).
#
# The likelihood used to implement a different model: it kept the know->guess
# and know->don't-know classes that the paper's identifying assumption sets to
# zero, dropped the don't-know->guess and don't-know->know classes that the
# paper keeps, and then reused the survivors in the d0 and d1 cells. Two things
# followed, and both are asserted here.
#
#   * The nine cell probabilities did not sum to 1. They summed to anywhere
#     between 1.02 and 2.29, so the objective carried a spurious -N log S(theta)
#     term with no statistical content.
#   * The seven parameters were not identified. Fed exact model-implied counts
#     with no sampling noise, the old estimator returned parameters off by up to
#     0.041 while the log-likelihood differed by 0.0009 in 1.7 million.
#
# The paper's model is identified in closed form and over-identified by one
# degree of freedom.

param_names <- c("gg", "gk", "gd", "kk", "dg", "dk", "dd")

random_lambda <- function() {
  l <- stats::rgamma(7, 1)
  stats::setNames(l / sum(l), param_names)
}

# ---------------------------------------------------------------------------
# The cell probabilities are a distribution
# ---------------------------------------------------------------------------

test_that("DK cell probabilities sum to exactly 1", {
  set.seed(11)
  for (i in seq_len(300)) {
    l <- random_lambda()
    g <- stats::runif(1, 0.01, 0.99)
    probs <- dk_cell_probs(l[1], l[2], l[3], l[4], l[5], l[6], l[7], g)
    expect_equal(sum(probs), 1)
    expect_true(all(probs >= 0))
  }
})

test_that("no-DK cell probabilities sum to exactly 1", {
  set.seed(12)
  for (i in seq_len(300)) {
    l <- stats::rgamma(3, 1)
    l <- l / sum(l)
    g <- stats::runif(1, 0.01, 0.99)
    probs <- nodk_cell_probs(l[1], l[2], l[3], g)
    expect_equal(sum(probs), 1)
    expect_true(all(probs >= 0))
  }
})

# ---------------------------------------------------------------------------
# The structural zeros the paper's assumption creates
# ---------------------------------------------------------------------------

test_that("someone who knows the item cannot answer wrongly later", {
  # No class has pre = know and post = anything but correct, so the only
  # contribution to cells 10 and 1d is from guessers, whose pre-test correct
  # answer was luck. Cell 10 must therefore be exactly gamma(1-gamma)*gg and
  # cell 1d exactly gamma*gd -- neither may contain a know-state parameter.
  g <- 0.3
  l <- stats::setNames(c(0.25, 0.15, 0.10, 0.20, 0.08, 0.12, 0.10), param_names)
  probs <- dk_cell_probs(l[1], l[2], l[3], l[4], l[5], l[6], l[7], g)

  expect_equal(unname(probs[CELL_10_DK]), g * (1 - g) * l[["gg"]])
  expect_equal(unname(probs[CELL_1D]), g * l[["gd"]])

  # kk moves cell 11 and nothing else.
  bumped <- l
  bumped[["kk"]] <- bumped[["kk"]] + 0.05
  bumped[["gg"]] <- bumped[["gg"]] - 0.05
  probs2 <- dk_cell_probs(
    bumped[1], bumped[2], bumped[3], bumped[4],
    bumped[5], bumped[6], bumped[7], g
  )
  expect_true(probs2[CELL_D0] == probs[CELL_D0])
  expect_true(probs2[CELL_D1] == probs[CELL_D1])
})

test_that("the d0 and d1 cells are driven by the don't-know classes", {
  g <- 0.4
  l <- stats::setNames(c(0.25, 0.15, 0.10, 0.20, 0.08, 0.12, 0.10), param_names)
  probs <- dk_cell_probs(l[1], l[2], l[3], l[4], l[5], l[6], l[7], g)

  expect_equal(unname(probs[CELL_D0]), (1 - g) * l[["dg"]])
  expect_equal(unname(probs[CELL_D1]), g * l[["dg"]] + l[["dk"]])
  expect_equal(unname(probs[CELL_DD]), l[["dd"]])
})

# ---------------------------------------------------------------------------
# Identification
# ---------------------------------------------------------------------------

test_that("the DK model is identified in closed form", {
  # gamma / (1 - gamma) is x10 / x00; every lambda follows from there. If this
  # inversion works then no two parameter vectors imply the same cells.
  set.seed(13)
  for (i in seq_len(200)) {
    l <- random_lambda()
    g <- stats::runif(1, 0.05, 0.95)
    p <- dk_cell_probs(l[1], l[2], l[3], l[4], l[5], l[6], l[7], g)

    g_hat <- p[CELL_10_DK] / (p[CELL_00_DK] + p[CELL_10_DK])
    gg <- p[CELL_00_DK] / (1 - g_hat)^2
    gk <- p[CELL_01_DK] / (1 - g_hat) - g_hat * gg
    gd <- p[CELL_0D] / (1 - g_hat)
    kk <- p[CELL_11_DK] - g_hat^2 * gg - g_hat * gk
    dg <- p[CELL_D0] / (1 - g_hat)
    dk <- p[CELL_D1] - g_hat * dg
    dd <- p[CELL_DD]

    expect_equal(unname(g_hat), g)
    expect_equal(unname(c(gg, gk, gd, kk, dg, dk, dd)), unname(l))
  }
})

test_that("the DK model leaves one over-identifying restriction", {
  # x1d / x0d and x10 / x00 are both gamma / (1 - gamma). This is the single
  # degree of freedom the goodness of fit test spends.
  set.seed(14)
  for (i in seq_len(200)) {
    l <- random_lambda()
    g <- stats::runif(1, 0.05, 0.95)
    p <- dk_cell_probs(l[1], l[2], l[3], l[4], l[5], l[6], l[7], g)
    expect_equal(
      unname(p[CELL_1D] / p[CELL_0D]),
      unname(p[CELL_10_DK] / p[CELL_00_DK])
    )
  }
})

test_that("the estimator recovers the truth from exact model-implied counts", {
  # With no sampling noise the answer is a matter of identification alone. The
  # previous implementation was off by up to 0.041 here no matter how large the
  # counts, because its parameters were observationally equivalent.
  truth <- stats::setNames(
    c(0.25, 0.15, 0.10, 0.20, 0.08, 0.12, 0.10), param_names
  )
  gamma <- 0.30

  probs <- dk_cell_probs(
    truth[1], truth[2], truth[3], truth[4],
    truth[5], truth[6], truth[7], gamma
  )
  tm <- matrix(probs * 1e7,
    nrow = 1,
    dimnames = list("item1", TRANSMAT_COLS_DK)
  )

  fit <- fit_item_lca_counts(tm)
  est <- fit$params[, 1]

  expect_equal(unname(est), unname(c(truth, gamma)), tolerance = 1e-3)
  expect_equal(unname(fit$learning[1]), truth[["gk"]] + truth[["dk"]],
    tolerance = 1e-3
  )
})

# ---------------------------------------------------------------------------
# The simulator implements the same model the likelihood scores
# ---------------------------------------------------------------------------

test_that("simulate_lca_dk can produce learning from confessed ignorance", {
  # The simulator used to draw know->guess and know->dk classes and had no
  # don't-know->know class at all, so it could not generate half of what the
  # paper defines as learning.
  sim <- simulate_lca_dk(
    n = 4000, n_items = 1,
    gg = 0, gk = 0, gd = 0, kk = 0, dg = 0, dk = 1, dd = 0,
    gamma = 0.25, seed = 77
  )
  expect_true(all(sim$pre[[1]] == "d"))
  expect_true(all(sim$post[[1]] == "1"))
})

test_that("simulate_lca_dk never shows a knower answering wrongly later", {
  sim <- simulate_lca_dk(n = 4000, n_items = 1, gamma = 0.25, seed = 78)
  tm <- count_item_transitions(sim$pre, sim$post)

  # Cell 10 can only come from a guesser lucky then unlucky, so it must be far
  # smaller than cell 11; the point is that no structural zero is violated.
  expect_true(all(tm >= 0))
  expect_equal(sum(tm[1, ]), 4000)
})

test_that("estimates converge on the truth as the simulated sample grows", {
  truth <- c(
    gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.20,
    dg = 0.08, dk = 0.12, dd = 0.10
  )
  gamma <- 0.30

  errs <- vapply(c(2000, 50000), function(n) {
    sim <- simulate_lca_dk(
      n = n, n_items = 1,
      gg = truth[["gg"]], gk = truth[["gk"]], gd = truth[["gd"]],
      kk = truth[["kk"]], dg = truth[["dg"]], dk = truth[["dk"]],
      dd = truth[["dd"]], gamma = gamma, seed = 31
    )
    tm <- count_item_transitions(sim$pre, sim$post)
    est <- fit_item_lca_counts(tm)$params[, 1]
    max(abs(est - c(truth, gamma = gamma)))
  }, numeric(1))

  expect_lt(errs[2], errs[1])
  expect_lt(errs[2], 0.02)
})

# ---------------------------------------------------------------------------
# Goodness of fit degrees of freedom
# ---------------------------------------------------------------------------

test_that("the goodness of fit test uses the surviving degrees of freedom", {
  sim <- simulate_lca_dk(n = 8000, n_items = 1, gamma = 0.25, seed = 88)
  tm <- count_item_transitions(sim$pre, sim$post)
  fit <- fit_item_lca_counts(tm)

  gof <- assess_item_lca_fit(fit, sim$pre, sim$post)

  stat <- gof$statistics[1, "statistic"]
  p <- gof$statistics[1, "p_value"]

  expect_false(is.na(p))

  # p must match 1 df, not the 8 the old code charged.
  p_df1 <- stats::pchisq(stat, df = 1, lower.tail = FALSE)
  p_df8 <- stats::pchisq(stat, df = 8, lower.tail = FALSE)

  expect_equal(unname(p), p_df1, tolerance = 1e-12)
  expect_gt(abs(p_df8 - p_df1), 0.02)
  expect_lt(abs(unname(p) - p_df1), abs(unname(p) - p_df8))
})

test_that("the saturated no-DK model reports no goodness of fit test", {
  pre <- data.frame(item1 = rep(c(1, 0, 0, 1), 50))
  pst <- data.frame(item1 = rep(c(1, 1, 0, 1), 50))
  tm <- count_item_transitions(pre, pst)
  fit <- fit_item_lca_counts(tm)

  gof <- assess_item_lca_fit(fit, pre, pst)

  expect_true(all(is.na(gof$statistics$statistic)))
  expect_true(all(is.na(gof$statistics$p_value)))
})
