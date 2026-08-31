make_known_dk_fit <- function() {
  params <- matrix(
    c(0.10, 0.20, 0.10, 0.40, 0.05, 0.05, 0.10, 0.25),
    ncol = 1L,
    dimnames = list(
      c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma"),
      "item_a"
    )
  )
  structure(
    list(
      params = params,
      learning = c(item_a = 0.25),
      n_items = 1L,
      n_obs = 1600L,
      model_type = "dk"
    ),
    class = "guess_fit"
  )
}

make_dk_responses <- function(counts, item_name) {
  cells <- sub("^x", "", names(counts))
  list(
    pre = as.data.frame(
      setNames(list(rep(substr(cells, 1L, 1L), counts)), item_name),
      stringsAsFactors = FALSE
    ),
    post = as.data.frame(
      setNames(list(rep(substr(cells, 2L, 2L), counts)), item_name),
      stringsAsFactors = FALSE
    )
  )
}

test_that("assess_item_lca_fit returns an exact DK Pearson diagnostic", {
  fit <- make_known_dk_fit()
  counts <- c(
    x00 = 90L, x01 = 270L, x0d = 120L,
    x10 = 30L, x11 = 730L, x1d = 40L,
    xd0 = 60L, xd1 = 100L, xdd = 160L
  )
  responses <- make_dk_responses(counts, "item_a")

  assessment <- assess_item_lca_fit(fit, responses$pre, responses$post)

  expect_s3_class(assessment, "guess_gof")
  expect_equal(assessment$statistics["item_a", "statistic"], 0)
  expect_equal(assessment$statistics["item_a", "df"], 1L)
  expect_equal(assessment$statistics["item_a", "p_value"], 1)
  expect_equal(assessment$observed["item_a", ], counts)
  expect_equal(assessment$expected["item_a", ], counts)
  expect_equal(
    assessment$residuals["item_a", ],
    stats::setNames(rep(0, length(counts)), names(counts))
  )
  expect_output(print(assessment), "Item-level Pearson")
})

test_that("assess_item_lca_fit matches fitted parameters and responses by item name", {
  sim <- simulate_lca_dk(n = 3000, n_items = 2, gamma = c(0.2, 0.4), seed = 183)
  fit <- fit_item_lca(sim$pre, sim$post)
  reordered_fit <- fit
  reordered_fit$params <- fit$params[, 2:1, drop = FALSE]

  original <- assess_item_lca_fit(fit, sim$pre, sim$post)
  reordered <- assess_item_lca_fit(reordered_fit, sim$pre, sim$post)
  reordered_post <- assess_item_lca_fit(fit, sim$pre, sim$post[, 2:1])

  expect_equal(reordered$statistics, original$statistics, tolerance = 1e-12)
  expect_equal(reordered$expected, original$expected, tolerance = 1e-12)
  expect_equal(reordered_post$statistics, original$statistics, tolerance = 1e-12)
})

test_that("assess_item_lca_fit rejects malformed fits and incompatible responses", {
  fit <- make_known_dk_fit()
  responses <- make_dk_responses(
    c(x00 = 90L, x01 = 270L, x0d = 120L, x10 = 30L, x11 = 730L,
      x1d = 40L, xd0 = 60L, xd1 = 100L, xdd = 160L),
    "item_a"
  )
  invalid_gamma <- fit
  invalid_gamma$params["gamma", ] <- -0.1
  invalid_weights <- fit
  invalid_weights$params["gg", ] <- 0.20

  expect_error(
    assess_item_lca_fit(invalid_gamma, responses$pre, responses$post),
    "gamma"
  )
  expect_error(
    assess_item_lca_fit(invalid_weights, responses$pre, responses$post),
    "sum to 1"
  )
  expect_error(
    assess_item_lca_fit(fit, responses$pre, data.frame(other_item = responses$post$item_a)),
    "same item names"
  )
})

test_that("assess_item_lca_fit reports binary saturation without a p-value", {
  sim <- simulate_lca(n = 1000, n_items = 1, seed = 954)
  fit <- fit_item_lca(sim$pre, sim$post)

  assessment <- assess_item_lca_fit(fit, sim$pre, sim$post)

  expect_equal(assessment$statistics[1, "df"], 0L)
  expect_true(is.na(assessment$statistics[1, "statistic"]))
  expect_true(is.na(assessment$statistics[1, "p_value"]))
  expect_equal(dim(assessment$expected), c(1L, 4L))
})

test_that("the superseded positional goodness-of-fit APIs are absent", {
  exports <- getNamespaceExports("guess")
  expect_false("fit_model" %in% exports)
  expect_false("fit_dk" %in% exports)
  expect_false("fit_nodk" %in% exports)
})
