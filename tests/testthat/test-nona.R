test_that("nona works correctly", {
  x <- c(NA, 1, 0)
  expect_equal(nona(x), c("d", "1", "0"))
  expect_equal(nona(x, na_as = "missing"), c("1", "0"))
  expect_error(
    nona(x, na_as = "missing", missing_action = "error"),
    "Structural missing"
  )
})
