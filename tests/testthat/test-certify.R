context("certify")

test_that("certify() works with arbitrary functions", {
  expect_identical(c(certify(42, ~ .x > 41)), 42)
  expect_error(certify(42, ~ .x > 42), "Condition `~\\.x > 42` not satisfied for `\\.x`\\.")
  expect_error(certify(42, function(x) x > 42), "Condition `function\\(x\\) x > 42` not satisfied for `\\.x`\\.")
})

test_that("certify() works with multiple conditions", {
  expect_identical(c(certify(42, ~ .x > 41, ~ .x < 43)), 42)
  expect_error(certify(42, ~ .x > 41, ~ .x < 39), "Condition `~\\.x < 39` not satisfied for `\\.x`\\.")
  expect_error(certify(42, ~ .x < 39, ~ .x > 43), "Condition `~\\.x < 39` not satisfied for `\\.x`\\.")
})

test_that("certify() helper functions work for scalar values", {
  expect_identical(c(certify(42, between(41, 43))), 42)
  expect_error(certify(42, between(43, 44)), "Condition `between\\(43, 44\\)` not satisfied for `\\.x`\\.")
  expect_identical(c(certify(42, between(42, 43))), 42)
  expect_error(certify(42, between(42, 43, strict = "both")), "Condition `between\\(42, 43, strict = \"both\"\\)` not satisfied for `\\.x`\\.")
  expect_identical(c(certify(42, lt(43))), 42)
  expect_error(certify(42, lt(42)), "Condition `lt\\(42\\)` not satisfied for `\\.x`\\.")
  expect_identical(c(certify(42, lte(42))), 42)
  expect_identical(c(certify(42, gt(41))), 42)
  expect_error(certify(42, gt(42)), "Condition `gt\\(42\\)` not satisfied for `\\.x`\\.")
  expect_identical(c(certify(42, gte(42))), 42)
})

test_that("certify() helper functions work for vector values", {
  expect_identical(c(certify(1:2, gt(0))), 1:2)
  expect_identical(c(certify(1:2, gte(1:2))), 1:2)
  expect_error(certify(1:2, gt(1)), "Condition `gt\\(1\\)` not satisfied for `\\.x`\\.")
  expect_identical(c(certify(1:5, between(1, 5))), 1:5)
  expect_error(certify(1:5, between(1, 5, strict = "lower")), "Condition `between\\(1, 5, strict = \"lower\"\\)` not satisfied for `\\.x`\\.")
})

test_that(".id gets carried through forge pipes", {
  library(magrittr)
  foo <- 42
  expect_error(
    cast_scalar_integer(foo) %>%
      certify(gt(42)),
    "Condition `gt\\(42\\)` not satisfied for `foo`\\."
  )

  expect_error(
    cast_scalar_integer(foo, .id = "bar") %>%
      certify(gt(42)),
    "Condition `gt\\(42\\)` not satisfied for `bar`\\."
  )
})
