context("certifications")

test_that("certify() works with arbitrary functions", {
  expect_identical(certify(42, ~ .x > 41), 42)
  expect_error(certify(42, ~ .x > 42), "Condition `~\\.x > 42` not satisfied\\.")
  expect_error(certify(42, function(x) x > 42), "Condition `function\\(x\\) x > 42` not satisfied\\.")
})

test_that("certify() works with multiple conditions", {
  expect_identical(certify(42, ~ .x > 41, ~ .x < 43), 42)
  expect_error(certify(42, ~ .x > 41, ~ .x < 39), "Condition `~\\.x < 39` not satisfied\\.")
  expect_error(certify(42, ~ .x < 39, ~ .x > 43), "Condition `~\\.x < 39` not satisfied\\.")
})

test_that("certify() helper functions work correctly", {
  expect_identical(certify(42, between(41, 43)), 42)
  expect_error(certify(42, between(43, 44)), "Condition `between\\(43, 44\\)` not satisfied\\.")
  expect_identical(certify(42, between(42, 43)), 42)
  expect_error(certify(42, between(42, 43, strict = "both")), "Condition `between\\(42, 43, strict = \"both\"\\)` not satisfied\\.")
  expect_identical(certify(42, lt(43)), 42)
  expect_error(certify(42, lt(42)), "Condition `lt\\(42\\)` not satisfied\\.")
  expect_identical(certify(42, lte(42)), 42)
  expect_identical(certify(42, gt(41)), 42)
  expect_error(certify(42, gt(42)), "Condition `gt\\(42\\)` not satisfied\\.")
  expect_identical(certify(42, gte(42)), 42)

})
