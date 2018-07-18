context("molds")

test_that("mold_integer() works properly", {
  expect_identical(mold_integer(c(1, 2)), 1:2)
  expect_identical(mold_integer(42), 42L)
  expect_error(mold_integer(NULL), "`x` must not be NULL\\.")
  expect_identical(mold_integer(NULL, allow_null = TRUE), NULL)
  expect_error(mold_integer(1:3, 2), "`x` must be of length 2, but is of length 3\\.")
  expect_error(mold_integer(1.2), "Can't convert a fractional double vector to an integer vector")
  expect_error(mold_integer(NA), "`x` must not contain NAs\\.")
  expect_identical(mold_integer(c(2, NA), allow_na = TRUE), c(2L, NA))
})

test_that("mold_scalar_integer() works properly", {
  expect_error(mold_scalar_integer(1:3), "`x` must be of length 1, but is of length 3.")
})
