context("molds")

test_that("mold_integer() works properly", {
  expect_identical(mold_integer(c(1, 2)), 1:2)
  expect_identical(mold_integer(42), 42L)
  expect_identical(mold_integer(NULL, allow_null = TRUE), NULL)
  expect_identical(mold_integer(c(2, NA), allow_na = TRUE), c(2L, NA))

  expect_error(mold_integer(NULL), "`x` must not be NULL\\.")
  expect_error(mold_integer(1:3, 2), "`x` must be of length 2, but is of length 3\\.")
  expect_error(mold_integer(1.2), "Can't convert a fractional double vector to an integer vector")
  expect_error(mold_integer(NA), "`x` must not contain NAs\\.")
  expect_error(mold_integer(1:2, c(1, 2)), "`n` must be an integer\\.")

  expect_error(mold_scalar_integer(1:3), "`x` must be of length 1, but is of length 3\\.")
})

test_that("mold_double() works properly", {
  expect_identical(mold_double(1:2), c(1, 2))
  expect_identical(mold_double(42.5), 42.5)
  expect_identical(mold_double(c(2.4, NA), allow_na = TRUE), c(2.4, NA))

  expect_error(mold_double(NULL), "`x` must not be NULL\\.")
  expect_error(mold_double(1:3, 2), "`x` must be of length 2, but is of length 3\\.")
  expect_error(mold_double(NA), "`x` must not contain NAs\\.")

  expect_error(mold_scalar_double(c(1, 2)), "`x` must be of length 1, but is of length 2\\.")
})

test_that("mold_character() works properly", {
  expect_identical(mold_character(c("foo", "bar")), c("foo", "bar"))
  expect_identical(mold_scalar_character("foo"), "foo")

  expect_error(mold_scalar_character(c("foo", "bar")), "`x` must be of length 1, but is of length 2\\.")
})
