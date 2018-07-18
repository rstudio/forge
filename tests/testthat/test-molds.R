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

test_that("mold_choice() works properly", {
  expect_identical(mold_choice(2, 1:3), 2L)
  expect_identical(mold_choice(2, c(1, 2, 3)), 2)
  expect_identical(mold_choice("foo", c("foo", "bar")), "foo")
  expect_identical(mold_choice(NA, letters, allow_na = TRUE), NA)
  expect_identical(mold_choice(NULL, letters, allow_null = TRUE), NULL)

  expect_error(mold_choice(NA, letters), "`x` must not be NA\\.")
  expect_error(mold_choice(NULL, letters), "`x` must not be NULL\\.")
  expect_error(mold_choice("foo", c("bar", "baz")), "`x` must be one of bar, baz\\.")
  expect_error(mold_choice("foo", c(TRUE, FALSE)), "`choices` must be a vector of numbers or strings\\.")
})

test_that("mold_boolean() works properly", {
  expect_identical(mold_boolean(TRUE), TRUE)
  expect_identical(mold_boolean(FALSE), FALSE)

  expect_error(mold_boolean(0), "`x` must be a logical vector\\.")
  expect_error(mold_scalar_boolean(c(TRUE, FALSE)),
               "`x` must be of length 1, but is of length 2\\.")
})
