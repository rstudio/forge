verify_length_na <- function(x, n, allow_na) {
  if (is.null(x)) stop("`x` must not be NULL.", call. = FALSE)

  if (any(is.na(x))) {
    if (!allow_na) stop("`x` must not contain NAs.", call. = FALSE)
  }

  if (!is.null(n)) verify_length(x, n)

  NULL
}

verify_length <- function(x, n) {
  n <- if (!identical(length(n), 1L)) stop("`n` must be an integer.", call. = FALSE) else
    rlang::as_integer(n)

  length_x <- length(x)
  if (length_x != n) stop("`x` must be of length ", n, ", but is of length ", length_x, ".",
                          call. = FALSE)
}

as_nullable_list <- function(x) {
  if (is.null(x)) return(x) else rlang::as_list(x)
}
