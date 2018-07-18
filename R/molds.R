#' @export
mold_integer <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(x) else stop("`x` must not be NULL.", call. = FALSE)
  }

  if (any(is.na(x))) {
    if (!allow_na) stop("`x` must not contain NAs.", call. = FALSE)
  }

  if (!is.null(n)) {
    length_x <- length(x)
    if (length_x != n) stop("`x` must be of length ", n, ", but is of length ", length_x, ".",
                            call. = FALSE)
  }

  rlang::as_integer(x)
}

#' @export
mold_scalar_integer <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_integer(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}
