#' Mold values into shape
#'
#' These functions verify and attempt to coerce values into the specified types and shapes. If they
#'   are unsuccessful in the coercion, an error is thrown.
#'
#' @param x A vector.
#' @param n The required length of the vector. If \code{NULL}, the length is not checked.
#' @param allow_na Whether to allow \code{NA}s in the vector.
#' @param allow_null Whether to allow \code{NULL}
#' @name mold

#' @rdname mold
#' @export
mold_integer <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  rlang::as_integer(x)
}

#' @rdname mold
#' @export
mold_scalar_integer <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_integer(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @rdname mold
#' @export
mold_double <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  rlang::as_double(x)
}

#' @rdname mold
#' @export
mold_scalar_double <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_double(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @rdname mold
#' @export
mold_character <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  rlang::as_character(x)
}

#' @rdname mold
#' @export
mold_scalar_character <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_character(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

