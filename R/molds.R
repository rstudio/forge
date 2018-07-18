#' @export
mold_integer <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  rlang::as_integer(x)
}

#' @export
mold_scalar_integer <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_integer(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @export
mold_double <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  rlang::as_double(x)
}

#' @export
mold_scalar_double <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_double(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @export
mold_character <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  rlang::as_character(x)
}

#' @export
mold_scalar_character <- function(x, allow_na = FALSE, allow_null = FALSE) {
  mold_character(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

