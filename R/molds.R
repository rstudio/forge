#' Cast values into shape
#'
#' These functions verify and attempt to coerce values into the specified types and shapes. If they
#'   are unsuccessful in the coercion, an error is thrown.
#'
#' @param x A vector.
#' @param n The required length of the vector. If \code{NULL}, the length is not checked.
#' @param allow_na Whether to allow \code{NA}s in the vector.
#' @param allow_null Whether to allow \code{NULL}.
#' @param choices A character, numeric, or integer vector of allowed values.
#' @name cast

#' @rdname cast
#' @export
cast_integer <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_int(x) else x
  verify_length_na(x, n, allow_na)
  rlang::as_integer(x)
}

#' @rdname cast
#' @export
cast_scalar_integer <- function(x, allow_na = FALSE, allow_null = FALSE) {
  cast_integer(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @rdname cast
#' @export
cast_nullable_integer <- function(x, n = NULL, allow_na = FALSE) {
  cast_integer(x, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_nullable_scalar_integer <- function(x, allow_na = FALSE) {
  cast_integer(x, n = 1, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_integer_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  as_nullable_list(cast_integer(x, n, allow_na, allow_null))
}

#' @rdname cast
#' @export
cast_nullable_integer_list <- function(x, n = NULL, allow_na = FALSE) {
  as_nullable_list(cast_nullable_integer(x, n, allow_na))
}

#' @rdname cast
#' @export
cast_double <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_dbl(x) else x
  verify_length_na(x, n, allow_na)
  rlang::as_double(x)
}

#' @rdname cast
#' @export
cast_scalar_double <- function(x, allow_na = FALSE, allow_null = FALSE) {
  cast_double(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @rdname cast
#' @export
cast_nullable_double <- function(x, n = NULL, allow_na = FALSE) {
  cast_double(x, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_nullable_scalar_double <- function(x, allow_na = FALSE) {
  cast_double(x, n = 1, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_double_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  as_nullable_list(cast_double(x, n, allow_na, allow_null))
}

#' @rdname cast
#' @export
cast_nullable_double_list <- function(x, n = NULL, allow_na = FALSE) {
  as_nullable_list(cast_nullable_double(x, n, allow_na))
}

#' @rdname cast
#' @export
cast_character <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_chr(x) else x
  verify_length_na(x, n, allow_na)
  rlang::as_character(as.character(x))
}

#' @rdname cast
#' @export
cast_scalar_character <- function(x, allow_na = FALSE, allow_null = FALSE) {
  cast_character(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @rdname cast
#' @export
cast_nullable_character <- function(x, n = NULL, allow_na = FALSE) {
  cast_character(x, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_nullable_scalar_character <- function(x, allow_na = FALSE) {
  cast_character(x, n = 1, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_character_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  as_nullable_list(cast_character(x, n, allow_na, allow_null))
}

#' @rdname cast
#' @export
cast_nullable_character_list <- function(x, n = NULL, allow_na = FALSE) {
  as_nullable_list(cast_nullable_character(x, n, allow_na))
}

#' @rdname cast
#' @export
cast_string <- cast_scalar_character

#' @rdname cast
#' @export
cast_nullable_string <- cast_nullable_scalar_character

#' @rdname cast
#' @export
cast_string_list <- cast_character_list

#' @rdname cast
#' @export
cast_nullable_string_list <- cast_nullable_character_list

#' @rdname cast
#' @export
cast_logical <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(x) else verify_length_na(x, n, allow_na)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_lgl(x) else x
  if (!is.logical(x)) stop("`x` must be a logical vector.")
  x
}

#' @rdname cast
#' @export
cast_scalar_logical <- function(x, allow_na = FALSE, allow_null = FALSE) {
  cast_logical(x, n = 1, allow_na = allow_na, allow_null = allow_null)
}

#' @rdname cast
#' @export
cast_nullable_logical <- function(x, n = NULL, allow_na = FALSE) {
  cast_logical(x, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_nullable_scalar_logical <- function(x, allow_na = FALSE) {
  cast_logical(x, n = 1, allow_na = allow_na, allow_null = TRUE)
}

#' @rdname cast
#' @export
cast_logical_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE) {
  as_nullable_list(cast_logical(x, n, allow_na, allow_null))
}

#' @rdname cast
#' @export
cast_nullable_logical_list <- function(x, n = NULL, allow_na = FALSE) {
  as_nullable_list(cast_nullable_logical(x, n, allow_na))
}

#' @rdname cast
#' @export
cast_choice <- function(x, choices, allow_na = FALSE, allow_null = FALSE) {
  cast <- switch(rlang::type_of(choices),
                 integer = cast_scalar_integer,
                 double = cast_scalar_double,
                 string = cast_scalar_character,
                 character = cast_scalar_character)
  if (is.null(cast)) stop("`choices` must be a vector of numbers or strings.",
                          call. = FALSE)

  if (rlang::is_na(x)) {
    if (allow_na) return(x) else stop("`x` must not be NA.", call. = FALSE)
  }

  if (rlang::is_null(x)) {
    if (allow_null) return(x) else stop("`x` must not be NULL.", call. = FALSE)
  }

  casted <- cast(x)

  if (casted %in% choices) casted else
    stop("`x` must be one of ", paste0(choices, collapse = ", "), ".",
         call. = FALSE)
}
