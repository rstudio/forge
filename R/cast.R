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
#' @param id Name given to the input to aid the user in identifying the bad value.
#' @param return_id Whether to return the ID as an attribute. This should only be set to \code{TRUE}
#'  when piping the result to another forge function. Defaults to \code{FALSE}.
#'
#' @importFrom rlang %||%
#'
#' @examples
#' # Cast a double vector to integer
#' cast_integer(c(1, 2))
#'
#' # Cast a numeric to a string
#' cast_string(4.5)
#'
#' # Cast an integer vector to a list of doubles
#' cast_double_list(1:4)
#'
#' @name cast
NULL

#' @rdname cast
#' @export
cast_integer <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)

  if (is.null(x) && allow_null) return(NULL)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_int(x) else x

  verify_length_na(x, n, allow_na, id = id)

  if (!rlang::is_integerish(x)) stop(
    backticks(id), " cannot be cast to an integer vector.",
    call. = FALSE
  )

  maybe_set_id(rlang::as_integer(x), id, return_id)
}

#' @rdname cast
#' @export
cast_scalar_integer <- function(x, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_integer(x, n = 1, allow_na = allow_na, allow_null = allow_null, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_integer <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_integer(x, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_scalar_integer <- function(x, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_integer(x, n = 1, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_integer_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_integer(x, n, allow_na, allow_null, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_integer_list <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_nullable_integer(x, n, allow_na, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_double <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)

  if (is.null(x) && allow_null) return(NULL)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_dbl(x) else x
  verify_length_na(x, n, allow_na, id = id)

  if (!rlang::is_double(x) && !rlang::is_integerish(x)) stop(
    backticks(id), " cannot be cast to a double vector.",
    call. = FALSE
  )

  maybe_set_id(rlang::as_double(x), id, return_id)
}

#' @rdname cast
#' @export
cast_scalar_double <- function(x, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_double(x, n = 1, allow_na = allow_na, allow_null = allow_null, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_double <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_double(x, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_scalar_double <- function(x, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_double(x, n = 1, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_double_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_double(x, n, allow_na, allow_null, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_double_list <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_nullable_double(x, n, allow_na, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_character <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  if (is.null(x) && allow_null) return(NULL)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_chr(x) else x
  verify_length_na(x, n, allow_na, id = id)

  maybe_set_id(as.character(x), id, return_id)
}

#' @rdname cast
#' @export
cast_scalar_character <- function(x, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_character(x, n = 1, allow_na = allow_na, allow_null = allow_null, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_character <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_character(x, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_scalar_character <- function(x, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_character(x, n = 1, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_character_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_character(x, n, allow_na, allow_null, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_character_list <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_nullable_character(x, n, allow_na, id = NULL), id = id, return_id = return_id)
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
cast_logical <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  if (is.null(x) && allow_null) return(NULL) else verify_length_na(x, n, allow_na, id)
  x <- if (rlang::is_bare_list(x)) rlang::flatten_lgl(x) else x
  if (!is.logical(x)) stop(backticks(id), " must be a logical vector.")
  maybe_set_id(x, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_scalar_logical <- function(x, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_logical(x, n = 1, allow_na = allow_na, allow_null = allow_null, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_logical <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_logical(x, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_scalar_logical <- function(x, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast_logical(x, n = 1, allow_na = allow_na, allow_null = TRUE, id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_logical_list <- function(x, n = NULL, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_logical(x, n, allow_na, allow_null, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_nullable_logical_list <- function(x, n = NULL, allow_na = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  as_nullable_list(cast_nullable_logical(x, n, allow_na, id = NULL), id = id, return_id = return_id)
}

#' @rdname cast
#' @export
cast_choice <- function(x, choices, allow_na = FALSE, allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  cast <- switch(rlang::type_of(choices),
                 integer = cast_scalar_integer,
                 double = cast_scalar_double,
                 string = cast_scalar_character,
                 character = cast_scalar_character)
  if (is.null(cast)) stop("`choices` must be a vector of numbers or strings.",
                          call. = FALSE)

  if (rlang::is_na(x)) {
    if (allow_na) return(x) else stop(backticks(id), " must not be NA.", call. = FALSE)
  }

  if (rlang::is_null(x)) {
    if (allow_null) return(NULL) else stop(backticks(id), " must not be NULL.", call. = FALSE)
  }

  casted <- cast(x)

  if (casted %in% choices) maybe_set_id(casted, id, return_id) else
    stop(backticks(id), " must be one of ", paste0(choices, collapse = ", "), ".",
         call. = FALSE)
}
