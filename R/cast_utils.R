verify_length_na <- function(x, n, allow_na, id) {
  if (is.null(x)) stop(
    backticks(id), " must not be NULL.", call. = FALSE
  )

  if (any(is.na(x))) {
    if (!allow_na) stop(
      backticks(id), " must not contain NAs.", call. = FALSE
    )
  }

  if (!is.null(n)) verify_length(x, n, id = id)

  NULL
}

verify_length <- function(x, n, id) {
  n <- if (!identical(length(n), 1L)) stop("`n` must be an integer.", call. = FALSE) else
    rlang::as_integer(n)

  length_x <- length(x)
  if (length_x != n) stop(
    backticks(id), " must be of length ", n, ", but is of length ", length_x, ".",
    call. = FALSE
  )
}

as_nullable_list <- function(x, id, return_id) {
  if (is.null(x)) return(x) else maybe_set_id(rlang::as_list(x), id, return_id)
}

backticks <- function(.s) paste0("`", .s, "`")

get_id <- function(x) attr(x, "id", exact = TRUE)

resolve_id <- function(x, id) {
  expr <- rlang::quo_squash(x)
  x <- rlang::eval_tidy(x)

  # provided id has precedence
  id %||%
    # check for `forge_id` attribute
    attr(x, "forge_id", exact = TRUE) %||%
    # grab the user input for x
    (if (rlang::is_symbol(expr)) {
      id_string <- rlang::expr_text(expr)
      # if the expr is a single line and is not "." we'll use it as the id
      if (!grepl("\n", id_string) && !identical(id_string, ".")) id_string else NULL
    }) %||%
    "x"
}

maybe_set_id <- function(x, id, return_id) {
  if (return_id) structure(x, forge_id = id) else x
}
