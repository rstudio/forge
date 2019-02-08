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

as_nullable_list <- function(x) {
  id <- resolve_id(x, NULL)
  if (is.null(x)) return(x) else new_forge_stamped(rlang::as_list(x), id = id)
}

backticks <- function(.s) paste0("`", .s, "`")

new_forge_stamped <- function(x, id) {
  structure(x, id = id, class = c("forge_stamped", class(x)))
}

get_id <- function(x) attr(x, "id", exact = TRUE)

#' @export
Ops.forge_stamped <- function(e1, e2) `attributes<-`(NextMethod(), list())

#' @export
Math.forge_stamped <- function(x) `attributes<-`(NextMethod(), list())

#' @export
Complex.forge_stamped <- function(z) `attributes<-`(NextMethod(), list())

#' @export
print.forge_stamped <- function(x, ...) {
  print(c(x))
}

resolve_id <- function(x, id) {
  expr <- rlang::quo_squash(x)
  x <- rlang::eval_tidy(x)

  # provided id has precedence
  id %||%
    # if x is the output of another forge function, get the id
    (if (inherits(x, "forge_stamped")) get_id(x)) %||%
    # grab the user input for x
    (if (rlang::is_symbol(expr)) {
      id_string <- rlang::expr_text(expr)
      # if the expr is a single line and is not "." we'll use it as the id
      if (!grepl("\n", id_string) && !identical(id_string, ".")) id_string else NULL
    }) %||%
    "x"
}
