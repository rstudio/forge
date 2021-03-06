#' Ensure Conditions on a Value
#'
#' Checks that the input value satisfies specified conditions
#'
#' @param x The value to be checked.
#' @param ... Conditions to be checked; should be functions that return TRUE/FALSE.
#' @param allow_null Whether to allow null input.
#' @param l Lower bound for the inequality condition.
#' @param u Upper bound for the inequality condition.
#' @param id Name given to the input to aid the user in identifying the bad value.
#' @param return_id Whether to return the ID as an attribute. This should only be set to \code{TRUE}
#'  when piping the result to another forge function. Defaults to \code{FALSE}.
#' @export
certify <- function(x, ..., allow_null = FALSE, id = NULL, return_id = FALSE) {
  id <- resolve_id(rlang::enquo(x), id)
  if (is.null(x)) {
    if (!allow_null) {
      stop(backticks(id), " must not be NULL.", call. = FALSE)
    } else {
      return(NULL)
    }
  }

  quos <- rlang::enquos(...)

  for (quo in quos) {
    condition <- rlang::as_function(rlang::eval_tidy(quo))
    satisfied <- condition(x)

    if (!rlang::is_scalar_logical(satisfied)) stop(
      "`",
      rlang::quo_text(quo),
      "` does not evaluate to a scalar logical for ",
      backticks(id),
      ".",
      call. = FALSE
    )

    if (!satisfied) stop(
      "Condition `",
      rlang::quo_text(quo),
      "` not satisfied for ",
      backticks(id),
      ".",
      call. = FALSE
    )
  }

  maybe_set_id(x, id, return_id)
}

#' @rdname certify
#' @export
gt <- function(l) {
  force(l)
  function(x) all(x > l)
}

#' @rdname certify
#' @export
gte <- function(l) {
  force(l)
  function(x) all(x >= l)
}

#' @rdname certify
#' @export
lt <- function(u) {
  force(u)
  function(x) all(x < u)
}

#' @rdname certify
#' @export
lte <- function(u) {
  force(u)
  function(x) all(x <= u)
}

#' @rdname certify
#' @param incl_lower Whether to include the left endpoint.
#' @param incl_upper Whether to include the right endpoint.
#' @export
bounded <- function(l = NULL, u = NULL, incl_lower = TRUE, incl_upper = TRUE) {
  if (is.null(l) && is.null(u)) stop("At least one of `l` or `u` must be specified.", call. = FALSE)

  lower_bound <- if (!is.null(l)) {
    if (incl_lower) gte(l) else gt(l)
  } else {
    function() TRUE
  }

  upper_bound <- if (!is.null(u)) {
    if (incl_upper) lte(u) else lt(u)
  } else {
    function() TRUE
  }

  function(x) lower_bound(x) && upper_bound(x)
}
