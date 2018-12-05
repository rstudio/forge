#' Ensure Conditions on a Value
#'
#' Checks that the input value satisfies specified conditions
#'
#' @param .x The value to be checked.
#' @param ... Conditions to be checked; should be functions that return TRUE/FALSE.
#' @param .l Lower bound for the inequality condition.
#' @param .u Upper bound for the inequality condition.
#' @export
certify <- function(.x, ...) {
  quos <- rlang::enquos(...)

  for (quo in quos) {
    condition <- rlang::as_function(rlang::eval_tidy(quo))
    satisfied <- condition(.x)

    if (!rlang::is_scalar_logical(satisfied)) stop(
      "`",
      rlang::quo_text(quo),
      "` does not evaluate to a scalar logical for the given input."
    )

    if (!satisfied) stop(
      "Condition `",
      rlang::quo_text(quo),
      "` not satisfied.",
      call. = FALSE
    )
  }

  .x
}

#' @rdname certify
#' @export
gt <- function(.l) {
  force(.l)
  .l <- cast_scalar_double(.l)
  function(..x) cast_scalar_double(..x) > .l
}

#' @rdname certify
#' @export
gte <- function(.l) {
  force(.l)
  .l <- cast_scalar_double(.l)
  function(..x) cast_scalar_double(..x) >= .l
}

#' @rdname certify
#' @export
lt <- function(.u) {
  force(.u)
  .u <- cast_scalar_double(.u)
  function(..x) cast_scalar_double(..x) < .u
}

#' @rdname certify
#' @export
lte <- function(.u) {
  force(.u)
  .u <- cast_scalar_double(.u)
  function(..x) cast_scalar_double(..x) <= .u
}

#' @rdname certify
#' @param strict Whether either of the inequality bounds is strict.
#' @export
between <- function(.l, .u, strict = c("neither", "lower", "upper", "both")) {
  strict <- match.arg(strict)
  switch(
    strict,
    neither = function(..x) gte(.l)(..x) && lte(.u)(..x),
    lower = function(..x) gt(.l)(..x) && lte(.u)(..x),
    upper = function(..x) gte(.l)(..x) && lt(.u)(..x),
    both = function(..x) gt(.l)(..x) && lt(.u)(..x)
  )
}
