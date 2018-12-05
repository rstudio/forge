#' Ensure Conditions on a Value
#'
#' Checks that the input value satisfies specified conditions
#'
#' @param .x The value to be checked.
#' @param ... Conditions to be checked; should be functions that return TRUE/FALSE.
#' @param .l Lower bound for the inequality condition.
#' @param .u Upper bound for the inequality condition.
#' @param .id Name given to the input to aid the user in identifying the bad value.
#' @export
certify <- function(.x, ..., .id = NULL) {
  .id <- resolve_id(rlang::enquo(.x), .id)
  quos <- rlang::enquos(...)

  for (quo in quos) {
    condition <- rlang::as_function(rlang::eval_tidy(quo))
    satisfied <- condition(.x)

    if (!rlang::is_scalar_logical(satisfied)) stop(
      "`",
      rlang::quo_text(quo),
      "` does not evaluate to a scalar logical for ",
      backticks(.id),
      "."
    )

    if (!satisfied) stop(
      "Condition `",
      rlang::quo_text(quo),
      "` not satisfied for ",
      backticks(.id),
      ".",
      call. = FALSE
    )
  }

  new_forge_stamped(.x, .id = .id)
}

#' @rdname certify
#' @export
gt <- function(.l) {
  force(.l)
  function(.x) all(.x > .l)
}

#' @rdname certify
#' @export
gte <- function(.l) {
  force(.l)
  function(.x) all(.x >= .l)
}

#' @rdname certify
#' @export
lt <- function(.u) {
  force(.u)
  function(.x) all(.x < .u)
}

#' @rdname certify
#' @export
lte <- function(.u) {
  force(.u)
  function(.x) all(.x <= .u)
}

#' @rdname certify
#' @param strict Whether either of the inequality bounds is strict.
#' @export
between <- function(.l, .u, strict = c("neither", "lower", "upper", "both")) {
  strict <- match.arg(strict)
  switch(
    strict,
    neither = function(.x) gte(.l)(.x) && lte(.u)(.x),
    lower = function(.x) gt(.l)(.x) && lte(.u)(.x),
    upper = function(.x) gte(.l)(.x) && lt(.u)(.x),
    both = function(.x) gt(.l)(.x) && lt(.u)(.x)
  )
}
