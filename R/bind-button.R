#' Action button wired to a click handler
#'
#' Creates an [shiny::actionButton()] that dispatches through ShinyState's
#' internal event channel. Call it inside a component `render()` function — the
#' namespace is taken from the render context, so no `ns` argument is needed.
#' Pass `onClick` to register the click handler inline; it receives the state
#' accessor. Use this instead of [shiny::actionButton()] — plain action buttons
#' are reset when the DOM re-renders, which can fire handlers twice.
#'
#' @param input_id Callback id.
#' @param label Button label.
#' @param onClick Handler run on click, receiving the state accessor. Equivalent
#'   to registering [useCallback()] with the same `input_id`. Optional — omit it
#'   to wire the handler separately with [useCallback()].
#' @param ... Passed to [shiny::actionButton()].
#'
#' @export
bindButton <- function(input_id, label, onClick = NULL, ...) {
  ctx <- require_render_ctx("bindButton")
  ns <- ctx$ns
  if (!is.null(onClick)) {
    if (!is.function(onClick)) {
      rlang::abort("`onClick` must be a function.")
    }
    ctx$callback_handlers[[input_id]] <- onClick
  }
  shiny::actionButton(
    ns(input_id),
    label,
    onclick = sprintf(
      "Shiny.setInputValue('%s', {id: '%s', t: Date.now()}, {priority: 'event'});",
      ns(".shinystate_event"),
      input_id
    ),
    ...
  )
}
