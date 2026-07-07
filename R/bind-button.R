#' Action button wired to useCallback()
#'
#' Creates an [shiny::actionButton()] that dispatches through ShinyState's
#' internal event channel. Use this instead of [shiny::actionButton()] inside
#' component `render()` functions — plain action buttons are reset when
#' [shiny::renderUI()] re-renders, which can fire handlers twice.
#'
#' @param ns Namespace function from the component `render()` function.
#' @param input_id Callback id passed to [useCallback()].
#' @param label Button label.
#' @param ... Passed to [shiny::actionButton()].
#'
#' @export
bindButton <- function(ns, input_id, label, ...) {
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
