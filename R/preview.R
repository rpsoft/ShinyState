#' Reactive preview region (optional)
#'
#' Explicitly mark a UI region that should update with state changes. This is
#' usually unnecessary: when `bindTextInput()`, `bindTextArea()`, or
#' `bindNumericInput()` are present, ShinyState automatically keeps those
#' controls mounted and live-updates everything else in `render()`.
#'
#' Use `preview()` only when you need a separate update region inside a subtree
#' that also contains typing controls.
#'
#' @param ... UI tags recomputed on every state change.
#'
#' @return A [shiny::uiOutput()] placeholder rendered into a separate output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' render = function(state, ns) {
#'   useInput("title")
#'   tagList(
#'     bindTextInput(ns, "title", "Title", state$title, update = "input"),
#'     h3(state$title)
#'   )
#' }
#' }
preview <- function(...) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`preview()` must be called inside a component `render()` function.")
  }

  eval_env <- rlang::caller_env()
  exprs <- rlang::enexprs(...)
  ctx$preview_fn <- function() {
    tags <- lapply(exprs, function(expr) {
      rlang::eval_tidy(expr, env = eval_env)
    })
    htmltools::tagList(!!!tags)
  }

  shiny::uiOutput(ctx$ns("shinystate_preview"))
}
