#' Reactive preview region
#'
#' Wrap UI that should update when component state changes without rebuilding
#' bound text inputs. Place dynamic output (tables, summaries, plots) inside
#' `preview()`; keep `bindTextInput()` and similar controls outside it.
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
#'     preview(
#'       h3(state$title),
#'       p("Characters:", nchar(state$title))
#'     )
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
