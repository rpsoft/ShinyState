#' Serve components with dormant lifecycle
#'
#' Registers component servers that stay **dormant** while their tab or page is
#' hidden. Dormant components do not render, run effects, or handle events.
#' Component state is preserved when a tab becomes active again.
#'
#' Names of components must match the `value` of each [shiny::tabPanel()] in the
#' corresponding [shiny::navbarPage()] or [shiny::tabsetPanel()].
#'
#' @param ... Named components created with [component()]. Names must match tab
#'   `value` arguments.
#' @param session,input,output Shiny server function arguments.
#' @param navbar Character id of the `navbarPage()` / `tabsetPanel()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'   ui = navbarPage(
#'     id = "pages",
#'     tabPanel("Dashboard", mount(counter), value = "dashboard"),
#'     tabPanel("Search", mount(filter), value = "search")
#'   ),
#'   server = function(input, output, session) {
#'     serve_dormant(
#'       session = session, input = input, output = output,
#'       navbar = "pages",
#'       dashboard = counter,
#'       search = filter
#'     )
#'   }
#' )
#' }
serve_dormant <- function(..., session, input, output, navbar) {
  components <- list(...)
  if (length(components) == 0L) {
    rlang::abort("`serve_dormant()` requires at least one named component.")
  }
  if (is.null(names(components)) || any(names(components) == "")) {
    rlang::abort(
      "All components passed to `serve_dormant()` must be named with tab `value` ids."
    )
  }
  if (missing(navbar) || !is.character(navbar) || length(navbar) != 1L || !nzchar(navbar)) {
    rlang::abort("`serve_dormant()` requires a single `navbar` id string.")
  }

  current_tab <- shiny::reactive({
    input[[navbar]]
  })

  for (tab_id in names(components)) {
    local({
      tab_value <- tab_id
      cmp <- components[[tab_value]]
      active <- shiny::reactive({
        identical(current_tab(), tab_value)
      })
      serve(cmp, input, output, session, is_active = active)
    })
  }

  invisible(NULL)
}
