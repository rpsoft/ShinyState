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
#' @param routing `"hash"` to sync the active tab with a `#!/page` URL hash
#'   via [router_server()]: pages become bookmarkable, and the browser
#'   back/forward buttons navigate between tabs. Component names double as
#'   page names; the first component is the fallback for unknown routes (it
#'   should correspond to the navbar's default tab). `"none"` (default)
#'   disables routing.
#'
#' @return Invisibly, the [router_server()] handle (reactives `page` and
#'   `parts`) when `routing = "hash"`, otherwise `NULL`.
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
serve_dormant <- function(..., session, input, output, navbar,
                          routing = c("none", "hash")) {
  routing <- match.arg(routing)
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
  for (nm in names(components)) {
    cmp <- components[[nm]]
    if (!is.list(cmp) || !is.function(cmp$server)) {
      rlang::abort(sprintf(
        "`serve_dormant()` argument '%s' is not a component; create it with `component()`.",
        nm
      ))
    }
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

  route <- NULL
  if (identical(routing, "hash")) {
    route <- router_server(
      session, input, navbar,
      tabs = names(components),
      default = names(components)[[1]]
    )
  }

  warned_tabs <- new.env(parent = emptyenv())
  shiny::observeEvent(input[[navbar]], {
    val <- input[[navbar]]
    if (is.character(val) && length(val) == 1L && !val %in% names(components) &&
        !exists(val, envir = warned_tabs, inherits = FALSE)) {
      assign(val, TRUE, envir = warned_tabs)
      rlang::warn(sprintf(
        "serve_dormant(): tab '%s' on navbar '%s' has no matching component (known: %s).",
        val, navbar, paste(names(components), collapse = ", ")
      ))
    }
  })

  invisible(route)
}
