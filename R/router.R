# Hash-based URL routing ------------------------------------------------------

#' Sync a navbar or tabset with the browser URL hash
#'
#' Keeps the selected tab of a [shiny::navbarPage()] / [shiny::tabsetPanel()]
#' in sync with a hash route of the form `#!/page`. Opening the app at
#' `https://myapp/#!/search` navigates to the `search` tab, switching tabs
#' pushes `#!/<tab>` onto the browser history, and the browser back/forward
#' buttons navigate between tabs. Hashes that do not start with `#!/` (plain
#' in-page anchors) are ignored.
#'
#' Usually you do not call this directly: pass `routing = "hash"` to
#' [serve_dormant()], which wires the router to its components. Call it
#' directly to add URL routing to a multi-page app that does not use the
#' dormant lifecycle.
#'
#' @param session,input Shiny server function arguments.
#' @param navbar Character id of the `navbarPage()` / `tabsetPanel()`.
#' @param tabs Optional character vector of valid page names. Hash routes
#'   naming other pages trigger a one-time warning and fall back to `default`.
#'   `NULL` accepts any page.
#' @param default Fallback page for unknown hash routes. `NULL` warns and
#'   stays on the current tab.
#' @param hash Optional reactive returning the current URL hash. Defaults to
#'   `session$clientData$url_hash`; override it in tests, where the mock
#'   session's hash is not settable.
#'
#' @return Invisibly, a list with reactives `page` (current page name, or
#'   `NULL` before first navigation) and `parts` (all decoded `/`-separated
#'   segments of the route, e.g. `c("search", "extra")` for `#!/search/extra`;
#'   segments beyond the page name are preserved but do not affect
#'   navigation).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   route <- router_server(session, input, navbar = "pages",
#'                          tabs = c("dashboard", "search"),
#'                          default = "dashboard")
#'   # route$page() is the current page as a reactive
#' }
#' }
router_server <- function(session, input, navbar, tabs = NULL, default = NULL, hash = NULL) {
  if (missing(navbar) || !is.character(navbar) || length(navbar) != 1L || !nzchar(navbar)) {
    rlang::abort("`router_server()` requires a single `navbar` id string.")
  }
  if (!is.null(tabs) && (!is.character(tabs) || length(tabs) == 0L)) {
    rlang::abort("`tabs` must be NULL or a non-empty character vector.")
  }
  if (!is.null(default) && (!is.character(default) || length(default) != 1L)) {
    rlang::abort("`default` must be NULL or a single page name.")
  }

  hash_reactive <- if (is.null(hash)) {
    shiny::reactive(session$clientData$url_hash)
  } else {
    hash
  }

  is_known <- function(page) {
    is.null(tabs) || page %in% tabs
  }

  warned_routes <- new.env(parent = emptyenv())
  warn_unknown <- function(page) {
    if (!exists(page, envir = warned_routes, inherits = FALSE)) {
      assign(page, TRUE, envir = warned_routes)
      rlang::warn(sprintf(
        "router_server(): route '#!/%s' matches no known page (known: %s).",
        page, paste(tabs, collapse = ", ")
      ))
    }
  }

  # last_hash is the last hash this router wrote or accepted. It is the loop
  # guard: pushes echo back as hashchange events, and replace-state writes
  # never update clientData$url_hash, so the hash can only be compared against
  # this record, never read back.
  last_hash <- NA_character_
  nav_pending <- FALSE
  stale_budget <- 0L
  route_rv <- shiny::reactiveVal(list(page = NULL, parts = character(0)))

  # Init: adopt a deep link before either observer first fires, so the tab
  # observer's initial (pre-navigation) firing cannot clobber it.
  h0 <- shiny::isolate(hash_reactive())
  p0 <- parse_route_hash(h0)
  if (!is.null(p0$page)) {
    if (is_known(p0$page)) {
      last_hash <- h0
      route_rv(p0)
      nav_pending <- TRUE
      stale_budget <- 1L
      update_tab(session, navbar, p0$page)
    } else {
      warn_unknown(p0$page)
    }
  }

  shiny::observeEvent(hash_reactive(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    h <- hash_reactive()
    if (identical(h, last_hash)) {
      return(invisible(NULL))
    }
    p <- parse_route_hash(h)
    if (is.null(p$page)) {
      return(invisible(NULL))
    }
    if (!is_known(p$page)) {
      warn_unknown(p$page)
      if (!is.null(default)) {
        last_hash <<- format_route_hash(default)
        route_rv(list(page = default, parts = default))
        push_hash(session, last_hash, "replace")
        if (!identical(shiny::isolate(input[[navbar]]), default)) {
          nav_pending <<- TRUE
          stale_budget <<- 0L
          update_tab(session, navbar, default)
        }
      }
      return(invisible(NULL))
    }
    last_hash <<- h
    route_rv(p)
    if (!identical(shiny::isolate(input[[navbar]]), p$page)) {
      nav_pending <<- TRUE
      stale_budget <<- 0L
      update_tab(session, navbar, p$page)
    }
  })

  shiny::observeEvent(input[[navbar]], ignoreNULL = TRUE, {
    tab <- input[[navbar]]
    if (!is.character(tab) || length(tab) != 1L) {
      return(invisible(NULL))
    }
    target <- parse_route_hash(last_hash)$page
    if (identical(tab, target)) {
      nav_pending <<- FALSE
      return(invisible(NULL))
    }
    if (nav_pending && stale_budget > 0L) {
      stale_budget <<- stale_budget - 1L
      return(invisible(NULL))
    }
    nav_pending <<- FALSE
    h <- format_route_hash(tab)
    mode <- if (is.na(last_hash)) "replace" else "push"
    last_hash <<- h
    route_rv(list(page = tab, parts = tab))
    push_hash(session, h, mode)
  })

  handle <- list(
    page = shiny::reactive(route_rv()$page),
    parts = shiny::reactive(route_rv()$parts)
  )
  session$userData$shinystate_route <- handle
  invisible(handle)
}

#' Access the active route inside a component
#'
#' Returns the router handle for the current app from any component `render()`
#' function, so a component can read the current page without it being threaded
#' through props. Requires routing to be enabled (e.g.
#' `serve_dormant(routing = "hash")` or a [router_server()] call).
#'
#' @return A list with reactives `page` and `parts` (see [router_server()]), or
#'   `NULL` with a warning if no router is active.
#' @export
useRoute <- function() {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`useRoute()` must be called inside a component `render()` function.")
  }
  session <- ctx$session
  handle <- if (!is.null(session)) session$userData$shinystate_route else NULL
  if (is.null(handle)) {
    rlang::warn("useRoute(): no active router; enable routing with serve_dormant(routing = \"hash\").")
  }
  handle
}

#' Link to a routed page
#'
#' Renders a plain `<a href="#!/page">` anchor. Clicking it changes the URL
#' hash, which [router_server()] picks up and turns into tab navigation — no
#' JavaScript required.
#'
#' @param page Page name (a tab `value` routed by [router_server()]).
#' @param label Link text. Defaults to the page name.
#' @param ... Additional attributes passed to [shiny::tags].
#'
#' @export
#'
#' @examples
#' route_link("search", "Go to search")
route_link <- function(page, label = page, ...) {
  shiny::tags$a(href = format_route_hash(page), label, ...)
}

#' @keywords internal
parse_route_hash <- function(hash) {
  empty <- list(page = NULL, parts = character(0))
  if (is.null(hash) || !is.character(hash) || length(hash) != 1L || is.na(hash)) {
    return(empty)
  }
  if (!startsWith(hash, "#!/")) {
    return(empty)
  }
  path <- substring(hash, 4L)
  if (!nzchar(path)) {
    return(empty)
  }
  parts <- vapply(strsplit(path, "/", fixed = TRUE)[[1]], utils::URLdecode, character(1), USE.NAMES = FALSE)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(empty)
  }
  list(page = parts[[1]], parts = parts)
}

#' @keywords internal
format_route_hash <- function(page) {
  paste0("#!/", utils::URLencode(page, reserved = TRUE))
}

#' @keywords internal
update_tab <- function(session, navbar, page) {
  shiny::updateTabsetPanel(session, navbar, selected = page)
}

#' @keywords internal
push_hash <- function(session, hash, mode) {
  shiny::updateQueryString(hash, mode = mode, session = session)
}
