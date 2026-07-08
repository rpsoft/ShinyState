#' Define a Shiny Component
#'
#' Creates a Shiny module that renders UI using a React-inspired hook model.
#' State, effects, and rendering are declared declaratively; Shiny reactivity is
#' handled internally so you never write [shiny::reactive()] or
#' [shiny::observe()].
#'
#' @param id Component / module id.
#' @param ... Declarations: `useState()` for initial state, `effect()` or
#'   `useEffect()` for side effects, and optionally additional hook calls inside
#'   `render`.
#' @param render Function returning UI (a [htmltools::tag] or list of tags).
#'   Receives a [shinystate_state] accessor when declarative `useState()` is
#'   provided, otherwise call `useState()` inside the function. A second
#'   argument `ns` provides the namespace function.
#' @param computed Named list of functions `function(state)` exposing derived
#'   values as `state$name`. Each is cached and recomputed only when state
#'   changes.
#'
#' @return A list with `ui` and `server` functions suitable for
#'   [shiny::moduleServer()], or use [componentUI()] / [componentServer()] helpers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' counter <- component(
#'   id = "counter",
#'   state = useState(count = 0L),
#'   effect(
#'     deps = "count",
#'     function(state) {
#'       message("Count is now: ", state$count)
#'     }
#'   ),
#'   render = function(state, ns) {
#'     useCallback("inc", function(s) {
#'       s$set(count = s$count + 1L)
#'     })
#'     shiny::tagList(
#'       shiny::h3(paste("Count:", state$count)),
#'       shiny::actionButton(ns("inc"), "Increment")
#'     )
#'   }
#' )
#'
#' shiny::shinyApp(
#'   ui = shiny::fluidPage(mount(counter)),
#'   server = function(input, output, session) serve(counter, input, output, session)
#' )
#' }
component <- function(id, ..., render, computed = list()) {
  if (missing(render) || !is.function(render)) {
    rlang::abort("`component()` requires a `render` function.")
  }
  if (!is.list(computed) || (length(computed) > 0L &&
      (is.null(names(computed)) || any(!nzchar(names(computed)))))) {
    rlang::abort("`computed` must be a named list of functions.")
  }
  for (fn in computed) {
    if (!is.function(fn)) {
      rlang::abort("Each `computed` entry must be a function of `state`.")
    }
  }

  dots <- list(...)
  state_spec <- NULL
  effect_specs <- list()

  for (item in dots) {
    if (inherits(item, "shinystate_state_spec")) {
      state_spec <- item
    } else if (inherits(item, "shinystate_effect_spec")) {
      effect_specs <- c(effect_specs, list(item))
    } else {
      rlang::warn(paste0(
        "Ignoring unknown component argument of class: ",
        paste(class(item), collapse = "/")
      ))
    }
  }

  initial_state <- if (!is.null(state_spec)) {
    state_spec$initial
  } else {
    list()
  }

  structure(
    list(
      id = id,
      initial_state = initial_state,
      effect_specs = effect_specs,
      computed = computed,
      render_fn = render,
      ui = componentUI(id),
      server = componentServer(id, initial_state, effect_specs, render, computed)
    ),
    class = c("shinystate_component", "list")
  )
}

#' @rdname component
#' @export
componentUI <- function(id) {
  ns <- shiny::NS(id)
  function(...) {
    htmltools::tagList(
      shinystate_dependency(),
      shiny::div(id = ns("ui"), class = "shinystate-output")
    )
  }
}

#' @keywords internal
shinystate_dependency <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("ShinyState")),
    error = function(e) "0.0.0"
  )
  htmltools::htmlDependency(
    name = "shinystate",
    version = version,
    src = c(file = system.file("www", package = "ShinyState")),
    script = "shinystate.js"
  )
}

#' @rdname component
#' @param initial_state Named list of initial state values.
#' @param effect_specs List of effect specifications from [useEffect()].
#' @param render_fn Render function.
#'
#' @details
#' The server function returned by `componentServer()` has signature
#' `function(input, output, session, is_active = NULL)`. `is_active` is an
#' optional reactive expression returning `TRUE` while the component should be
#' active; when it returns `FALSE` the component is dormant (no rendering,
#' effects, or event handling) while its state is preserved. It is supplied
#' automatically by [serve()] and [serve_dormant()].
#' @export
componentServer <- function(id, initial_state = list(), effect_specs = list(), render_fn, computed = list()) {
  force(id)
  force(initial_state)
  force(effect_specs)
  force(render_fn)
  force(computed)

  function(input, output, session, is_active = NULL, props = NULL) {
    ns <- session$ns
    store <- new_state_store(initial_state)
    version <- shiny::reactiveVal(0L)

    active_reactive <- if (is.null(is_active)) {
      shiny::reactive(TRUE)
    } else {
      is_active
    }

    is_component_active <- function() {
      isTRUE(active_reactive())
    }

    # Bump the render counter without registering `version` as a reactive
    # dependency of the caller. schedule_rerender() is invoked from plain
    # observers (props, stores) and event handlers alike; an un-isolated
    # version() read there would make the caller depend on version and
    # re-fire whenever it bumps, causing an infinite render loop.
    bump_version <- function() {
      shiny::isolate(version(version() + 1L))
    }

    schedule_rerender <- function() {
      if (isTRUE(ctx$dormant)) {
        return(invisible(NULL))
      }
      ss_debug("render scheduled: ", id)
      bump_version()
      invisible(NULL)
    }

    ctx <- new_hook_context(id, store, schedule_rerender, ns = ns, input = input)
    ctx$session <- session
    ctx$dormant <- !is.null(is_active)
    state_accessor <- make_state_accessor(store, schedule_rerender, computed)

    if (!is.null(props)) {
      shiny::observeEvent(props(), {
        values <- props()
        if (is.list(values) && length(values) > 0L) {
          do.call(state_set, c(list(store = store), values))
          schedule_rerender()
        }
      }, ignoreNULL = FALSE)
    }

    if (!is.null(is_active)) {
      shiny::observe({
        if (is_component_active()) {
          if (isTRUE(ctx$dormant)) {
            ss_debug("activated: ", id)
            ctx$dormant <- FALSE
            ctx$.pending_activated <- TRUE
            bump_version()
          }
        } else if (!isTRUE(ctx$dormant)) {
          ss_debug("deactivated: ", id)
          ctx$dormant <- TRUE
          run_lifecycle_handlers(ctx$deactivated_handlers, state_accessor)
          run_effect_cleanups(ctx)
        }
      })
    }

    session$onSessionEnded(function() {
      run_effect_cleanups(ctx)
      run_lifecycle_handlers(ctx$unmounted_handlers, state_accessor)
    })

    shiny::observeEvent(
      input$`.shinystate_event`,
      {
        if (isTRUE(ctx$dormant)) {
          return()
        }
        payload <- shiny::req(input$`.shinystate_event`)
        handler <- ctx$callback_handlers[[payload$id]]
        if (is.null(handler)) {
          return()
        }
        value <- payload$value
        nformals <- length(formals(handler))
        if (!is.null(value) && nformals >= 2L) {
          handler(state_accessor, value)
        } else if (nformals >= 1L) {
          handler(state_accessor)
        } else {
          handler()
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    render_raw_ui <- function() {
      reset_hook_index(ctx)
      ctx$in_render <- TRUE
      ctx$effect_specs <- list()
      ctx$activated_handlers <- list()
      ctx$deactivated_handlers <- list()
      ctx$unmounted_handlers <- list()
      ctx$pending_children <- list()

      on.exit({
        ctx$in_render <- FALSE
      }, add = TRUE)

      for (i in seq_along(effect_specs)) {
        spec <- effect_specs[[i]]
        ctx$effect_specs[[paste0("decl_", i)]] <- list(fn = spec$fn, deps = spec$deps)
      }

      nformals <- length(formals(render_fn))
      result <- with_hook_context(ctx, {
        if (nformals >= 2L) {
          render_fn(state_accessor, ns)
        } else if (nformals >= 1L) {
          render_fn(state_accessor)
        } else {
          render_fn()
        }
      })
      check_hook_order(ctx)
      result
    }

    output$ui <- shiny::renderUI({
      version()
      if (isTRUE(ctx$dormant)) {
        return(NULL)
      }
      shiny::isolate({
        tryCatch(
          {
            result <- render_raw_ui()
            is_first <- is.null(ctx$.has_rendered)
            ctx$.has_rendered <- TRUE
            run_effects(ctx, state_accessor, is_first_run = is_first)
            serve_pending_children(ctx, output, session, active_reactive)
            if (isTRUE(ctx$.pending_activated)) {
              ctx$.pending_activated <- NULL
              run_lifecycle_handlers(ctx$activated_handlers, state_accessor)
            }
            result
          },
          error = function(e) {
            shiny::div(
              style = "color: #b00020; padding: 1em; border: 1px solid #b00020;",
              shiny::strong("ShinyState render error: "),
              conditionMessage(e)
            )
          }
        )
      })
    })

  }
}

#' @keywords internal
serve_pending_children <- function(ctx, output, session, active_reactive) {
  pending <- ctx$pending_children
  if (is.null(pending) || length(pending) == 0L) {
    return(invisible(NULL))
  }
  if (is.null(ctx$children_registry)) {
    ctx$children_registry <- new.env(parent = emptyenv())
  }
  reg <- ctx$children_registry

  for (child_id in names(pending)) {
    entry <- pending[[child_id]]
    child <- entry$component
    props <- entry$props %||% list()

    if (exists(child_id, envir = reg, inherits = FALSE)) {
      rec <- get(child_id, envir = reg, inherits = FALSE)
      if (!identical(rec$last_props, props)) {
        rec$props_rv(props)
        rec$last_props <- props
        assign(child_id, rec, envir = reg)
      }
      next
    }

    props_rv <- shiny::reactiveVal(props)
    local({
      ch <- child
      prv <- props_rv
      act <- active_reactive
      shiny::moduleServer(
        ch$id,
        function(input, output, session) {
          ch$server(input, output, session, is_active = act, props = prv)
        },
        session = session
      )
    })
    assign(child_id, list(props_rv = props_rv, last_props = props), envir = reg)
  }
  invisible(NULL)
}

#' @keywords internal
run_lifecycle_handlers <- function(handlers, state_accessor) {
  if (is.null(handlers)) {
    return(invisible(NULL))
  }
  for (fn in handlers) {
    if (is.function(fn)) {
      if (length(formals(fn)) > 0L) {
        fn(state_accessor)
      } else {
        fn()
      }
    }
  }
  invisible(NULL)
}

#' Mount a component in a Shiny UI
#'
#' Place a component's UI in a page, or — when called inside another
#' component's `render()` — nest it as a **child component** and pass it
#' `props`. A child's server is started once; when the parent re-renders with
#' different `props`, the child receives the new values (as state fields) and
#' re-renders. Props are owned by the parent: a child should read them
#' (`state$propname`) but not overwrite them with `state$set()`.
#'
#' @param component A component list returned by [component()].
#' @param props Named list of values to pass to a nested child component. Each
#'   name becomes a state field the child can read. Only used when `mount()` is
#'   called inside a parent's `render()`.
#' @param ... Passed to the component UI function (top-level mounting only).
#'
#' @export
mount <- function(component, props = list(), ...) {
  ctx <- get_hook_context()
  if (!is.null(ctx) && isTRUE(ctx$in_render)) {
    if (is.null(ctx$pending_children)) {
      ctx$pending_children <- list()
    }
    ctx$pending_children[[component$id]] <- list(component = component, props = props)
    return(htmltools::tagList(
      shinystate_dependency(),
      shiny::div(
        id = ctx$ns(paste0(component$id, "-ui")),
        class = "shinystate-output"
      )
    ))
  }
  component$ui(...)
}

#' Run server logic for a mounted component
#'
#' @param component A component list returned by [component()].
#' @param input,output,session Shiny server function arguments.
#' @param is_active Optional reactive expression returning `TRUE` when the
#'   component should be active. When `FALSE`, the component is dormant: no
#'   render, effects, or event handling. See [serve_dormant()].
#'
#' @export
serve <- function(component, input, output, session, is_active = NULL) {
  active_reactive <- if (is.null(is_active)) {
    NULL
  } else {
    shiny::reactive({
      is_active()
    })
  }
  module <- function(input, output, session) {
    component$server(input, output, session, is_active = active_reactive)
  }
  shiny::moduleServer(component$id, module, session = session)
}

#' Wrap a component as a Shiny module
#'
#' @param component A component list returned by [component()].
#'
#' @return A list with `ui` and `server` functions for [shiny::moduleServer()].
#' @export
asModule <- function(component) {
  list(
    ui = component$ui,
    server = component$server
  )
}
