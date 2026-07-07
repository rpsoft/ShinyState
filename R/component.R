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
component <- function(id, ..., render) {
  if (missing(render) || !is.function(render)) {
    rlang::abort("`component()` requires a `render` function.")
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

  list(
    id = id,
    initial_state = initial_state,
    effect_specs = effect_specs,
    render_fn = render,
    ui = componentUI(id),
    server = componentServer(id, initial_state, effect_specs, render)
  )
}

#' @rdname component
#' @param name Module namespace id (same as component id).
#' @export
componentUI <- function(id) {
  ns <- shiny::NS(id)
  function(...) {
    shiny::uiOutput(ns("ui"))
  }
}

#' @rdname component
#' @param initial_state Named list of initial state values.
#' @param effect_specs List of effect specifications from [useEffect()].
#' @param render_fn Render function.
#' @export
componentServer <- function(id, initial_state = list(), effect_specs = list(), render_fn) {
  force(id)
  force(initial_state)
  force(effect_specs)
  force(render_fn)

  function(input, output, session) {
    ns <- session$ns
    store <- new_state_store(initial_state)
    version <- shiny::reactiveVal(0L)

    schedule_rerender <- function() {
      version(version() + 1L)
    }

    ctx <- new_hook_context(id, store, schedule_rerender, ns = ns, input = input)
    state_accessor <- make_state_accessor(store, schedule_rerender)

    shiny::observeEvent(
      input$`.shinystate_event`,
      {
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

    render_component <- function() {
      reset_hook_index(ctx)
      ctx$in_render <- TRUE
      ctx$effect_specs <- list()

      on.exit({
        ctx$in_render <- FALSE
      }, add = TRUE)

      for (spec in effect_specs) {
        idx <- length(ctx$effect_specs) + 1L
        ctx$effect_specs[[idx]] <- list(fn = spec$fn, deps = spec$deps)
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

      result
    }

    output$ui <- shiny::renderUI({
      version()
      shiny::isolate({
        tryCatch(
          {
            result <- render_component()
            is_first <- is.null(ctx$.has_rendered)
            ctx$.has_rendered <- TRUE
            run_effects(ctx, state_accessor, is_first_run = is_first)
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

#' Mount a component in a Shiny UI
#'
#' @param component A component list returned by [component()].
#' @param ... Passed to the component UI function.
#'
#' @export
mount <- function(component, ...) {
  component$ui(...)
}

#' Run server logic for a mounted component
#'
#' @param component A component list returned by [component()].
#' @param input,output,session Shiny server function arguments.
#'
#' @export
serve <- function(component, input, output, session) {
  shiny::moduleServer(component$id, component$server, session = session)
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
