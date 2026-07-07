#' Component state accessor
#'
#' Object returned inside a component `render()` function for reading and
#' updating state. Use `$field` to read and `$set()` to write.
#'
#' @name shinystate_state
#' @format An environment with S3 access to state fields.
NULL

#' @keywords internal
make_state_accessor <- function(store, schedule_rerender) {
  env <- new.env(parent = emptyenv())
  env$.store <- store
  env$.schedule <- schedule_rerender

  env$set <- function(...) {
    state_set(store, ...)
    schedule_rerender()
    invisible(NULL)
  }

  env$update <- function(updater) {
    if (!is.function(updater)) {
      rlang::abort("`state$update()` expects a function.")
    }
    current <- state_snapshot(store)
    updates <- updater(current)
    if (!is.null(updates)) {
      if (is.null(names(updates)) || any(names(updates) == "")) {
        rlang::abort("`state$update()` must return a named list.")
      }
      do.call(state_set, c(list(store = store), updates))
      schedule_rerender()
    }
    invisible(NULL)
  }

  env$all <- function() {
    state_snapshot(store)
  }

  structure(env, class = "shinystate_state")
}

#' @export
`$.shinystate_state` <- function(x, name) {
  if (exists(name, envir = x, inherits = FALSE)) {
    return(get(name, envir = x, inherits = FALSE))
  }
  state_get(x$.store, name)
}

#' @export
`[[.shinystate_state` <- `$.shinystate_state`

#' @export
print.shinystate_state <- function(x, ...) {
  vals <- state_snapshot(x$.store)
  cat("<shinystate_state>\n")
  for (nm in names(vals)) {
    cat("  ", nm, ": ", sep = "")
    print(vals[[nm]], ...)
  }
  invisible(x)
}
