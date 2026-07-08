# Shared stores --------------------------------------------------------------

#' Create a shared store
#'
#' A store holds state that several components can read and write, like a Pinia
#' or Zustand store. Components subscribe with [useStore()] and re-render when
#' the store changes. Use the returned object's `$get()` / `$set()` methods to
#' read and write from anywhere (e.g. inside a click handler).
#'
#' @param ... Named initial values.
#'
#' @return A `shinystate_store` object with methods:
#'   \describe{
#'     \item{`$get(name = NULL)`}{Read one field, or a snapshot of all fields.}
#'     \item{`$set(...)`}{Update named fields and notify subscribers.}
#'     \item{`$subscribe(fn)`}{Register a listener; returns an unsubscribe
#'       function.}
#'   }
#'
#' @seealso [useStore()]
#' @export
#'
#' @examples
#' store <- createStore(count = 0L)
#' store$set(count = 1L)
#' store$get("count")
createStore <- function(...) {
  initial <- list(...)
  if (length(initial) > 0L && (is.null(names(initial)) || any(!nzchar(names(initial))))) {
    rlang::abort("All `createStore()` arguments must be named.")
  }
  store_env <- new_state_store(initial)
  listeners <- new.env(parent = emptyenv())
  counter <- 0L

  notify <- function() {
    for (id in ls(listeners, all.names = TRUE)) {
      fn <- listeners[[id]]
      if (is.function(fn)) fn()
    }
    invisible(NULL)
  }

  self <- list(
    .store = store_env,
    notify = notify
  )
  self$get <- function(name = NULL) {
    if (is.null(name)) state_snapshot(store_env) else state_get(store_env, name)
  }
  self$set <- function(...) {
    do.call(state_set, c(list(store = store_env), list(...)))
    notify()
    invisible(NULL)
  }
  self$subscribe <- function(fn) {
    counter <<- counter + 1L
    id <- paste0("listener_", counter)
    listeners[[id]] <- fn
    function() {
      if (exists(id, envir = listeners, inherits = FALSE)) {
        rm(list = id, envir = listeners)
      }
      invisible(NULL)
    }
  }
  structure(self, class = "shinystate_store")
}

#' Use a shared store inside a component
#'
#' Subscribes the calling component to a [createStore()] store and returns a
#' state accessor for it. The component re-renders when the store changes, and
#' the subscription is cleaned up automatically when the session ends. Call
#' inside a component `render()` function.
#'
#' @param store A store created with [createStore()].
#'
#' @return A state accessor: read fields with `$field`, write with `$set()`
#'   (which notifies every subscribed component).
#'
#' @seealso [createStore()]
#' @export
useStore <- function(store) {
  if (!inherits(store, "shinystate_store")) {
    rlang::abort("`useStore()` requires a store created with `createStore()`.")
  }
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`useStore()` must be called inside a component `render()` function.")
  }

  slot <- next_hook_slot(ctx, "useStore")
  key <- paste0("store_", slot)
  if (is.null(ctx$store_subs)) {
    ctx$store_subs <- list()
  }
  if (!key %in% names(ctx$store_subs)) {
    unsubscribe <- store$subscribe(ctx$schedule_rerender)
    ctx$store_subs[[key]] <- unsubscribe
    if (!is.null(ctx$session)) {
      ctx$session$onSessionEnded(unsubscribe)
    }
  }

  make_state_accessor(store$.store, store$notify)
}
