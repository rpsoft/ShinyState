#' Component State
#'
#' Declares initial state for a [component()] (declarative style) or reads /
#' updates state during render (hook style).
#'
#' @param ... Named initial values. Each name becomes a state field.
#'
#' @return
#' Outside of an active render cycle, returns a state *specification* object
#' passed to [component()]. Inside `render()`, returns a [shinystate_state]
#' accessor for reading and updating values.
#'
#' @export
#'
#' @examples
#' # Declarative specification (used in component())
#' useState(page = 1L, filter = NULL)
#'
#' # Runtime usage inside render() is shown in ?component
useState <- function(...) {
  initial <- list(...)
  if (length(initial) == 0L) {
    rlang::abort("`useState()` requires at least one named argument.")
  }
  if (is.null(names(initial)) || any(names(initial) == "")) {
    rlang::abort("All `useState()` arguments must be named.")
  }

  ctx <- get_hook_context()
  if (!is.null(ctx) && isTRUE(ctx$in_render)) {
    return(use_state_hook(initial, ctx))
  }

  structure(
    list(initial = initial),
    class = "shinystate_state_spec"
  )
}

#' @keywords internal
use_state_hook <- function(initial, ctx) {
  next_hook_slot(ctx, "useState")
  store <- ctx$state_store

  missing_fields <- setdiff(names(initial), ls(envir = store, all.names = TRUE))
  if (length(missing_fields) > 0L) {
    do.call(state_set, c(list(store = store), initial[missing_fields]))
  }

  make_state_accessor(store, ctx$schedule_rerender)
}

#' @rdname useState
#' @param reducer Function `(state, action)` returning the new state.
#' @param initial Initial reducer state.
#' @export
useReducer <- function(reducer, initial) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`useReducer()` must be called inside a component `render()` function.")
  }

  slot <- next_hook_slot(ctx, "useReducer")
  key <- paste0("reducer_", slot)

  if (is.null(ctx$reducer_slots)) {
    ctx$reducer_slots <- list()
  }

  if (!key %in% names(ctx$reducer_slots)) {
    ctx$reducer_slots[[key]] <- list(
      reducer = reducer,
      state = initial
    )
  }

  dispatch <- function(action) {
    slot_state <- ctx$reducer_slots[[key]]
    slot_state["state"] <- list(slot_state$reducer(slot_state$state, action))
    ctx$reducer_slots[[key]] <- slot_state
    ctx$schedule_rerender()
    invisible(slot_state$state)
  }

  list(
    state = ctx$reducer_slots[[key]]$state,
    dispatch = dispatch
  )
}

#' @rdname useState
#' @param fn Function to run. For [useEffect()], receives the state accessor
#'   (declarative components) or no arguments (hook-style). For [useMemo()],
#'   should return the memoized value.
#' @param deps Character vector of state field names, or `NULL` to run once on
#'   mount. Dependency values are read from the component state store.
#'
#' @export
useEffect <- function(fn, deps = NULL) {
  ctx <- get_hook_context()

  if (!is.null(ctx) && isTRUE(ctx$in_render)) {
    if (is.null(ctx$hook_effect_index)) {
      ctx$hook_effect_index <- 0L
    }
    ctx$hook_effect_index <- ctx$hook_effect_index + 1L
    ctx$effect_specs[[paste0("hook_", ctx$hook_effect_index)]] <- list(fn = fn, deps = deps)
    return(invisible(NULL))
  }

  if (missing(fn)) {
    rlang::abort("`useEffect()` requires a function.")
  }

  structure(
    list(fn = fn, deps = deps),
    class = "shinystate_effect_spec"
  )
}

#' @rdname useState
#' @export
useMemo <- function(fn, deps) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`useMemo()` must be called inside a component `render()` function.")
  }

  slot <- next_hook_slot(ctx, "useMemo")
  cache_key <- paste0("memo_", slot)
  current_deps <- resolve_dep_values(ctx$state_store, deps)
  cached <- ctx$memo_cache[[cache_key]]

  if (is.null(cached) || !identical(cached$deps, current_deps)) {
    value <- fn()
    ctx$memo_cache[[cache_key]] <- list(deps = current_deps, value = value)
    return(value)
  }

  cached$value
}

#' @rdname useState
#' @param input_id Callback id (without namespace). Pair with [bindButton()] or a
#'   `bind*()` input helper in the UI — do not use raw Shiny inputs directly
#'   inside `render()`.
#' @param fn Handler function. Receives the state accessor as the first
#'   argument. When wired through a `bind*()` helper that sends a value, the
#'   event value is passed as the second argument.
#' @export
useCallback <- function(input_id, fn) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`useCallback()` must be called inside a component `render()` function.")
  }

  next_hook_slot(ctx, "useCallback")
  ctx$callback_handlers[[input_id]] <- fn
  invisible(NULL)
}

#' Lifecycle hooks
#'
#' Register callbacks tied to a component's lifecycle. Call these inside a
#' component `render()` function. Handlers receive the state accessor.
#'
#' * `onMounted()` runs once, after the component first renders.
#' * `onUnmounted()` runs when the session ends (after effect cleanups).
#' * `onActivated()` / `onDeactivated()` run when a dormant tab becomes visible
#'   or hidden (see [serve_dormant()]). They never fire for always-on
#'   components served with [serve()].
#'
#' @param fn Handler function; receives the state accessor.
#' @return Invisibly `NULL`.
#' @name lifecycle-hooks
NULL

#' @rdname lifecycle-hooks
#' @export
onMounted <- function(fn) {
  if (!is.function(fn)) {
    rlang::abort("`onMounted()` requires a function.")
  }
  useEffect(fn, deps = NULL)
}

#' @rdname lifecycle-hooks
#' @export
onUnmounted <- function(fn) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`onUnmounted()` must be called inside a component `render()` function.")
  }
  ctx$unmounted_handlers <- c(ctx$unmounted_handlers, list(fn))
  invisible(NULL)
}

#' @rdname lifecycle-hooks
#' @export
onActivated <- function(fn) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`onActivated()` must be called inside a component `render()` function.")
  }
  ctx$activated_handlers <- c(ctx$activated_handlers, list(fn))
  invisible(NULL)
}

#' @rdname lifecycle-hooks
#' @export
onDeactivated <- function(fn) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`onDeactivated()` must be called inside a component `render()` function.")
  }
  ctx$deactivated_handlers <- c(ctx$deactivated_handlers, list(fn))
  invisible(NULL)
}

#' Watch state fields for changes
#'
#' Runs `fn(new_values, old_values)` whenever any of the watched state `fields`
#' change. Both arguments are named lists keyed by `fields`. Call inside a
#' component `render()` function. Sugar over [useEffect()] that also gives you
#' the previous values.
#'
#' @param fields Character vector of state field names to watch.
#' @param fn Function `(new_values, old_values)` run when a watched field
#'   changes.
#' @param immediate If `TRUE`, also run once on first render (with `old_values`
#'   all `NULL`).
#' @return Invisibly `NULL`.
#' @export
watch <- function(fields, fn, immediate = FALSE) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort("`watch()` must be called inside a component `render()` function.")
  }
  if (!is.character(fields) || length(fields) == 0L) {
    rlang::abort("`watch()` requires a non-empty character vector of field names.")
  }
  if (!is.function(fn)) {
    rlang::abort("`watch()` requires a function `(new, old)`.")
  }
  ctx$hook_effect_index <- ctx$hook_effect_index + 1L
  ctx$effect_specs[[paste0("watch_", ctx$hook_effect_index)]] <- list(
    fn = fn, deps = fields, watch = TRUE, immediate = isTRUE(immediate)
  )
  invisible(NULL)
}

#' Declarative effect specification for [component()]
#'
#' @param deps Character vector of state field names to watch, or `NULL` to run
#'   once on mount.
#' @param fn Function invoked when dependencies change. Receives the state
#'   accessor.
#'
#' @export
effect <- function(deps = NULL, fn) {
  if (missing(fn) || !is.function(fn)) {
    rlang::abort("`effect()` requires a function as the second argument.")
  }
  structure(
    list(fn = fn, deps = deps),
    class = "shinystate_effect_spec"
  )
}

#' @keywords internal
resolve_dep_values <- function(store, deps) {
  if (is.null(deps)) {
    return(structure(list(), class = "shinystate_deps"))
  }
  if (length(deps) == 0L) {
    return(structure(list(), class = "shinystate_deps"))
  }
  if (length(deps) == 1L && !is.null(names(deps))) {
    deps <- deps[[1]]
  }
  vals <- lapply(deps, function(name) state_get(store, name))
  names(vals) <- deps
  structure(vals, class = "shinystate_deps")
}

#' @keywords internal
deps_changed <- function(prev, current) {
  if (is.null(prev) && is.null(current)) {
    return(FALSE)
  }
  if (is.null(prev) || is.null(current)) {
    return(TRUE)
  }
  !identical(prev, current)
}

#' @keywords internal
run_effects <- function(ctx, state_accessor, is_first_run = FALSE) {
  specs <- ctx$effect_specs
  if (length(specs) == 0L) {
    return(invisible(NULL))
  }

  keys <- names(specs)
  if (is.null(keys) || any(keys == "")) {
    fallback <- paste0("pos_", seq_along(specs))
    if (is.null(keys)) {
      keys <- fallback
    } else {
      keys[keys == ""] <- fallback[keys == ""]
    }
    names(specs) <- keys
  }

  if (is.null(ctx$prev_effect_deps)) {
    ctx$prev_effect_deps <- list()
  }
  if (is.null(ctx$effect_cleanup)) {
    ctx$effect_cleanup <- list()
  }

  for (key in keys) {
    spec <- specs[[key]]
    is_watch <- isTRUE(spec$watch)
    current_deps <- resolve_dep_values(ctx$state_store, spec$deps)
    prev_deps <- ctx$prev_effect_deps[[key]]

    should_run <- if (is_first_run) {
      if (is_watch) isTRUE(spec$immediate) else TRUE
    } else if (is.null(spec$deps)) {
      FALSE
    } else {
      deps_changed(prev_deps, current_deps)
    }

    if (!should_run) {
      ctx$prev_effect_deps[[key]] <- current_deps
      next
    }

    if (is_watch) {
      new_values <- unclass(current_deps)
      old_values <- if (is.null(prev_deps)) {
        stats::setNames(vector("list", length(spec$deps)), spec$deps)
      } else {
        unclass(prev_deps)
      }
      spec$fn(new_values, old_values)
      ctx$prev_effect_deps[[key]] <- current_deps
      next
    }

    cleanup <- ctx$effect_cleanup[[key]]
    if (!is.null(cleanup)) {
      tryCatch(cleanup(), error = function(e) NULL)
      ctx$effect_cleanup[[key]] <- NULL
    }

    result <- if (length(formals(spec$fn)) > 0L) {
      spec$fn(state_accessor)
    } else {
      spec$fn()
    }

    if (is.function(result)) {
      ctx$effect_cleanup[[key]] <- result
    }

    ctx$prev_effect_deps[[key]] <- current_deps
  }

  invisible(NULL)
}

#' @keywords internal
run_effect_cleanups <- function(ctx) {
  if (is.null(ctx$effect_cleanup)) {
    return(invisible(NULL))
  }
  for (cleanup in ctx$effect_cleanup) {
    if (is.function(cleanup)) {
      tryCatch(cleanup(), error = function(e) NULL)
    }
  }
  ctx$effect_cleanup <- list()
  invisible(NULL)
}
