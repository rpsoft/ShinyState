# State storage ----------------------------------------------------------------

#' @keywords internal
new_state_store <- function(initial = list()) {
  store <- new.env(parent = emptyenv())
  for (nm in names(initial)) {
    store[[nm]] <- initial[[nm]]
  }
  attr(store, "ss_version") <- 0L
  store
}

#' @keywords internal
state_store_version <- function(store) {
  attr(store, "ss_version") %||% 0L
}

#' @keywords internal
state_snapshot <- function(store) {
  as.list(store, all.names = TRUE)
}

#' @keywords internal
state_get <- function(store, name) {
  store[[name]]
}

#' @keywords internal
state_set <- function(store, ...) {
  updates <- list(...)
  if (length(updates) == 0L) {
    return(invisible(NULL))
  }
  nm <- names(updates)
  if (is.null(nm) || any(nm == "")) {
    rlang::abort("All state updates must be named.")
  }
  for (name in nm) {
    store[[name]] <- updates[[name]]
  }
  attr(store, "ss_version") <- (attr(store, "ss_version") %||% 0L) + 1L
  invisible(updates)
}
