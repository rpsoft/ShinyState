# Hook execution context -------------------------------------------------------

#' @keywords internal
new_hook_context <- function(component_id, state_store, schedule_rerender, ns = NULL, input = NULL) {
  env <- new.env(parent = emptyenv())
  env$component_id <- component_id
  env$state_store <- state_store
  env$schedule_rerender <- schedule_rerender
  env$ns <- ns
  env$input <- input
  env$hook_index <- 0L
  env$hook_effect_index <- 0L
  env$hook_sequence <- character(0)
  env$memo_cache <- list()
  env$effect_specs <- list()
  env$callback_handlers <- list()
  env$in_render <- FALSE
  env
}

#' @keywords internal
get_hook_context <- function() {
  getOption("shinystate.hook_context", NULL)
}

#' @keywords internal
with_hook_context <- function(ctx, expr) {
  old <- getOption("shinystate.hook_context", NULL)
  on.exit(options(shinystate.hook_context = old), add = TRUE)
  options(shinystate.hook_context = ctx)
  force(expr)
}

#' @keywords internal
next_hook_slot <- function(ctx, type = "hook") {
  ctx$hook_index <- ctx$hook_index + 1L
  ctx$hook_sequence <- c(ctx$hook_sequence, type)
  ctx$hook_index
}

#' @keywords internal
reset_hook_index <- function(ctx) {
  ctx$hook_index <- 0L
  ctx$hook_effect_index <- 0L
  ctx$hook_sequence <- character(0)
}

#' @keywords internal
check_hook_order <- function(ctx) {
  prev <- ctx$prev_hook_sequence
  cur <- ctx$hook_sequence
  if (!is.null(prev) && !identical(prev, cur) && !isTRUE(ctx$hook_order_warned)) {
    ctx$hook_order_warned <- TRUE
    rlang::warn(sprintf(
      paste0(
        "ShinyState: hooks in component '%s' were called in a different order ",
        "than the previous render. Call hooks unconditionally and in the same ",
        "order on every render (put conditions inside the hook, not around it)."
      ),
      ctx$component_id
    ))
  }
  ctx$prev_hook_sequence <- cur
  invisible(NULL)
}
