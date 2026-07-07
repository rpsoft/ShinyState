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
next_hook_slot <- function(ctx) {
  ctx$hook_index <- ctx$hook_index + 1L
  ctx$hook_index
}

#' @keywords internal
reset_hook_index <- function(ctx) {
  ctx$hook_index <- 0L
  ctx$hook_effect_index <- 0L
}
