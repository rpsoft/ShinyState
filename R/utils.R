#' @keywords internal
ss_debug <- function(...) {
  if (isTRUE(getOption("shinystate.debug", FALSE))) {
    message("[shinystate] ", ...)
  }
  invisible(NULL)
}
