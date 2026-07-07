#' @keywords internal
typing_control_class <- "shinystate-typing-control"

#' @keywords internal
is_typing_control <- function(x) {
  if (!inherits(x, "shiny.tag")) {
    return(FALSE)
  }
  class_attr <- x$attribs[["class"]] %||% ""
  grepl(typing_control_class, class_attr, fixed = TRUE)
}

#' @keywords internal
is_preview_placeholder <- function(x) {
  if (!inherits(x, "shiny.tag")) {
    return(FALSE)
  }
  id <- x$attribs[["id"]] %||% ""
  grepl("shinystate_(auto_preview_|preview)", id)
}

#' @keywords internal
ui_node_children <- function(ui) {
  if (inherits(ui, "shiny.tag.list") || inherits(ui, "tagList")) {
    return(as.list(ui))
  }
  if (inherits(ui, "shiny.tag")) {
    children <- ui$children
    if (length(children) == 0L) {
      return(list())
    }
    if (!is.list(children)) {
      return(list(children))
    }
    return(as.list(children))
  }
  if (is.list(ui)) {
    return(ui)
  }
  list()
}

#' @keywords internal
tag_child_nodes <- ui_node_children

#' @keywords internal
ui_children <- function(ui) {
  if (inherits(ui, "shiny.tag.list") || inherits(ui, "tagList")) {
    return(ui_node_children(ui))
  }
  if (inherits(ui, "shiny.tag")) {
    return(list(ui))
  }
  list(ui)
}

#' @keywords internal
is_interactive_control <- function(x) {
  if (!inherits(x, "shiny.tag")) {
    return(FALSE)
  }
  if (is_typing_control(x)) {
    return(TRUE)
  }
  class_attr <- x$attribs[["class"]] %||% ""
  grepl("shiny-input-container", class_attr, fixed = TRUE) ||
    grepl("action-button", class_attr, fixed = TRUE) ||
    grepl("\\bcheckbox\\b", class_attr) ||
    grepl("shiny-input-radiogroup", class_attr, fixed = TRUE) ||
    grepl("shiny-input-checkboxgroup", class_attr, fixed = TRUE)
}

#' @keywords internal
is_structure_tag <- function(x) {
  inherits(x, "shiny.tag") && x$name %in% c("h1", "h2", "h3", "h4", "h5", "h6", "hr", "br")
}

#' @keywords internal
is_title_panel <- function(x) {
  if (!inherits(x, "shiny.tag.list") && !inherits(x, "tagList")) {
    return(FALSE)
  }
  for (child in as.list(x)) {
    if (inherits(child, "shiny.tag") && identical(child$name, "h2")) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
is_shell_passthrough <- function(x) {
  is_shell_static(x) ||
    is_interactive_control(x) ||
    is_preview_placeholder(x) ||
    is_structure_tag(x) ||
    is_title_panel(x)
}

#' @keywords internal
is_shell_static <- function(child) {
  inherits(child, "html_dependency") || is.null(child)
}

#' @keywords internal
subtree_has_interactive_control <- function(ui) {
  if (is_interactive_control(ui)) {
    return(TRUE)
  }
  for (child in ui_node_children(ui)) {
    if (subtree_has_interactive_control(child)) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
tree_contains_typing_control <- function(ui) {
  if (is_typing_control(ui)) {
    return(TRUE)
  }
  for (child in ui_node_children(ui)) {
    if (tree_contains_typing_control(child)) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
preserve_root_dependencies <- function(original, shell) {
  if (!inherits(original, "shiny.tag.list") && !inherits(original, "tagList")) {
    return(shell)
  }
  dep_fn <- attr(original, "html_dependencies", exact = TRUE)
  if (!is.null(dep_fn)) {
    attr(shell, "html_dependencies") <- dep_fn
  }
  shell
}

#' @keywords internal
partition_ui <- function(ui, ns) {
  slots <- list()
  slot_counter <- 0L

  new_slot <- function(nodes) {
    slot_counter <<- slot_counter + 1L
    id <- paste0("shinystate_auto_preview_", slot_counter)
    slots[[id]] <<- nodes
    shiny::uiOutput(ns(id))
  }

  process_children <- function(children) {
    if (!is.list(children)) {
      children <- list(children)
    }
    out <- list()
    dynamic_batch <- list()

    flush <- function() {
      if (length(dynamic_batch) == 0L) {
        return(invisible(NULL))
      }
      out <<- c(out, list(new_slot(dynamic_batch)))
      dynamic_batch <<- list()
    }

    for (child in children) {
      if (is_shell_passthrough(child)) {
        flush()
        out <- c(out, list(child))
        next
      }
      if (subtree_has_interactive_control(child)) {
        flush()
        if (is.list(child) && !inherits(child, "shiny.tag") && !inherits(child, "shiny.tag.list") && !inherits(child, "tagList")) {
          out <- c(out, process_children(child))
        } else if (inherits(child, "shiny.tag") && length(child$children) > 0L) {
          child$children <- process_children(child$children)
          out <- c(out, list(child))
        } else {
          out <- c(out, list(child))
        }
        next
      }
      dynamic_batch <- c(dynamic_batch, list(child))
    }

    flush()
    out
  }

  shell <- htmltools::tagList(!!!process_children(ui_children(ui)))
  shell <- preserve_root_dependencies(ui, shell)
  list(ui = shell, slots = slots)
}

#' @keywords internal
extract_ui_slots <- function(ui) {
  slots <- list()
  slot_counter <- 0L

  new_slot <- function(nodes) {
    slot_counter <<- slot_counter + 1L
    id <- paste0("shinystate_auto_preview_", slot_counter)
    slots[[id]] <<- nodes
  }

  process_children <- function(children) {
    if (!is.list(children)) {
      children <- list(children)
    }
    dynamic_batch <- list()

    flush <- function() {
      if (length(dynamic_batch) == 0L) {
        return(invisible(NULL))
      }
      new_slot(dynamic_batch)
      dynamic_batch <<- list()
    }

    for (child in children) {
      if (is_shell_passthrough(child)) {
        flush()
        next
      }
      if (subtree_has_interactive_control(child)) {
        flush()
        if (is.list(child) && !inherits(child, "shiny.tag") && !inherits(child, "shiny.tag.list") && !inherits(child, "tagList")) {
          process_children(child)
        } else if (inherits(child, "shiny.tag") && length(child$children) > 0L) {
          process_children(child$children)
        }
        next
      }
      dynamic_batch <- c(dynamic_batch, list(child))
    }

    flush()
    invisible(NULL)
  }

  process_children(ui_children(ui))
  slots
}

#' @keywords internal
auto_preview_slot_id <- function(index) {
  paste0("shinystate_auto_preview_", index)
}
