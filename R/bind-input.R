#' @keywords internal
event_channel_id <- function(ns) {
  ns(".shinystate_event")
}

#' @keywords internal
dispatch_js <- function(ns, callback_id, value_expr) {
  channel <- event_channel_id(ns)
  send_body <- sprintf(
    paste0(
      "var __ss_val=%s;",
      "Shiny.setInputValue('%s',{id:'%s',t:Date.now(),value:__ss_val},{priority:'event'});"
    ),
    value_expr,
    channel,
    callback_id
  )
  sprintf("(function(el){%s})(this);", send_body)
}

#' @keywords internal
require_render_ctx <- function(fn_name) {
  ctx <- get_hook_context()
  if (is.null(ctx) || !isTRUE(ctx$in_render)) {
    rlang::abort(sprintf(
      "`%s()` must be called inside a component `render()` function.", fn_name
    ))
  }
  ctx
}

#' @keywords internal
auto_bind <- function(ctx, input_id, state_field, transform) {
  ctx$callback_handlers[[input_id]] <- function(s, value) {
    if (!is.null(transform)) {
      value <- transform(value)
    }
    updates <- list(value)
    names(updates) <- state_field
    do.call(s$set, updates)
  }
  invisible(NULL)
}

#' @keywords internal
resolve_value <- function(ctx, value, state_field) {
  if (!is.null(value)) {
    return(value)
  }
  state_get(ctx$state_store, state_field)
}

#' @keywords internal
field_label <- function(label, for_id = NULL) {
  if (is.null(label)) {
    return(NULL)
  }
  shiny::tags$label(label, `for` = for_id)
}

#' @keywords internal
normalize_choices <- function(choices) {
  if (is.null(names(choices))) {
    stats::setNames(as.character(choices), as.character(choices))
  } else {
    stats::setNames(as.character(unname(choices)), names(choices))
  }
}

#' Register a callback that writes event values into state
#'
#' The `bind*()` helpers already register this automatically. Use `useInput()`
#' directly only for advanced cases — e.g. wiring a raw input id to a state
#' field with a custom transform without a bound helper.
#'
#' @param input_id Callback id shared with a `bind*()` helper.
#' @param state_field State field to update. Defaults to `input_id`.
#' @param transform Optional function applied to the incoming value before state
#'   is updated.
#'
#' @export
useInput <- function(input_id, state_field = input_id, transform = NULL) {
  useCallback(
    input_id,
    function(s, value) {
      if (!is.null(transform)) {
        value <- transform(value)
      }
      updates <- list(value)
      names(updates) <- state_field
      do.call(s$set, updates)
    }
  )
}

#' Bound inputs
#'
#' Bound inputs are called **inside** a component `render()` function. They read
#' their current value from component state, wire changes back into state
#' automatically (no separate [useInput()] needed), and are safe across
#' re-renders. The namespace is taken from the active render context, so no `ns`
#' argument is required.
#'
#' @param input_id Callback / state field id.
#' @param label Field label, or `NULL` for none.
#' @param value Current value. Defaults to the value of `state_field` in
#'   component state.
#' @param placeholder Optional placeholder text.
#' @param width Optional CSS width (e.g. `"100%"`).
#' @param update When to sync state: `"input"` (live, default) or `"blur"`.
#' @param state_field State field this input reads and writes. Defaults to
#'   `input_id`.
#' @param transform Optional function applied to the incoming value before it is
#'   written to state.
#'
#' @return A UI tag to include in a component's rendered output.
#' @rdname bindTextInput
#' @export
bindTextInput <- function(input_id, label = NULL, value = NULL, placeholder = NULL,
                          width = NULL, update = c("input", "blur"),
                          state_field = input_id, transform = NULL) {
  ctx <- require_render_ctx("bindTextInput")
  update <- match.arg(update)
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, transform)
  value <- resolve_value(ctx, value, state_field)
  event_js <- dispatch_js(ns, input_id, "el.value")

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "text",
      class = "form-control",
      value = value %||% "",
      placeholder = placeholder,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      `onblur` = if (update == "blur") event_js else NULL,
      `oninput` = if (update == "input") event_js else NULL
    )
  )
}

#' @rdname bindTextInput
#' @param rows Number of rows.
#' @export
bindTextArea <- function(input_id, label = NULL, value = NULL, rows = 3L,
                         placeholder = NULL, width = NULL, update = c("input", "blur"),
                         state_field = input_id, transform = NULL) {
  ctx <- require_render_ctx("bindTextArea")
  update <- match.arg(update)
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, transform)
  value <- resolve_value(ctx, value, state_field)
  event_js <- dispatch_js(ns, input_id, "el.value")

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$textarea(
      id = ns(input_id),
      class = "form-control",
      rows = rows,
      placeholder = placeholder,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      `onblur` = if (update == "blur") event_js else NULL,
      `oninput` = if (update == "input") event_js else NULL,
      value %||% ""
    )
  )
}

#' Bound numeric input
#' @rdname bindTextInput
#' @param min,max,step Numeric constraints for [bindNumericInput()] and
#'   [bindSlider()]; for [bindDateInput()], `min`/`max` are date bounds.
#' @export
bindNumericInput <- function(input_id, label = NULL, value = NULL, min = NA, max = NA,
                             step = NA, width = NULL, update = c("input", "blur"),
                             state_field = input_id, transform = NULL) {
  ctx <- require_render_ctx("bindNumericInput")
  update <- match.arg(update)
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, transform)
  value <- resolve_value(ctx, value, state_field)
  event_js <- dispatch_js(ns, input_id, "parseFloat(el.value)")

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "number",
      class = "form-control",
      value = value,
      min = if (!is.na(min)) min else NULL,
      max = if (!is.na(max)) max else NULL,
      step = if (!is.na(step)) step else NULL,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      `onblur` = if (update == "blur") event_js else NULL,
      `oninput` = if (update == "input") event_js else NULL
    )
  )
}

#' Bound checkbox / toggle
#' @rdname bindTextInput
#' @export
bindCheckbox <- function(input_id, label = NULL, value = NULL, state_field = input_id) {
  ctx <- require_render_ctx("bindCheckbox")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, NULL)
  value <- resolve_value(ctx, value, state_field)

  shiny::div(
    class = "checkbox",
    shiny::tags$label(
      shiny::tags$input(
        type = "checkbox",
        id = ns(input_id),
        checked = if (isTRUE(value)) NA else NULL,
        onclick = dispatch_js(ns, input_id, "el.checked")
      ),
      " ",
      label
    )
  )
}

#' @rdname bindTextInput
#' @export
bindSwitch <- function(input_id, label = NULL, value = NULL, state_field = input_id) {
  ctx <- require_render_ctx("bindSwitch")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, NULL)
  value <- resolve_value(ctx, value, state_field)

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$div(
      class = "form-check form-switch",
      shiny::tags$input(
        class = "form-check-input",
        type = "checkbox",
        role = "switch",
        id = ns(input_id),
        checked = if (isTRUE(value)) NA else NULL,
        onclick = dispatch_js(ns, input_id, "el.checked")
      )
    )
  )
}

#' Bound radio buttons
#' @param choices Named vector of choices.
#' @param selected Currently selected value. Defaults to the value of
#'   `state_field` in component state.
#' @param inline Display options inline.
#' @rdname bindTextInput
#' @export
bindRadioButtons <- function(input_id, label = NULL, choices, selected = NULL,
                             inline = FALSE, state_field = input_id) {
  ctx <- require_render_ctx("bindRadioButtons")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, NULL)
  selected <- resolve_value(ctx, selected, state_field)
  choices <- normalize_choices(choices)

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label),
    shiny::tags$div(
      class = if (inline) "shiny-input-radiogroup shiny-input-inline" else "shiny-input-radiogroup",
      lapply(names(choices), function(choice_label) {
        choice_value <- choices[[choice_label]]
        shiny::tags$label(
          class = "radio-inline",
          shiny::tags$input(
            type = "radio",
            name = ns(input_id),
            value = choice_value,
            checked = if (identical(as.character(selected), choice_value)) NA else NULL,
            onclick = dispatch_js(ns, input_id, "el.value")
          ),
          " ",
          choice_label
        )
      })
    )
  )
}

#' Bound checkbox group
#' @rdname bindTextInput
#' @export
bindCheckboxGroup <- function(input_id, label = NULL, choices, selected = NULL,
                              state_field = input_id) {
  ctx <- require_render_ctx("bindCheckboxGroup")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, NULL)
  selected <- as.character(resolve_value(ctx, selected, state_field) %||% character())
  choices <- normalize_choices(choices)
  group_name <- ns(input_id)
  collect_js <- sprintf(
    "Array.from(document.querySelectorAll('input[name=\"%s\"]:checked')).map(function(el){return el.value;})",
    group_name
  )

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label),
    shiny::tags$div(
      class = "shiny-input-checkboxgroup",
      lapply(names(choices), function(choice_label) {
        choice_value <- choices[[choice_label]]
        shiny::tags$label(
          shiny::tags$input(
            type = "checkbox",
            name = group_name,
            value = choice_value,
            checked = if (choice_value %in% selected) NA else NULL,
            onclick = dispatch_js(ns, input_id, collect_js)
          ),
          " ",
          choice_label
        )
      })
    )
  )
}

#' Bound select input
#' @param multiple Allow multiple selections.
#' @rdname bindTextInput
#' @export
bindSelect <- function(input_id, label = NULL, choices, selected = NULL,
                       multiple = FALSE, width = NULL, state_field = input_id) {
  ctx <- require_render_ctx("bindSelect")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, NULL)
  selected_chr <- as.character(resolve_value(ctx, selected, state_field) %||% character())
  choices <- normalize_choices(choices)

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$select(
      id = ns(input_id),
      class = "form-control",
      multiple = if (isTRUE(multiple)) NA else NULL,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      onchange = dispatch_js(
        ns,
        input_id,
        if (isTRUE(multiple)) {
          sprintf(
            "Array.from(document.getElementById('%s').selectedOptions).map(function(o){return o.value;})",
            ns(input_id)
          )
        } else {
          "el.value"
        }
      ),
      lapply(names(choices), function(choice_label) {
        choice_value <- choices[[choice_label]]
        shiny::tags$option(
          value = choice_value,
          selected = if (choice_value %in% selected_chr) NA else NULL,
          choice_label
        )
      })
    )
  )
}

#' Bound slider
#' @rdname bindTextInput
#' @export
bindSlider <- function(input_id, label = NULL, min, max, value = NULL, step = 1,
                       state_field = input_id) {
  ctx <- require_render_ctx("bindSlider")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, NULL)
  value <- resolve_value(ctx, value, state_field)

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "range",
      class = "form-range",
      min = min,
      max = max,
      step = step,
      value = value,
      `onchange` = dispatch_js(ns, input_id, "parseFloat(el.value)"),
      `oninput` = paste0(
        "var v=this.parentElement.querySelector('.shinystate-slider-value');",
        "if(v){v.textContent=this.value;}"
      )
    ),
    shiny::tags$p(
      class = "help-block",
      "Value: ",
      shiny::tags$span(class = "shinystate-slider-value", value)
    )
  )
}

#' Bound date input
#' @rdname bindTextInput
#' @export
bindDateInput <- function(input_id, label = NULL, value = NULL, min = NULL, max = NULL,
                          state_field = input_id) {
  ctx <- require_render_ctx("bindDateInput")
  ns <- ctx$ns
  auto_bind(ctx, input_id, state_field, function(x) as.Date(x))
  value <- resolve_value(ctx, value, state_field)

  to_date <- function(x) {
    if (inherits(x, "Date")) {
      return(x)
    }
    if (is.character(x) && nzchar(x)) {
      return(as.Date(x))
    }
    Sys.Date()
  }

  current <- to_date(value)
  min_date <- if (!is.null(min)) to_date(min) else NULL
  max_date <- if (!is.null(max)) to_date(max) else NULL

  shiny::div(
    class = "form-group shiny-input-container",
    field_label(label, ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "date",
      class = "form-control",
      value = format(current, "%Y-%m-%d"),
      min = if (!is.null(min_date)) format(min_date, "%Y-%m-%d") else NULL,
      max = if (!is.null(max_date)) format(max_date, "%Y-%m-%d") else NULL,
      onchange = dispatch_js(ns, input_id, "el.value")
    )
  )
}
