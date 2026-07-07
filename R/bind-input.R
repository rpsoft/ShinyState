#' @keywords internal
event_channel_id <- function(ns) {
  ns(".shinystate_event")
}

#' @keywords internal
dispatch_js <- function(ns, callback_id, value_js_expr) {
  sprintf(
    "Shiny.setInputValue('%s',{id:'%s',t:Date.now(),value:%s},{priority:'event'});",
    event_channel_id(ns),
    callback_id,
    value_js_expr
  )
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
#' Pair with the `bind*()` input helpers. The handler receives the parsed event
#' `value` as its second argument.
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

#' Bound text input
#'
#' @param ns Namespace function from `render()`.
#' @param input_id Callback/state field id.
#' @param label Field label.
#' @param value Current value from component state.
#' @param placeholder Optional placeholder text.
#' @param width Optional CSS width (e.g. `"100%"`).
#'
#' @export
bindTextInput <- function(ns, input_id, label, value, placeholder = NULL, width = NULL) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, `for` = ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "text",
      class = "form-control",
      value = value %||% "",
      placeholder = placeholder,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      oninput = dispatch_js(ns, input_id, "this.value")
    )
  )
}

#' Bound text area
#' @rdname bindTextInput
#' @param rows Number of rows.
#' @export
bindTextArea <- function(ns, input_id, label, value, rows = 3L, placeholder = NULL, width = NULL) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, `for` = ns(input_id)),
    shiny::tags$textarea(
      id = ns(input_id),
      class = "form-control",
      rows = rows,
      placeholder = placeholder,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      oninput = dispatch_js(ns, input_id, "this.value"),
      value
    )
  )
}

#' Bound numeric input
#' @rdname bindTextInput
#' @export
bindNumericInput <- function(ns, input_id, label, value, min = NA, max = NA, step = NA, width = NULL) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, `for` = ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "number",
      class = "form-control",
      value = value,
      min = if (!is.na(min)) min else NULL,
      max = if (!is.na(max)) max else NULL,
      step = if (!is.na(step)) step else NULL,
      style = if (!is.null(width)) paste0("width:", width, ";") else NULL,
      oninput = dispatch_js(ns, input_id, "parseFloat(this.value)")
    )
  )
}

#' Bound checkbox / toggle
#' @rdname bindTextInput
#' @export
bindCheckbox <- function(ns, input_id, label, value = FALSE) {
  shiny::div(
    class = "checkbox",
    shiny::tags$label(
      shiny::tags$input(
        type = "checkbox",
        id = ns(input_id),
        checked = if (isTRUE(value)) NA else NULL,
        onclick = dispatch_js(ns, input_id, "this.checked")
      ),
      " ",
      label
    )
  )
}

#' @rdname bindTextInput
#' @export
bindSwitch <- function(ns, input_id, label, value = FALSE) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, `for` = ns(input_id)),
    shiny::tags$div(
      class = "form-check form-switch",
      shiny::tags$input(
        class = "form-check-input",
        type = "checkbox",
        role = "switch",
        id = ns(input_id),
        checked = if (isTRUE(value)) NA else NULL,
        onclick = dispatch_js(ns, input_id, "this.checked")
      )
    )
  )
}

#' Bound radio buttons
#' @param choices Named vector of choices.
#' @param selected Currently selected value.
#' @param inline Display options inline.
#' @rdname bindTextInput
#' @export
bindRadioButtons <- function(ns, input_id, label, choices, selected, inline = FALSE) {
  choices <- normalize_choices(choices)
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label),
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
            onclick = dispatch_js(ns, input_id, "this.value")
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
bindCheckboxGroup <- function(ns, input_id, label, choices, selected = character()) {
  choices <- normalize_choices(choices)
  selected <- as.character(selected)
  group_name <- ns(input_id)
  collect_js <- sprintf(
    "Array.from(document.querySelectorAll('input[name=\"%s\"]:checked')).map(function(el){return el.value;})",
    group_name
  )

  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label),
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
bindSelect <- function(ns, input_id, label, choices, selected, multiple = FALSE, width = NULL) {
  choices <- normalize_choices(choices)
  selected_chr <- as.character(selected)

  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, `for` = ns(input_id)),
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
          "this.value"
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
bindSlider <- function(ns, input_id, label, min, max, value, step = 1) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(label, `for` = ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "range",
      class = "form-range",
      min = min,
      max = max,
      step = step,
      value = value,
      oninput = dispatch_js(ns, input_id, "parseFloat(this.value)")
    ),
    shiny::tags$p(class = "help-block", paste("Value:", value))
  )
}

#' Bound date input
#' @rdname bindTextInput
#' @export
bindDateInput <- function(ns, input_id, label, value, min = NULL, max = NULL) {
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
    shiny::tags$label(label, `for` = ns(input_id)),
    shiny::tags$input(
      id = ns(input_id),
      type = "date",
      class = "form-control",
      value = format(current, "%Y-%m-%d"),
      min = if (!is.null(min_date)) format(min_date, "%Y-%m-%d") else NULL,
      max = if (!is.null(max_date)) format(max_date, "%Y-%m-%d") else NULL,
      onchange = dispatch_js(ns, input_id, "this.value")
    )
  )
}
