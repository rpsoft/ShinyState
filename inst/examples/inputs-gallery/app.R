library(shiny)
library(ShinyState)

inputs_gallery <- component(
  id = "inputs",
  state = useState(
    title = "Project Atlas",
    notes = "Describe your study here...",
    sample_size = 25,
    active = TRUE,
    notifications = FALSE,
    species = "dog",
    colors = c("green", "blue"),
    region = "emea",
    regions = c("emea", "apac"),
    volume = 40,
    start_date = Sys.Date()
  ),
  effect(
    deps = "title",
    function(state) {
      message("Title changed: ", state$title)
    }
  ),
  render = function(state, ns) {
    useInput("title")
    useInput("notes")
    useInput("sample_size")
    useInput("active")
    useInput("notifications")
    useInput("species")
    useInput("colors")
    useInput("region")
    useInput("regions")
    useInput("volume")
    useInput("start_date", transform = function(x) as.Date(x))

    useCallback("reset_form", function(s) {
      s$set(
        title = "Project Atlas",
        notes = "Describe your study here...",
        sample_size = 25,
        active = TRUE,
        notifications = FALSE,
        species = "dog",
        colors = c("green", "blue"),
        region = "emea",
        regions = c("emea", "apac"),
        volume = 40,
        start_date = Sys.Date(),
        .refresh_controls = TRUE
      )
    })

    fluidPage(
      titlePanel("ShinyState Inputs Gallery"),
      fluidRow(
        column(
          width = 6,
          h3("Interactive controls"),
          bindTextInput(ns, "title", "Title", state$title, placeholder = "Enter a title", update = "input"),
          bindTextArea(ns, "notes", "Long text / notes", state$notes, rows = 4, width = "100%", update = "input"),
          bindNumericInput(ns, "sample_size", "Numeric input", state$sample_size, min = 1, max = 500, step = 1, update = "blur"),
          bindSwitch(ns, "active", "Toggle / switch", state$active),
          bindCheckbox(ns, "notifications", "Checkbox", state$notifications),
          bindRadioButtons(
            ns, "species", "Radio buttons",
            choices = c(Cat = "cat", Dog = "dog", Bird = "bird"),
            selected = state$species,
            inline = TRUE
          ),
          bindCheckboxGroup(
            ns, "colors", "Checkbox group",
            choices = c(Red = "red", Green = "green", Blue = "blue", Yellow = "yellow"),
            selected = state$colors
          ),
          bindSelect(
            ns, "region", "Dropdown / select",
            choices = c("North America" = "na", "Europe" = "emea", "Asia Pacific" = "apac"),
            selected = state$region,
            width = "100%"
          ),
          bindSelect(
            ns, "regions", "Multi-select",
            choices = c("North America" = "na", "Europe" = "emea", "Asia Pacific" = "apac", "LATAM" = "latam"),
            selected = state$regions,
            multiple = TRUE,
            width = "100%"
          ),
          bindSlider(ns, "volume", "Slider", min = 0, max = 100, value = state$volume, step = 5),
          bindDateInput(ns, "start_date", "Date input", state$start_date),
          hr(),
          bindButton(ns, "reset_form", "Reset form")
        ),
        column(
          width = 6,
          preview(
            h3("Live preview"),
            p(
              useMemo(
                function() {
                  paste(
                    "Title:", state$title,
                    "| Active:", state$active,
                    "| Species:", state$species,
                    "| Colors:", paste(state$colors, collapse = ", "),
                    "| Volume:", state$volume
                  )
                },
                deps = c("title", "active", "species", "colors", "volume")
              )
            ),
            tags$table(
              class = "table table-striped",
              tags$tbody(
                tags$tr(tags$td("Title"), tags$td(state$title)),
                tags$tr(tags$td("Notes"), tags$td(state$notes)),
                tags$tr(tags$td("Sample size"), tags$td(state$sample_size)),
                tags$tr(tags$td("Active"), tags$td(state$active)),
                tags$tr(tags$td("Notifications"), tags$td(state$notifications)),
                tags$tr(tags$td("Species"), tags$td(state$species)),
                tags$tr(tags$td("Colors"), tags$td(paste(state$colors, collapse = ", "))),
                tags$tr(tags$td("Region"), tags$td(state$region)),
                tags$tr(tags$td("Regions"), tags$td(paste(state$regions, collapse = ", "))),
                tags$tr(tags$td("Volume"), tags$td(state$volume)),
                tags$tr(tags$td("Start date"), tags$td(as.character(state$start_date)))
              )
            ),
            tags$details(
              tags$summary("Raw state JSON"),
              tags$pre(
                if (requireNamespace("jsonlite", quietly = TRUE)) {
                  jsonlite::toJSON(state$all(), auto_unbox = TRUE, pretty = TRUE)
                } else {
                  paste(capture.output(str(state$all())), collapse = "\n")
                }
              )
            )
          )
        )
      )
    )
  }
)

shinyApp(
  ui = mount(inputs_gallery),
  server = function(input, output, session) {
    serve(inputs_gallery, input, output, session)
  }
)
