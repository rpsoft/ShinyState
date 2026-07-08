library(shiny)
library(ShinyState)

counter_component <- component(
  id = "counter",
  state = useState(count = 0L),
  effect(
    deps = "count",
    function(state) {
      message("Counter changed to: ", state$count)
    }
  ),
  render = function(state) {
    doubled <- useMemo(function() state$count * 2L, deps = "count")

    tagList(
      h3("Counter Component"),
      p(paste("Count:", state$count)),
      p(paste("Doubled:", doubled)),
      bindButton("dec", "−", onClick = function(s) s$set(count = s$count - 1L)),
      bindButton("inc", "+", onClick = function(s) s$set(count = s$count + 1L)),
      bindButton("reset", "Reset", onClick = function(s) s$set(count = 0L)),
      p(route_link("search", "Go to search →"))
    )
  }
)

filter_component <- component(
  id = "filter",
  state = useState(query = "a", page = 1L),
  effect(
    deps = "query",
    function(state) {
      state$set(page = 1L)
    }
  ),
  render = function(state) {
    items <- c("Alfa", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot")
    filtered <- items[grepl(state$query, items, ignore.case = TRUE)]
    page_size <- 2L
    start <- (state$page - 1L) * page_size + 1L
    end <- min(length(filtered), start + page_size - 1L)
    current <- if (length(filtered) == 0L || start > length(filtered)) character() else filtered[start:end]

    tagList(
      h3("Search + Pagination Component"),
      p(paste("Current filter:", if (nzchar(state$query)) state$query else "(all)")),
      bindButton("filter_a", "Contains 'a'", onClick = function(s) s$set(query = "a")),
      bindButton("filter_e", "Contains 'e'", onClick = function(s) s$set(query = "e")),
      bindButton("filter_all", "Show all", onClick = function(s) s$set(query = "")),
      p(paste("Current page:", state$page)),
      if (length(current) == 0L) p("No matches.") else tags$ul(lapply(current, tags$li)),
      bindButton("prev_page", "Previous", onClick = function(s) s$set(page = max(1L, s$page - 1L))),
      bindButton("next_page", "Next", onClick = function(s) s$set(page = s$page + 1L))
    )
  }
)

reducer_component <- component(
  id = "reducer",
  render = function(state) {
    reducer <- useReducer(
      function(prev, action) {
        switch(
          action,
          add = c(prev, sprintf("Task %d", length(prev) + 1L)),
          drop = if (length(prev) > 0L) prev[-length(prev)] else prev,
          prev
        )
      },
      initial = character()
    )

    tagList(
      h3("Reducer Component"),
      p(paste("Items:", length(reducer$state))),
      if (length(reducer$state) == 0L) p("No items yet.") else tags$ul(lapply(reducer$state, tags$li)),
      bindButton("add", "Add item", onClick = function(s) reducer$dispatch("add")),
      bindButton("drop", "Remove last", onClick = function(s) reducer$dispatch("drop"))
    )
  }
)

shinyApp(
  ui = navbarPage(
    "ShinyState Multipage Example",
    id = "pages",
    tabPanel("Dashboard", mount(counter_component), value = "dashboard"),
    tabPanel("Search", mount(filter_component), value = "search"),
    tabPanel("Reducer", mount(reducer_component), value = "reducer")
  ),
  server = function(input, output, session) {
    # routing = "hash" makes tabs bookmarkable (#!/dashboard, #!/search, ...)
    # and wires the browser back/forward buttons to tab navigation.
    serve_dormant(
      session = session, input = input, output = output,
      navbar = "pages",
      routing = "hash",
      dashboard = counter_component,
      search = filter_component,
      reducer = reducer_component
    )
  }
)
