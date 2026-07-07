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
  render = function(state, ns) {
    useCallback("inc", function(s) s$set(count = s$count + 1L))
    useCallback("dec", function(s) s$set(count = s$count - 1L))
    useCallback("reset", function(s) s$set(count = 0L))

    doubled <- useMemo(function() state$count * 2L, deps = "count")

    tagList(
      h3("Counter Component"),
      p(paste("Count:", state$count)),
      p(paste("Doubled:", doubled)),
      bindButton(ns, "dec", "−"),
      bindButton(ns, "inc", "+"),
      bindButton(ns, "reset", "Reset")
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
  render = function(state, ns) {
    useCallback("filter_a", function(s) s$set(query = "a"))
    useCallback("filter_e", function(s) s$set(query = "e"))
    useCallback("filter_all", function(s) s$set(query = ""))
    useCallback("next_page", function(s) s$set(page = s$page + 1L))
    useCallback("prev_page", function(s) s$set(page = max(1L, s$page - 1L)))

    items <- c("Alfa", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot")
    filtered <- items[grepl(state$query, items, ignore.case = TRUE)]
    page_size <- 2L
    start <- (state$page - 1L) * page_size + 1L
    end <- min(length(filtered), start + page_size - 1L)
    current <- if (length(filtered) == 0L || start > length(filtered)) character() else filtered[start:end]

    tagList(
      h3("Search + Pagination Component"),
      p(paste("Current filter:", if (nzchar(state$query)) state$query else "(all)")),
      bindButton(ns, "filter_a", "Contains 'a'"),
      bindButton(ns, "filter_e", "Contains 'e'"),
      bindButton(ns, "filter_all", "Show all"),
      p(paste("Current page:", state$page)),
      if (length(current) == 0L) p("No matches.") else tags$ul(lapply(current, tags$li)),
      bindButton(ns, "prev_page", "Previous"),
      bindButton(ns, "next_page", "Next")
    )
  }
)

reducer_component <- component(
  id = "reducer",
  render = function(state, ns) {
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

    useCallback("add", function() reducer$dispatch("add"))
    useCallback("drop", function() reducer$dispatch("drop"))

    tagList(
      h3("Reducer Component"),
      p(paste("Items:", length(reducer$state))),
      if (length(reducer$state) == 0L) p("No items yet.") else tags$ul(lapply(reducer$state, tags$li)),
      bindButton(ns, "add", "Add item"),
      bindButton(ns, "drop", "Remove last")
    )
  }
)

shinyApp(
  ui = navbarPage(
    "ShinyState Multipage Example",
    tabPanel("Dashboard", mount(counter_component)),
    tabPanel("Search", mount(filter_component)),
    tabPanel("Reducer", mount(reducer_component))
  ),
  server = function(input, output, session) {
    serve(counter_component, input, output, session)
    serve(filter_component, input, output, session)
    serve(reducer_component, input, output, session)
  }
)
