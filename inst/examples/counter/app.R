library(shiny)
library(ShinyState)

counter <- component(
  id = "counter",
  state = useState(count = 0L),
  effect(
    deps = "count",
    function(state) {
      message("Count changed to: ", state$count)
    }
  ),
  render = function(state) {
    doubled <- useMemo(
      function() state$count * 2L,
      deps = "count"
    )

    tagList(
      h2("ShinyState Counter"),
      h3(paste("Count:", state$count)),
      p(paste("Doubled (useMemo):", doubled)),
      bindButton("dec", "−", onClick = function(s) s$set(count = s$count - 1L)),
      bindButton("inc", "+", onClick = function(s) s$set(count = s$count + 1L)),
      bindButton("reset", "Reset", onClick = function(s) s$set(count = 0L))
    )
  }
)

shinyApp(
  ui = fluidPage(
    titlePanel("ShinyState Example"),
    mount(counter)
  ),
  server = function(input, output, session) {
    serve(counter, input, output, session)
  }
)
