library(shiny)
library(ShinyState)

# A shared store: state that several components read and write. Components
# subscribe with useStore() and re-render automatically when it changes.
app_store <- createStore(
  count = 0L,
  history = c("Initialized at 0")
)

controls_component <- component(
  id = "controls",
  render = function(state) {
    store <- useStore(app_store)

    bump <- function(delta, verb) {
      next_count <- store$count + delta
      store$set(
        count = next_count,
        history = c(store$history, paste(verb, next_count))
      )
    }

    tagList(
      h3("Controls Component"),
      p(paste("Shared count:", store$count)),
      bindButton("dec", "−", onClick = function(s) bump(-1L, "Decremented to")),
      bindButton("inc", "+", onClick = function(s) bump(1L, "Incremented to")),
      bindButton("reset", "Reset", onClick = function(s) {
        store$set(count = 0L, history = c(store$history, "Reset to 0"))
      })
    )
  }
)

history_component <- component(
  id = "history",
  render = function(state) {
    store <- useStore(app_store)
    recent <- rev(utils::tail(store$history, 8))

    tagList(
      h3("History Component"),
      p(paste("Shared count seen here:", store$count)),
      p(paste("Latest event:", utils::tail(store$history, 1))),
      tags$ul(lapply(recent, tags$li))
    )
  }
)

# Both tabs share app_store; changes on one are reflected on the other.
shinyStateApp(
  title = "ShinyState Shared Store Example",
  controls = controls_component,
  history = history_component
)
