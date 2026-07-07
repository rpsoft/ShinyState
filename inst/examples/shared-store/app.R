library(shiny)
library(ShinyState)

# Minimal shared-store pattern for ShinyState components.
createStore <- function(initial) {
  store <- new.env(parent = emptyenv())
  store$data <- initial
  store$listeners <- list()

  store$get <- function() {
    store$data
  }

  store$set <- function(...) {
    updates <- list(...)
    for (nm in names(updates)) {
      store$data[[nm]] <- updates[[nm]]
    }
    for (id in names(store$listeners)) {
      listener <- store$listeners[[id]]
      if (is.function(listener)) {
        listener(store$data)
      }
    }
    invisible(store$data)
  }

  store$subscribe <- function(listener) {
    id <- paste0(as.integer(as.numeric(Sys.time()) * 1000), "_", sample.int(1000, 1))
    store$listeners[[id]] <- listener
    function() {
      store$listeners[[id]] <<- NULL
      invisible(NULL)
    }
  }

  store
}

shared_store <- createStore(
  list(
    count = 0L,
    history = c("Initialized at 0")
  )
)

controls_component <- component(
  id = "controls",
  state = useState(count = 0L),
  effect(
    deps = NULL,
    function(state) {
      # Sync local component state with global store.
      unsubscribe <- shared_store$subscribe(function(data) {
        state$set(count = data$count)
      })
      state$set(count = shared_store$get()$count)
      unsubscribe
    }
  ),
  render = function(state, ns) {
    useCallback("inc", function(s) {
      current <- shared_store$get()
      next_count <- current$count + 1L
      shared_store$set(
        count = next_count,
        history = c(current$history, paste("Incremented to", next_count))
      )
    })

    useCallback("dec", function(s) {
      current <- shared_store$get()
      next_count <- current$count - 1L
      shared_store$set(
        count = next_count,
        history = c(current$history, paste("Decremented to", next_count))
      )
    })

    useCallback("reset", function(s) {
      current <- shared_store$get()
      shared_store$set(
        count = 0L,
        history = c(current$history, "Reset to 0")
      )
    })

    tagList(
      h3("Controls Component"),
      p(paste("Shared count:", state$count)),
      bindButton(ns, "dec", "−"),
      bindButton(ns, "inc", "+"),
      bindButton(ns, "reset", "Reset")
    )
  }
)

history_component <- component(
  id = "history",
  state = useState(count = 0L, history = c("Waiting for updates...")),
  effect(
    deps = NULL,
    function(state) {
      unsubscribe <- shared_store$subscribe(function(data) {
        state$set(count = data$count, history = data$history)
      })
      data <- shared_store$get()
      state$set(count = data$count, history = data$history)
      unsubscribe
    }
  ),
  render = function(state, ns) {
    latest <- if (length(state$history) == 0L) "No events yet." else tail(state$history, 1)
    recent <- rev(utils::tail(state$history, 8))

    tagList(
      h3("History Component"),
      p(paste("Shared count seen here:", state$count)),
      p(paste("Latest event:", latest)),
      tags$ul(lapply(recent, tags$li))
    )
  }
)

shinyApp(
  ui = navbarPage(
    "ShinyState Shared Store Example",
    tabPanel("Controls", mount(controls_component)),
    tabPanel("History", mount(history_component))
  ),
  server = function(input, output, session) {
    serve(controls_component, input, output, session)
    serve(history_component, input, output, session)
  }
)
