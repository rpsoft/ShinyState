test_that("createStore get/set/subscribe work", {
  store <- createStore(count = 0L, label = "a")
  expect_equal(store$get("count"), 0L)
  expect_equal(store$get()$label, "a")

  fired <- 0L
  unsub <- store$subscribe(function() fired <<- fired + 1L)
  store$set(count = 5L)
  expect_equal(store$get("count"), 5L)
  expect_equal(fired, 1L)

  unsub()
  store$set(count = 6L)
  expect_equal(fired, 1L)
})

test_that("useStore re-renders subscribing components and shares state", {
  store <- createStore(count = 0L)

  writer <- component(
    id = "writer",
    render = function(state) {
      s <- useStore(store)
      bindButton("inc", "+", onClick = function(x) s$set(count = s$count + 1L))
    }
  )
  reader <- component(
    id = "reader",
    render = function(state) {
      s <- useStore(store)
      shiny::p(paste("count:", s$count), class = "reader")
    }
  )

  shiny::testServer(
    function(input, output, session) {
      serve(writer, input, output, session)
      serve(reader, input, output, session)
    },
    {
      session$flushReact()
      html <- htmltools::renderTags(output$`reader-ui`)$html
      expect_match(html, "count: 0")

      session$setInputs(`writer-.shinystate_event` = list(id = "inc", t = 1))
      session$flushReact()
      html <- htmltools::renderTags(output$`reader-ui`)$html
      expect_match(html, "count: 1")
    }
  )
})
