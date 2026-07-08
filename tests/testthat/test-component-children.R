make_parent_child <- function() {
  child <- component(
    id = "child",
    state = useState(clicks = 0L),
    render = function(state) {
      shiny::tagList(
        shiny::p(paste("name:", state$name), class = "child-name"),
        shiny::p(paste("clicks:", state$clicks), class = "child-clicks"),
        bindButton("bump", "bump", onClick = function(s) s$set(clicks = s$clicks + 1L))
      )
    }
  )
  parent <- component(
    id = "parent",
    state = useState(selected = "Ada"),
    render = function(state) {
      shiny::tagList(
        shiny::p(paste("parent:", state$selected), class = "parent-name"),
        mount(child, props = list(name = state$selected)),
        bindButton("rename", "rename", onClick = function(s) s$set(selected = "Grace"))
      )
    }
  )
  list(parent = parent, child = child)
}

test_that("a child component renders inside its parent with props", {
  cmps <- make_parent_child()
  shiny::testServer(
    function(input, output, session) {
      serve(cmps$parent, input, output, session)
    },
    {
      session$flushReact()
      session$flushReact()

      parent_html <- htmltools::renderTags(output$`parent-ui`)$html
      expect_match(parent_html, "parent: Ada")
      expect_match(parent_html, "shinystate-output") # child placeholder

      child_html <- htmltools::renderTags(output$`parent-child-ui`)$html
      expect_match(child_html, "name: Ada")
      expect_match(child_html, "clicks: 0")
    }
  )
})

test_that("prop changes re-render the child while its own state survives", {
  cmps <- make_parent_child()
  shiny::testServer(
    function(input, output, session) {
      serve(cmps$parent, input, output, session)
    },
    {
      session$flushReact()
      session$flushReact()

      # Child updates its own state.
      session$setInputs(`parent-child-.shinystate_event` = list(id = "bump", t = 1))
      session$flushReact()
      child_html <- htmltools::renderTags(output$`parent-child-ui`)$html
      expect_match(child_html, "clicks: 1")

      # Parent re-renders with a new prop value.
      session$setInputs(`parent-.shinystate_event` = list(id = "rename", t = 2))
      session$flushReact()
      session$flushReact()
      child_html <- htmltools::renderTags(output$`parent-child-ui`)$html
      expect_match(child_html, "name: Grace")
      expect_match(child_html, "clicks: 1") # child state preserved
    }
  )
})
