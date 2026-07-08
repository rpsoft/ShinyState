library(shiny)
library(ShinyState)

# A reusable child component. It reads `name` and `role` from props passed by
# its parent, and owns its own local state (a like counter) that survives
# parent re-renders.
user_card <- component(
  id = "user_card",
  state = useState(likes = 0L),
  render = function(state) {
    div(
      class = "well",
      style = "padding:1em;margin:.5em 0;border:1px solid #ddd;border-radius:6px;",
      h4(state$name),
      p(tags$em(state$role)),
      p(paste("Likes:", state$likes)),
      bindButton("like", "👍 Like", onClick = function(s) s$set(likes = s$likes + 1L))
    )
  }
)

# The parent selects which user to show and passes props down to the child.
app <- component(
  id = "app",
  state = useState(selected = "ada"),
  computed = list(
    profile = function(state) {
      people <- list(
        ada = list(name = "Ada Lovelace", role = "Mathematician"),
        grace = list(name = "Grace Hopper", role = "Rear Admiral"),
        alan = list(name = "Alan Turing", role = "Logician")
      )
      people[[state$selected]]
    }
  ),
  render = function(state) {
    tagList(
      h2("Child components with props"),
      bindSelect(
        "selected", "Show profile",
        choices = c("Ada Lovelace" = "ada", "Grace Hopper" = "grace", "Alan Turing" = "alan")
      ),
      mount(
        user_card,
        props = list(name = state$profile$name, role = state$profile$role)
      ),
      p(tags$small("The like count is the child's own state and survives switching profiles."))
    )
  }
)

shinyStateApp(app, title = "ShinyState Children Example")
