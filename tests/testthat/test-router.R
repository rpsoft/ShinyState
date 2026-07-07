test_that("parse_route_hash handles anchors, routes, and encoded segments", {
  expect_null(parse_route_hash(NULL)$page)
  expect_null(parse_route_hash("")$page)
  expect_null(parse_route_hash(NA_character_)$page)
  expect_null(parse_route_hash("#section")$page)
  expect_null(parse_route_hash("#!/")$page)

  p <- parse_route_hash("#!/dashboard")
  expect_equal(p$page, "dashboard")
  expect_equal(p$parts, "dashboard")

  p <- parse_route_hash("#!/search/a%20b/x")
  expect_equal(p$page, "search")
  expect_equal(p$parts, c("search", "a b", "x"))
})

test_that("format_route_hash round-trips with parse_route_hash", {
  expect_equal(format_route_hash("dashboard"), "#!/dashboard")
  page <- "a b"
  expect_equal(parse_route_hash(format_route_hash(page))$page, page)
})

test_that("route_link renders a hash anchor", {
  html <- as.character(route_link("search"))
  expect_match(html, 'href="#!/search"', fixed = TRUE)
  expect_match(html, ">search<")
  html <- as.character(route_link("search", "Go!"))
  expect_match(html, ">Go!<")
})

test_that("tab changes write the hash: first replace, then push, echoes ignored", {
  pushes <- list()
  tab_updates <- list()
  local_mocked_bindings(
    push_hash = function(session, hash, mode) {
      pushes[[length(pushes) + 1L]] <<- list(hash = hash, mode = mode)
    },
    update_tab = function(session, navbar, page) {
      tab_updates[[length(tab_updates) + 1L]] <<- page
    }
  )

  hash_rv <- shiny::reactiveVal(NULL)
  shiny::testServer(
    function(input, output, session) {
      router_server(
        session, input, navbar = "pages",
        tabs = c("dashboard", "search"), default = "dashboard",
        hash = hash_rv
      )
    },
    {
      session$setInputs(pages = "dashboard")
      expect_length(pushes, 1L)
      expect_equal(pushes[[1L]], list(hash = "#!/dashboard", mode = "replace"))

      session$setInputs(pages = "search")
      expect_length(pushes, 2L)
      expect_equal(pushes[[2L]], list(hash = "#!/search", mode = "push"))

      hash_rv("#!/search")
      session$flushReact()
      expect_length(pushes, 2L)
      expect_length(tab_updates, 0L)
    }
  )
})

test_that("deep link navigates on init and swallows the stale initial tab", {
  pushes <- list()
  tab_updates <- list()
  local_mocked_bindings(
    push_hash = function(session, hash, mode) {
      pushes[[length(pushes) + 1L]] <<- list(hash = hash, mode = mode)
    },
    update_tab = function(session, navbar, page) {
      tab_updates[[length(tab_updates) + 1L]] <<- page
    }
  )

  hash_rv <- shiny::reactiveVal("#!/search")
  route <- NULL
  shiny::testServer(
    function(input, output, session) {
      route <<- router_server(
        session, input, navbar = "pages",
        tabs = c("dashboard", "search"), default = "dashboard",
        hash = hash_rv
      )
    },
    {
      expect_equal(tab_updates, list("search"))
      expect_equal(route$page(), "search")

      session$setInputs(pages = "dashboard")
      expect_length(pushes, 0L)

      session$setInputs(pages = "search")
      expect_length(pushes, 0L)
      expect_length(tab_updates, 1L)
    }
  )
})

test_that("hash changes navigate tabs without phantom history entries", {
  pushes <- list()
  tab_updates <- list()
  local_mocked_bindings(
    push_hash = function(session, hash, mode) {
      pushes[[length(pushes) + 1L]] <<- list(hash = hash, mode = mode)
    },
    update_tab = function(session, navbar, page) {
      tab_updates[[length(tab_updates) + 1L]] <<- page
    }
  )

  hash_rv <- shiny::reactiveVal(NULL)
  shiny::testServer(
    function(input, output, session) {
      router_server(
        session, input, navbar = "pages",
        tabs = c("dashboard", "search"), default = "dashboard",
        hash = hash_rv
      )
    },
    {
      session$setInputs(pages = "dashboard")
      session$setInputs(pages = "search")
      n_pushes <- length(pushes)

      hash_rv("#!/dashboard")
      session$flushReact()
      expect_equal(tab_updates, list("dashboard"))

      session$setInputs(pages = "dashboard")
      expect_length(pushes, n_pushes)
    }
  )
})

test_that("unknown routes warn once and fall back to the default page", {
  pushes <- list()
  tab_updates <- list()
  local_mocked_bindings(
    push_hash = function(session, hash, mode) {
      pushes[[length(pushes) + 1L]] <<- list(hash = hash, mode = mode)
    },
    update_tab = function(session, navbar, page) {
      tab_updates[[length(tab_updates) + 1L]] <<- page
    }
  )

  hash_rv <- shiny::reactiveVal(NULL)
  shiny::testServer(
    function(input, output, session) {
      router_server(
        session, input, navbar = "pages",
        tabs = c("dashboard", "search"), default = "dashboard",
        hash = hash_rv
      )
    },
    {
      session$setInputs(pages = "dashboard")

      expect_warning(
        {
          hash_rv("#!/bogus")
          session$flushReact()
        },
        "no known page"
      )
      expect_equal(pushes[[length(pushes)]], list(hash = "#!/dashboard", mode = "replace"))
      expect_length(tab_updates, 0L)

      hash_rv("#!/search")
      session$flushReact()
      expect_no_warning({
        hash_rv("#!/bogus")
        session$flushReact()
      })
      expect_equal(pushes[[length(pushes)]], list(hash = "#!/dashboard", mode = "replace"))
    }
  )
})

test_that("plain anchors are ignored and extra route segments are preserved", {
  pushes <- list()
  tab_updates <- list()
  local_mocked_bindings(
    push_hash = function(session, hash, mode) {
      pushes[[length(pushes) + 1L]] <<- list(hash = hash, mode = mode)
    },
    update_tab = function(session, navbar, page) {
      tab_updates[[length(tab_updates) + 1L]] <<- page
    }
  )

  hash_rv <- shiny::reactiveVal(NULL)
  route <- NULL
  shiny::testServer(
    function(input, output, session) {
      route <<- router_server(
        session, input, navbar = "pages",
        tabs = c("dashboard", "search"), default = "dashboard",
        hash = hash_rv
      )
    },
    {
      session$setInputs(pages = "dashboard")
      n_pushes <- length(pushes)

      hash_rv("#footnote")
      session$flushReact()
      expect_length(pushes, n_pushes)
      expect_length(tab_updates, 0L)

      hash_rv("#!/search/extra%20bit")
      session$flushReact()
      expect_equal(tab_updates, list("search"))
      expect_equal(route$parts(), c("search", "extra bit"))

      session$setInputs(pages = "search")
      expect_length(pushes, n_pushes)
    }
  )
})

test_that("serve_dormant routing='hash' returns a router handle and tracks tabs", {
  pushes <- list()
  local_mocked_bindings(
    push_hash = function(session, hash, mode) {
      pushes[[length(pushes) + 1L]] <<- list(hash = hash, mode = mode)
    },
    update_tab = function(session, navbar, page) NULL
  )

  counter <- component(
    id = "counter",
    state = useState(count = 0L),
    render = function(state, ns) {
      shiny::p(state$count)
    }
  )
  other <- component(
    id = "other",
    state = useState(label = "idle"),
    render = function(state, ns) {
      shiny::p(state$label)
    }
  )

  route <- NULL
  shiny::testServer(
    function(input, output, session) {
      route <<- serve_dormant(
        session = session, input = input, output = output,
        navbar = "pages",
        routing = "hash",
        dashboard = counter,
        search = other
      )
    },
    {
      session$setInputs(pages = "dashboard")
      session$flushReact()
      expect_equal(route$page(), "dashboard")
      expect_equal(pushes[[1L]], list(hash = "#!/dashboard", mode = "replace"))

      session$setInputs(pages = "search")
      session$flushReact()
      expect_equal(route$page(), "search")
      expect_equal(pushes[[2L]], list(hash = "#!/search", mode = "push"))
    }
  )
})
