# ShinyState Tutorial

This folder contains a slide presentation introducing the ShinyState package.

## View the slides

### Option 1: Render locally

```r
install.packages(c("rmarkdown", "revealjs"))
rmarkdown::render(
  system.file("tutorial/shinystate-tutorial.Rmd", package = "ShinyState")
)
```

Open the generated `shinystate-tutorial.html` in your browser.

### Option 2: Render from source tree

```r
rmarkdown::render("inst/tutorial/shinystate-tutorial.Rmd")
browseURL("inst/tutorial/shinystate-tutorial.html")
```

### Option 3: Present directly from RStudio

Open `shinystate-tutorial.Rmd` and click **Knit** (with `revealjs` installed).

## What the tutorial covers

- Why ShinyState exists
- Components and hooks (`useState`, `effect`, `useMemo`, `useReducer`, `useCallback`)
- `bindButton()` and render safety
- Multi-page apps with multiple components
- Shared state across components
- Bundled examples and troubleshooting
