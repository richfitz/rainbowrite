context("knitr")

test_that("hook is activated", {
  skip_if_not_installed("fansi")
  skip_if_not_installed("rmarkdown")

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  file.copy("example.Rmd", path)
  path_html <- rmarkdown::render(file.path(path, "example.Rmd"), quiet = TRUE)
  expect_true(file.exists(path_html))

  txt <- readLines(path_html)
  expect_match(txt, '<PRE class="fansi fansi-output">',
               all = FALSE, fixed = TRUE)
})
