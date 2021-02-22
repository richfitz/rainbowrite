context("util")

test_that("make_palette returns correct colours", {
  pal <- c("#ff0000", "#00ff00", "#ff0000")
  f <- make_palette(pal)
  expect_is(f, "function")
  p <- seq(0, 1, length.out = 11)
  v <- f(p)
  expect_match(v, "^#[[:xdigit:]]{6}$", all = TRUE)
  expect_equal(v,
               c("#FF0000", "#CC3300", "#996600", "#659900", "#32CC00",
                 "#00FF00", "#33CB00", "#669800", "#996500", "#CC3200",
                 "#FF0000"))
})


test_that("apply colours returns reasonable sequence", {
  pal <- c("#ff0000", "#00ff00", "#ff0000")
  f <- make_palette(pal)
  v <- f(seq(0, 1, length.out = 11))
  x <- paste(letters[seq_along(v)], collapse = "")

  res <- with_options(
    list(crayon.enabled = TRUE, crayon.colors = 256), {
      crayon::num_colors(forget = TRUE)
      apply_colour(x, v)
    })

  expect_true(crayon::has_style(res))
  expect_equal(crayon::strip_style(res), x)

  expected <- paste0(
    "\033[38;5;196ma\033[39m\033[38;5;166mb\033[39m\033[38;5;136mc",
    "\033[39m\033[38;5;106md\033[39m\033[38;5;76me\033[39m\033[38;5;46mf",
    "\033[39m\033[38;5;76mg\033[39m\033[38;5;106mh\033[39m\033[38;5;136mi",
    "\033[39m\033[38;5;166mj\033[39m\033[38;5;196mk\033[39m")
  expect_equal(res, expected,
               fixed = TRUE)
})


test_that("apply colours does not colour when unavailable", {
  pal <- c("#ff0000", "#00ff00", "#ff0000")
  f <- make_palette(pal)
  v <- f(seq(0, 1, length.out = 11))
  x <- paste(letters[seq_along(v)], collapse = "")

  res <- with_options(
    list(crayon.enabled = FALSE),
    apply_colour(x, v))
  expect_identical(res, x)
})
