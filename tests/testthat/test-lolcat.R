context("lolcat")


test_that("lolcat produces lolified output", {
  x <- lol$new()
  res <- with_options(
    list(crayon.enabled = TRUE),
    capture.output(lolcat("Hello", lol = x)))
  expect_equal(crayon::strip_style(res), "Hello")
  last <- r6_private(x)$last

  expect_false(last$reset)
  expect_equal(last$row, 0L)
  expect_equal(last$col, 0:4)

  res <- with_options(
    list(crayon.enabled = TRUE),
    capture.output(lolcat(" World!\n", lol = x)))
  expect_equal(crayon::strip_style(res), " World!")
  last <- r6_private(x)$last

  expect_true(last$reset)
  expect_equal(last$row, 0L)
  expect_equal(last$col, 5:11)
})


test_that("lolmessage produces lolified output", {
  x <- lol$new()
  res <- with_options(
    list(crayon.enabled = TRUE),
    capture_messages(lolmessage("Hello", lol = x)))
  expect_equal(crayon::strip_style(res), "Hello\n")
  last <- r6_private(x)$last

  expect_true(last$reset)
  expect_equal(last$row, 0L)
  expect_equal(last$col, 0:4)

  res <- with_options(
    list(crayon.enabled = TRUE),
    capture_messages(lolmessage("World!", appendLF = FALSE, lol = x)))
  expect_equal(crayon::strip_style(res), "World!")
  last <- r6_private(x)$last

  expect_false(last$reset)
  expect_equal(last$row, 1L)
  expect_equal(last$col, 0:5)
})


test_that("lolprint produces lolified output", {
  x <- lol$new()
  obj <- runif(20)
  res <- with_options(
    list(crayon.enabled = TRUE),
    capture_output(lolprint(obj, lol = x)))
  cmp <- capture_output(print(obj))
  cmp_lines <- strsplit(cmp, "\n")[[1L]]
  expect_equal(
    crayon::strip_style(res),
    cmp)
  expect_true(crayon::has_style(res))
  last <- r6_private(x)$last
  expect_true(last$reset)
  expect_equal(last$row, length(cmp_lines) - 1L)
  expect_equal(last$col, seq_len(nchar(cmp_lines[[length(cmp_lines)]])) - 1L)
})


test_that("Don't lol to file", {
  path <- tempfile()
  x <- lol$new()
  res <- with_options(
    list(crayon.enabled = TRUE),
    capture_output(lolcat("Hello\n", file = path, lol = x)))
  expect_identical(res, "")
  expect_identical(readLines(path), "Hello")
})


test_that("Find default renderer", {
  default_reset()
  default <- pkg$default
  other <- lol$new()

  f <- function(x) {
    r6_private(x)$row
  }

  start <- f(default)
  render("aaa", NULL, TRUE)
  expect_equal(f(default), start + 1L)
  expect_equal(f(other), 0L)

  render("aaa", default, TRUE)
  expect_equal(f(default), start + 2L)
  expect_equal(f(other), 0L)

  render("aaa", other, TRUE)
  expect_equal(f(default), start + 2L)
  expect_equal(f(other), 1L)
})


test_that("Reset package", {
  default <- pkg$default
  default_reset()
  expect_false(identical(default, pkg$default))
})


test_that("Set default as needed", {
  pkg$default <- NULL
  res <- with_options(
    list(crayon.enabled = TRUE),
    capture_messages(lolmessage("hello")))
  expect_true(crayon::has_style(res))
  expect_s3_class(pkg$default, "lol")
})


test_that("mutiline message renders correctly", {
  str <- "a\nmultiline\nmessage"
  res <- with_colour(capture_messages(lolmessage(str)))
  expect_true(crayon::has_style(res))
  expect_equal(crayon::strip_style(res), paste0(str, "\n"))
})
