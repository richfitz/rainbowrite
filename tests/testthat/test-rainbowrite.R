context("rainbowrite")

test_that("construction", {
  x <- lol$new()
  expect_null(r6_private(x)$last)
  expect_equal(r6_private(x)$row, 0L)
  expect_equal(r6_private(x)$col, 0L)
})


test_that("can change angle", {
  xd <- lol$new()
  xh <- lol$new(angle = 0)
  xv <- lol$new(angle = pi / 2)
  n <- 78L
  text <- strrep("x", n)

  invisible({
    xd$render(text)
    xh$render(text)
    xv$render(text)
  })

  ld <- r6_private(xd)$last
  lh <- r6_private(xh)$last
  lv <- r6_private(xv)$last

  expect_equal(ld$row, 0L)
  expect_equal(ld$col, 0:(n - 1))
  expect_true(ld$reset)
  v <- c("row", "col", "reset")

  expect_identical(lh[v], ld[v])
  expect_equal(lh$p, rep(0, n))

  expect_equal(lv$p * (sqrt(2) / 2), ld$p)
  expect_identical(lv[v], ld[v])
})


test_that("can render without newlines", {
  x <- lol$new()
  invisible(
    x$render(c("aaa", "bbbb", "ccccc"), reset = FALSE))
  last <- r6_private(x)$last
  expect_equal(last$row, 2L)
  expect_equal(last$col, 0:4)
  expect_false(last$reset)
  expect_equal(r6_private(x)$row, 2L)
  expect_equal(r6_private(x)$col, 5L)
  p1 <- last$p

  invisible(
    x$render_line("dddddd", reset = FALSE))
  last <- r6_private(x)$last
  expect_equal(last$row, 2L)
  expect_equal(last$col, 5:10)
  expect_false(last$reset)
  expect_equal(r6_private(x)$row, 2L)
  expect_equal(r6_private(x)$col, 11L)
  p2 <- last$p

  invisible(
    x$render_line("eee", reset = TRUE))
  last <- r6_private(x)$last
  expect_equal(last$row, 2L)
  expect_equal(last$col, 11:13)
  expect_true(last$reset)
  expect_equal(r6_private(x)$row, 3L)
  expect_equal(r6_private(x)$col, 0L)
  p3 <- last$p

  expect_equal(c(p1, p2, p3),
               seq(p1[[1]], p3[[length(p3)]], length.out = 14))
})


test_that("Can render a newline", {
  x <- lol$new()
  x$render("aaa", reset = FALSE)
  expect_equal(r6_private(x)$row, 0L)
  expect_equal(r6_private(x)$col, 3L)
  expect_null(x$render(character(0), TRUE))
  expect_equal(r6_private(x)$last,
               list(row = 0L, col = integer(0), p = numeric(0), reset = TRUE))
  expect_equal(r6_private(x)$row, 1L)
  expect_equal(r6_private(x)$col, 0L)
})


test_that("Can set a new default", {
  default_reset()
  x <- lol$new()
  default <- pkg$default
  expect_false(identical(x, pkg$default))
  prev <- x$set_as_default()
  expect_identical(x, pkg$default)
  expect_identical(prev, default)

  prev2 <- prev$set_as_default()
  expect_identical(default, pkg$default)
  expect_identical(prev2, x)
})
