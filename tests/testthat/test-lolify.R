context("lolify")

test_that("Can lolify a function", {
  surround2 <- lolify(surround)
  expect_true(is_lolified(surround2))
  expect_false(is_lolified(surround))

  cmp <- capture_messages(surround("hello"))
  res <- with_options(
    list(crayon.enabled = TRUE),
    capture_messages(surround2("hello")))
  expect_equal(crayon::strip_style(res), cmp)
  expect_true(crayon::has_style(res))

  expect_identical(unlolify(surround2), surround)
})


test_that("Don't lolify twice", {
  surround2 <- lolify(surround)
  expect_error(lolify(surround2),
               "Function is already lolified")
  expect_error(unlolify(surround),
               "This function does not look lolified")

})


test_that("Don't lolify with impossible args", {
  surround <- function(x) {
    border <- strrep("*", nchar(x) + 8)
    message(paste(c(border, sprintf("*** %s ***", x), border), collapse = "\n"))
  }
  expect_error(
    lolify(surround, "other"),
    "Invalid value for 'which': 'other'")
  expect_error(
    lolify(surround, c("cat", "other", "apple", "print")),
    "Invalid value for 'which': 'other', 'apple'")
})
