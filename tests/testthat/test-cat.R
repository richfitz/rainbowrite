source("helper-rainbowrite.R")
context("cat")

cat_prepare <- rainbowrite:::cat_prepare
capture <- rainbowrite:::capture

test_that("boostrapping", {
  expect_that(capture(cat("hello world\n")),
              equals("hello world\n"))
  expect_that(capture(cat("hello world")),
              equals("hello world"))
})

test_that("Trailing newlines", {
  str <- "foo"
  expect_that(capture(cat(str)),
              equals(cat_prepare(str)))
  expect_that(capture(cat(str, sep="\n")),
              equals(cat_prepare(str, sep="\n")))
  expect_that(capture(cat(paste0(str, "\n"), sep="\n")),
              equals(cat_prepare(paste0(str, "\n"), sep="\n")))
  expect_that(capture(cat(paste0(letters, "\n"), sep="\n")),
              equals(cat_prepare(paste0(letters, "\n"), sep="\n")))
})
