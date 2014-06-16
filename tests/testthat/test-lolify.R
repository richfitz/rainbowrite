source("helper-rainbowrite.R")
context("lolify")

test_that("lolify", {
  say <- lolify(cowsay::say)
  expect_that(rainbowrite:::is_lolified(say), is_true())
  expect_that(lolify(say), gives_warning())
  say <- unlolify(say)
  expect_that(rainbowrite:::is_lolified(say), is_false())
  expect_that(unlolify(say), gives_warning())
})

test_that("lolify_in_environment", {
  ## First, load the package entirely.
  library(cowsay)
  lolify_in_environment('say', 'package:cowsay')
  expect_that(rainbowrite:::is_lolified(say), is_true())
  expect_that(rainbowrite:::is_lolified(cowsay::say), is_false())
  unlolify_in_environment('say', 'package:cowsay')
  expect_that(rainbowrite:::is_lolified(say), is_false())
  expect_that(rainbowrite:::is_lolified(cowsay::say), is_false())
})
