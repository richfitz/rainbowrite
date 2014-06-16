source("helper-rainbowrite.R")
context("lolcat")

test_that("Offset is updated appropriately", {
  os <- rainbowrite:::opts$os
  ignore <- rainbowrite:::capture(lolcat("hello"))
  expect_that(rainbowrite:::opts$os, equals(os))
  ignore <- rainbowrite:::capture(lolcat("hello\n"))
  expect_that(rainbowrite:::opts$os, equals(os+1))
})
