context("Functionality")

library(dplyr)
library(maps)

setwd(system.file("extdata", package = "fars"))

test_that("fars_read() functions properly", {
  expect_is(fars_read("accident_2013.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2005.csv.bz2"))
})

test_that("fars_summarize_years() functions properly", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_is(fars_summarize_years(list(2014, 2015)), "tbl_df")
  expect_equal(names(fars_summarize_years(2013:2015)), c("MONTH", 2013:2015))
  expect_error(fars_summarize_years(2025))
})

test_that("fars_map_state() functions properly", {
  expect_silent(fars_map_state(9, 2015))
  expect_error(fars_map_state(61, 2014))
  expect_error(fars_map_state(5, 1789))
})
