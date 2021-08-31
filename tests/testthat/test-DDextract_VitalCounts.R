require(DemoTools)
require(DDSQLtools)
require(tidyverse)
require(dplyr)

lids <- get_locations() %>% distinct(PK_LocID) %>% pull()

test_that("an invalid location id results into an error", {
  expect_error()
})
