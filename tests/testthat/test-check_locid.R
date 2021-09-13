context("Testing that check_locid() works as expected")


## Create a list of plausible location ids
possible_ids <- DDSQLtools::get_locations() %>% distinct(PK_LocID, Name)

## the function directs users to View(get_locations()) when a wrong location id is used.

test_that("the function directs users to View(get_locations()) when a wrong id is inserted", {

  ## Generate the test
  locid <- 9000000
  if (!locid %in% possible_ids$PK_LocID) {
    text <- check_locid(locid)
    expect_true(grepl("get_locations", text))

  }
})


## the function returns the correct location name when a correct location id is used

test_that("returns the correct location name when a correct location id is used", {

  locid <- 404
  if (locid %in% possible_ids$PK_LocID) {
    text <- check_locid(locid)
    locname <- trimws(gsub(".*The location name is ", "", text))
    expect_equal(locname, possible_ids$Name[possible_ids$PK_LocID == locid])

  }
})
