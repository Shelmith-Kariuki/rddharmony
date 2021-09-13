context("Testing that DDextract_VitalCounts() returns the correct class and structure of output")

possible_ids <- DDSQLtools::get_locations() %>% distinct(PK_LocID, Name)
times <- c(1950,2050)
process = c("census", "vr")
return_unique_ref_period <- TRUE # if true, then only most authoritative series will be returned for each reference period, per dd_rank_id_vitals()
DataSourceShortName = NULL
DataSourceYear = NULL

## location id results into an output of class NULL

test_that("an invalid location id results into an output of class NULL", {
  locid <- 9000000
  if (!locid %in% possible_ids$PK_LocID) {
  dd_extract <- DDextract_VitalCounts(locid = locid,
                                      type = "births",
                                      process = process,
                                      start_year = times[1],
                                      end_year = times[length(times)],
                                      DataSourceShortName = DataSourceShortName,
                                      DataSourceYear = DataSourceYear)

   expect_type(dd_extract,"NULL")
  }
})

## a valid location id results into a dataframe

test_that("a valid location id results into a dataframe", {

  locid <- 404
  if (locid %in% possible_ids$PK_LocID) {
    dd_extract <- DDextract_VitalCounts(locid = locid,
                                      type = "births",
                                      process = process,
                                      start_year = times[1],
                                      end_year = times[length(times)],
                                      DataSourceShortName = DataSourceShortName,
                                      DataSourceYear = DataSourceYear)

  expect_true("data.frame" %in% class(dd_extract))
  }
  })

## if a valid location id is used, the resulting dataframe has 93 columns

test_that("if a valid location id is used, the resulting dataframe has 93 columns", {
  locid <- 404
  if (locid %in% possible_ids$PK_LocID) {
  dd_extract <- DDextract_VitalCounts(locid = locid,
                                      type = "births",
                                      process = process,
                                      start_year = times[1],
                                      end_year = times[length(times)],
                                      DataSourceShortName = DataSourceShortName,
                                      DataSourceYear = DataSourceYear)

  expect_equal(ncol(dd_extract), 93)
  }
})
