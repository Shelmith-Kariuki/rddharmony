
## For records where DataProcessType == "Census", test to see that subnational ids do not exists

## Create a list of plausible location ids
possible_ids <- DDSQLtools::get_locations() %>% distinct(PK_LocID, Name)
times <- c(1950,2050)
process = c("census", "vr")
return_unique_ref_period <- TRUE
DataSourceYear = NULL
DataSourceShortName <- NULL
DataSourceYear = NULL

test_that("dpi == 2 does not contain any subnational ids", {

  require(dplyr)

  locid <- 404
  if (locid %in% possible_ids$PK_LocID) {

  # Gets all DataCatalog records
  DataCatalog <- DDSQLtools::get_datacatalog(locIds = locid, dataProcessTypeIds = 2, addDefault = "false")
  DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]

  ## Extract the data
    dd_extract <- DDextract_VitalCounts(locid = locid,
                                        type = "births",
                                        process = process,
                                        start_year = times[1],
                                        end_year = times[length(times)],
                                        DataSourceShortName = DataSourceShortName,
                                        DataSourceYear = DataSourceYear)

  ## Subset the data to ensure we don't have subnational ids
   dd_extract <- dd_extract %>%
      dplyr::filter(DataProcessID == 36 |(DataProcessID == 2 & DataCatalogID %in% DataCatalog$DataCatalogID))

   ## Create a vector of DataCatalogIDs
    dcs <- dd_extract %>%
            filter(DataProcessID == 2) %>%
            distinct(DataCatalogID) %>%
            pull()

    ## Check that all the DataCatalogIDs in the data are in the DataCatalog dataset
    expect_true(all(dcs %in% DataCatalog$DataCatalogID))
  }

})
