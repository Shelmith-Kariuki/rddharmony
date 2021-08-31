## This file describes the general workflow of the DDharmonize_validate_BirthCounts() function.
## We will use Sweden as an example
## 0. Load the packages required

require(DDSQLtools)
require(DemoTools)
require(tidyverse)
require(dplyr)

## 1. Extract all vital counts for a given country over the period specified in times
locid = 752
start_year = 1950
end_year =2017
process = c("census","vr")

dd_extract <- rddharmony::DDextract_VitalCounts(locid,
                                                type = c("births"),
                                                process = c("census","vr"),
                                                start_year = start_year,
                                                end_year = end_year,
                                                DataSourceShortName = NULL,
                                                DataSourceYear = NULL)

 # get data process id
  dpi <- ifelse(process == "census", 2, 36)

  ## 2. Filter out sub-national censuses (data process "vr" does not work with get_datacatalog?)
  if (dpi == 2) {

    DataCatalog <- get_datacatalog(locIds = locid, dataProcessTypeIds = 2, addDefault = "false")
    DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]
  }

  if(nrow(DataCatalog) > 0) {
    # Keep only those population series for which isSubnational is FALSE
    dd_extract <- dd_extract %>%
      dplyr::filter(dpi == 36 |(dpi == 2 & DataCatalogID %in% DataCatalog$DataCatalogID))
  }

## 3. Get additional DataSource keys (temporary fix until Dennis adds to DDSQLtools extract)

  if (!("DataSourceTypeName" %in% names(dd_extract))) {

    DataSources <- get_datasources(locIds = locid, dataProcessTypeIds = dpi, addDefault = "false") %>%
      dplyr::select(LocID, PK_DataSourceID, DataSourceTypeName, DataSourceStatusName) %>%
      dplyr::rename(DataSourceID = PK_DataSourceID)

    dd_extract <- dd_extract %>%
      left_join(DataSources, by = c("LocID", "DataSourceID") )
  }

## 4. Discard DataTypeName==“Direct (standard abridged age groups computed)” or
  # “Direct (standard abridged age groups computed - Unknown redistributed)”
  dd_extract <- dd_extract %>%
    dplyr::filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
                  DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)',
                  DataTypeName!= 'Reverse survival method',
                  DataTypeName!= 'Birth histories') %>%
    mutate(id = paste(LocID, LocName, DataProcess, "Births", TimeLabel, DataProcessType, DataSourceName, StatisticalConceptName, DataTypeName, DataReliabilityName, sep = " - ")) %>%
    arrange(id)

## 5. for births by age of mother, use only both sexes combined
  dd_extract <- dd_extract %>% dplyr::filter(SexID ==3)

## 6. Testing the code with a use case where we have both complete and five-year age labels for the same:
  ## LocID: 752
  ## Loc: Sweden
  ## id: 752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair
  vitals_raw <- dd_extract %>%
    dplyr::filter(id == "752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair")

## 7. Isolate records that refer to five-year age data
  # -1 (Total), -2 (Unknown): These age labels will feature in both 5-year and 1-year data.
  vitals5_raw <- vitals_raw %>%
    dplyr::filter(AgeSpan %in% c(-2, -1) | AgeSpan >=5)

    print("harmonizing vital counts by 5-year age group")
    indata <- vitals5_raw
    abr_sex <- NULL
    sexes <- unique(indata$SexID)
    sex <- sexes

      ##7.0. Print the SexID whose data we are dealing with
      print(paste("SexID = ", sex))

      ##7.1. Filter the data to only be left with data for this specific SexID
      abr <- indata %>%
        dplyr::filter(SexID == sex & !is.na(DataValue)) %>%
        select(-SexID) %>%
        distinct()

      ##7.2. if "Final" data status is available, keep only the final series
      if ("Final" %in% unique(abr$DataStatusName)) {
        abr <- abr %>%
          dplyr::filter(DataStatusName == "Final")
      }

      ##7.3. check for multiple series ids
      ids_series <- unique(abr$SeriesID)
      n_series <- length(ids_series)

      # for each unique series,
      abr_out <- NULL
      for (i in 1:n_series) {

      ##7.4. Filter the data for a specific SeriesID
        df <- abr %>% dplyr::filter(SeriesID == ids_series[i])

      ##7.5. Check whether it is a full series with all age groups represented and an open age greater than 60
        ## AgeSpan == -1 (Total), AgeSpan == -2 (Unknown), AgeSpan == 5(five-year)

        df_abr_std    <- df[(df$AgeStart == 0 & df$AgeSpan == 1 ) |
                              df$AgeSpan %in% c(-1, -2, 5),]

        if (nrow(df_abr_std) > 13) {

          df$check_full <- rddharmony::dd_series_isfull(df_abr_std, abridged = TRUE)

        } else { df$check_full <- FALSE }
        abr_out <- rbind(abr_out, df)
      }
      abr <- abr_out
      rm(abr_out)

    ##7.6. Keep the latest datasource year

      # if there is more than one series ...
      if (n_series > 1) {
        latest_source_year <- max(abr$DataSourceYear)
        check_latest_full  <- unique(abr$check_full[abr$DataSourceYear == latest_source_year])
        # ... and latest series is full then keep only that one
        if (check_latest_full) {
          abr <- abr[abr$DataSourceYear == latest_source_year,]
        } else {
          # ... and latest series is not full, then keep the latest data source record for each age
          abr <- abr  %>%  rddharmony::dd_latest_source_year()
        }
      }

    ##7.7 Tidy up the data frame
      abr <- abr %>%
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>%
        distinct()

    ##7.8. If there is no record for unknown age, set data value to zero
      if (!("Unknown" %in% abr$AgeLabel)) {
        abr <- abr %>%
          bind_rows(data.frame(AgeStart = -2,
                               AgeEnd = -2,
                               AgeSpan = -2,
                               AgeLabel = "Unknown",
                               DataSourceYear = NA,
                               DataValue = 0))
      }

    ##7.9. For births, reconcile wide early age groups to abridged (e.g., 0-19, 0-14, 10-19 etc)
      # if (type == "births") {

        abr <- abr %>% rddharmony::dd_firstages_compute_births() %>%
          select(-AgeSort)

      # }

      # reconcile first age groups 0-1, 0-4, 1-4
      abr <- abr %>% rddharmony::dd_firstages_compute()

    ##7.10.Check whether there are multiple open age groups (TRUE/FALSE Output)
      oag_multi <- abr %>% rddharmony::dd_oag_multiple()

    ##7.11. Compute closed age groups from multiple open age groups and add to data if missing**
      if (oag_multi) {
        add <- abr %>%
          rddharmony::dd_oag2closed() %>%
          dplyr::filter(!(AgeLabel %in% abr$AgeLabel[!is.na(abr$DataValue)]))

        if (nrow(add > 0)) {
          abr <- abr %>%
            bind_rows(add) %>%
            arrange(AgeStart)
        }
      }

    ## 7.12. Identify the start age of the open age group needed to close the series
      oag_start <- abr %>% rddharmony::dd_oag_agestart()

      # flag whether this open age group exists in the series
      oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel

    ## 7.13. Drop records for open age groups that do not close the series
      if (!is_empty(oag_start)){
        abr <- abr %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start))
      }

    ## 7.14. Check that there are no missing age groups on the abridged series
      if (nrow(abr[abr$AgeStart >= 5,]) > 0) {
        check_abr <- is_abridged(abr$AgeStart[abr$AgeStart >=5])
      } else {
        check_abr <- FALSE
      }


    ## 7.15. Compute all possible open age groups given available input ** //Start from here
   if (check_abr==TRUE) {
        abr_oag <- dd_oag_compute(abr, age_span = 5)

    ## and append the oag that completes the abridged series
        abr <- abr %>%
          bind_rows(abr_oag[!(abr_oag$AgeLabel %in% abr$AgeLabel) &
                              abr_oag$AgeStart == oag_start,]) %>%
          arrange(AgeSort)

      }

    ## 7.16. Check again whether any open age group exists
      oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel

    ## 7.17. If total is missing and series is otherwise complete, compute total
      if (!("Total" %in% abr$AgeLabel) & "0-4" %in% abr$AgeLabel & oag_check == TRUE) {
        abr <- abr %>%
          bind_rows(data.frame(AgeStart = 0,
                               AgeEnd = -1,
                               AgeLabel = "Total",
                               AgeSpan = -1,
                               AgeSort = 184,
                               DataSourceYear = NA,
                               DataValue = sum(abr$DataValue[abr$AgeSpan == 5]) +
                                 abr$DataValue[abr$AgeSpan == -1 & abr$AgeStart == oag_start] +
                                 abr$DataValue[abr$AgeLabel == "Unknown"]))
      }

    ## 7.18. Write a note to alert about missing data
      abr$note <- NA
      if (check_abr == FALSE | oag_check == FALSE) {
        abr$note <- "The abridged series is missing data for one or more age groups."
      }
      if (!("0" %in% abr$AgeLabel & "1-4" %in% abr$AgeLabel & "0-4" %in% abr$AgeLabel)) {
        abr$note <- "The abridged series is missing data for one or more age groups."
      }
      abr$SexID <- sex

    ##7.22. Add series field to data
      if (!is.null(abr_sex)) {
        abr_sex <- abr_sex %>%
          mutate(abridged = TRUE,
                 complete = FALSE,
                 series = "abridged") %>%
          dplyr::filter(AgeSpan %in% c(-2, -1, 1, 4, 5))
      }
