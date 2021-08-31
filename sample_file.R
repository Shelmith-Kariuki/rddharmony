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
          abr <- abr  %>%  dd_latest_source_year
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

