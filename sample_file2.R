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

  #  hamonize the vital5 data into standard age groups
  if (nrow(vitals5_raw[vitals5_raw$AgeSpan == 5,]) > 0) {

    print("harmonizing vital counts by 5-year age group")
    vitals5_std <- DDharmonize_Vitals5(indata = vitals5_raw, type = "births")

  } else { vitals5_std <- NULL }

## 8. isolate records that refer to single year age data
  vitals1_raw <- vitals_raw %>%
    dplyr::filter(AgeSpan %in% c(-2, -1, 1))

# harmonize the pop1 data into standard age groups
if (nrow(vitals1_raw[vitals1_raw$AgeSpan == 1,]) > 0) {
  print("harmonizing vital counts by 1-year age group")
  vitals1_std <- DDharmonize_Vitals1(indata = vitals1_raw)

} else { vitals1_std <- NULL }

  # continue standardizing if there are any age-specific records
  if (!is.null(vitals1_std) | !is.null(vitals5_std)) {

## 9. for births, check whether youngest age on abridged is lower than youngest age on complete, and if so
  ## distribute births at ages 10-14 across single ages 10-14 need to get Kirill's Matlab regression for this for now do arbitrary

  if (!is.null(vitals1_std) & !is.null(vitals5_std)) {
    if ("10-14" %in% vitals5_std$AgeLabel) {

      min_age1 <- min(vitals1_std$AgeStart[vitals1_std$AgeSpan == 1])
      min_age5 <- min(vitals5_std$AgeStart[vitals5_std$AgeSpan > 0])

      # distribute the ages
      if (min_age1 == 15 & min_age5 == 10) {
        b10_14 <- vitals5_std$DataValue[vitals5_std$AgeLabel == "10-14"]

        vitals1_std <- dd_births_abridged2single_1014(vitals1_std, births10_14 = b10_14)
      }
    }
  }

## 10. generate two datasets (vitals_abr, vitals_cpl) which are copies of (vitals5_std, vitals1_std) and set this to NULL
## if they are empty or don't exist

  if(!is.null(vitals5_std)) {
    vitals_abr <- vitals5_std
    if (nrow(vitals_abr) == 0) {
      vitals_abr <- NULL
    }
  } else {
    vitals_abr <- NULL
  }
  if(!is.null(vitals1_std)) {
    vitals_cpl <- vitals1_std
  } else {
    vitals_cpl <- NULL
  }


## 11. reconcile abridged and complete series, as necessary

    if (!is.null(vitals_abr) & !is.null(vitals_cpl)) {

    vitals_abr_cpl <- DDharmonize_AbridgedAndComplete(data_abr = vitals_abr,
                                                      data_cpl_from_abr = NULL,
                                                      data_cpl = vitals_cpl) %>%
      dplyr::filter(series %in% c("abridged reconciled with complete", "complete reconciled with abridged"))



  ## 12. fill in zeros for births at young ages, if missing //Start from here
  ## part a: abridged reconciled with complete
    vitals_abr_cpl1 <- vitals_abr_cpl %>%
      dplyr::filter(series == "abridged reconciled with complete")
    if (nrow(vitals_abr_cpl1) > 0) {
      vitals_abr_cpl1 <- dd_fillzeros_births(data = vitals_abr_cpl1 %>%
                                               select(-AgeSort), abridged = TRUE) %>%
        mutate(abridged = TRUE,
               complete = FALSE,
               series = "abridged reconciled with complete")
    }

    ## part b: complete reconciled with abridged
    vitals_abr_cpl2 <- vitals_abr_cpl %>%
      dplyr::filter(series == "complete reconciled with abridged")
    if (nrow(vitals_abr_cpl2) > 0) {
      vitals_abr_cpl2 <- dd_fillzeros_births(data = vitals_abr_cpl2 %>%
                                               select(-AgeSort), abridged = FALSE) %>%
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete reconciled with abridged")
    }
   ## append both vitals_abr_cpl1 and vitals_abr_cpl2
    vitals_abr_cpl <- rbind(vitals_abr_cpl1, vitals_abr_cpl2)
    rm(vitals_abr_cpl1, vitals_abr_cpl2)
    } else { vitals_abr_cpl <- NULL }


  ## 13. If we only have complete series and not abridged, ...
  if (is.null(vitals_abr) & !is.null(vitals_cpl)) {
    vitals5_std <- NULL
    vitals_abr_cpl <- NULL
    for (sex in unique(vitals_cpl$SexID)) {
      vitals_abr_cpl_sex <- dd_single2abridged(data = vitals_cpl %>% dplyr::filter(SexID == sex)) %>%
        mutate(SexID = sex)
      vitals_abr_cpl <- rbind(vitals_abr_cpl, vitals_abr_cpl_sex)
      rm(vitals_abr_cpl_sex)
    }
    vitals_abr_cpl <- dd_fillzeros_births(vitals_abr_cpl %>% select(-AgeSort), abridged = TRUE)

    vitals_abr_cpl <- vitals_abr_cpl %>%
      mutate(abridged = TRUE,
             complete = FALSE,
             series = "abridged reconciled with complete")
  }


  ## 14. For births, populate missing abridged age groups with zeros, as appropriate
  ## This is a case where only one of the series exists, right?
  if (!is.null(vitals5_std)) {
    vitals5_std <- dd_fillzeros_births(vitals5_std %>% select(-AgeSort), abridged = TRUE) %>%
      mutate(abridged = TRUE,
             complete = FALSE,
             series = "abridged")
  }

  if (!is.null(vitals_cpl)) {
    if (nrow(vitals_cpl[vitals_cpl$AgeSpan == 1,]) >=5 ) {
      vitals_cpl <- dd_fillzeros_births(vitals_cpl %>% select(-AgeSort), abridged = FALSE) %>%
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete")
    }
  }

  } else { # if there are no age-specific records
    vitals_cpl <- NULL
    vitals_abr_cpl <- NULL
  }


  ## 15.
