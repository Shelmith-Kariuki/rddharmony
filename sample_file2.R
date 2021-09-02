## This file describes the general workflow of the DDharmonize_validate_BirthCounts() function.
## We will use Sweden as an example
## 0. Load the packages required

require(DDSQLtools)
require(DemoTools)
require(tidyverse)
require(dplyr)
require(rddharmony)

## -------------------------------------------------------------------------------------------------------------------
## PART 1: EXTRACT VITAL COUNTS (Census and VR) FROM DEMO DATA AND HARMONIZE TO STANDARD
## ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES
## -------------------------------------------------------------------------------------------------------------------

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

  # list of series uniquely identified
  ids <- unique(dd_extract$id)

  vitals_std_all <- list()

  for (i in 1:length(ids)) {
    print(ids[i])

    ## 6. for each series:
    vitals_raw <- dd_extract %>%
      dplyr::filter(id == ids[i])

## 6. Testing the code with a use case where we have both complete and five-year age labels for the same:
  ## LocID: 752
  ## Loc: Sweden
  ## id: 752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair
  # vitals_raw <- dd_extract %>%
  #   dplyr::filter(id == "752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair")


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
    } ##end #11 i.e a case where we have both abridged and complete series
    else { vitals_abr_cpl <- NULL }


  ## 13. If we only have complete series and not abridged, ...
  if (is.null(vitals_abr) & !is.null(vitals_cpl)) {

    vitals5_std <- NULL
    vitals_abr_cpl <- NULL

    ## Generate abridged data from the complete series
    for (sex in unique(vitals_cpl$SexID)) {

      vitals_abr_cpl_sex <- dd_single2abridged(data = vitals_cpl %>% dplyr::filter(SexID == sex)) %>%
                                mutate(SexID = sex) # this returns a dataset containing 5-year age groups generated
      #from single years of age for each sex

      vitals_abr_cpl <- rbind(vitals_abr_cpl, vitals_abr_cpl_sex)

      rm(vitals_abr_cpl_sex)
    }
    ## Fill in the younger ages with 0s if the data does not exist

    vitals_abr_cpl <- dd_fillzeros_births(vitals_abr_cpl %>% select(-AgeSort), abridged = TRUE)

    vitals_abr_cpl <- vitals_abr_cpl %>%
      mutate(abridged = TRUE,
             complete = FALSE,
             series = "abridged reconciled with complete")
  }

    ## What if: (!is.null(vitals_abr) & is.null(vitals_cpl)), we leave as it is?

  ## 14. For births, populate missing abridged age groups with zeros, as appropriate
  ## This is a case where only one of the series exists, right?
  ## Should this be if (!is.null(vitals_abr) & is.null(vitals_cpl))? Ask Sara
  ## Wait ... is this redundant? Ask Sara
  if (!is.null(vitals5_std)) {
    vitals5_std <- dd_fillzeros_births(vitals5_std %>% select(-AgeSort), abridged = TRUE) %>%
      mutate(abridged = TRUE,
             complete = FALSE,
             series = "abridged")
  }

  ## Should this be if (!is.null(vitals_cpl) & is.null(vitals_abr))? Ask Sara
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


## 15.Assemble all of the series into a single dataset
  vitals_all <- vitals5_std %>% ## why not vitals_abr?
    bind_rows(vitals_cpl) %>%
    bind_rows(vitals_abr_cpl)

  if (nrow(vitals_all) > 0) {
    vitals_all <- vitals_all %>%
      mutate(id                     = ids[i],
             id_series = paste(id, series, sep = " - "),
             LocName                = vitals_raw$LocName[1],
             LocID                  = vitals_raw$LocID[1],
             LocTypeName            = vitals_raw$LocTypeName[1],
             LocAreaTypeName        = vitals_raw$LocAreaTypeName[1],
             SubGroupName           = vitals_raw$SubGroupName[1],
             SubGroupTypeName       = vitals_raw$SubGroupTypeName[1],
             DataCatalogID          = vitals_raw$DataCatalogID[1],
             DataCatalogName        = vitals_raw$DataCatalogName[1],
             DataProcess            = vitals_raw$DataProcess[1],
             DataProcessSort        = vitals_raw$DataProcessSort[1],
             DataProcessType        = vitals_raw$DataProcessType[1],
             DataProcessTypeSort    = vitals_raw$DataProcessTypeSort[1],
             ReferencePeriod        = vitals_raw$ReferencePeriod[1],
             TimeUnit               = vitals_raw$TimeUnit[1],
             TimeStart              = vitals_raw$TimeStart[1],
             TimeEnd                = vitals_raw$TimeEnd[1],
             TimeMid                = vitals_raw$TimeMid[1],
             TimeLabel              = vitals_raw$TimeLabel[1],
             DataSourceID           = vitals_raw$DataSourceID[1],
             DataSourceName         = vitals_raw$DataSourceName[1],
             DataSourceAuthor       = vitals_raw$DataSourceAuthor[1],
             DataSourceShortName    = vitals_raw$DataSourceShortName[1],
             DataSourceYear         = max(vitals_raw$DataSourceYear),
             DataSourceTypeName     = vitals_raw$DataSourceTypeName[1],
             DataSourceStatusName   = vitals_raw$DataSourceStatusName[1],
             DataStatusName         = vitals_raw$DataStatusName[1],
             DataStatusSort         = vitals_raw$DataStatusSort[1],
             StatisticalConceptName = vitals_raw$StatisticalConceptName[1],
             StatisticalConceptSort = vitals_raw$StatisticalConceptSort[1],
             DataTypeName           = vitals_raw$DataTypeName[1],
             DataSeriesID           = vitals_raw$SeriesID[1],
             DataReliabilityName    = vitals_raw$DataReliabilityName[1],
             DataReliabilitySort    = vitals_raw$DataReliabilitySort[1],
             ModelPatternName       = vitals_raw$ModelPatternName[1],
             PeriodTypeName         = vitals_raw$PeriodTypeName[1],
             PeriodGroupName        = vitals_raw$PeriodGroupName[1])
  }
  vitals_std_all[[i]] <- vitals_all

  rm(vitals_abr, vitals_cpl, vitals_abr_cpl, vitals5_std, vitals1_std)

  } # end of id loop
vitals_std_all <- do.call(rbind, vitals_std_all)

x <- vitals_std_all %>%
  dplyr::filter(id == "752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair")
unique(x$series)

## Potential tests at the end of part one:------------------------------------------------------------------------
## 1. At the end of part one, if the data has both abridged and complete series, it should have 4 unique series:
       ## 1) abridged , 2) complete , 3)  abridged reconciled with complete , 4) complete reconciled with abridged

## 2. An age label can either be abridged or complete, but not both
## 3. Abridged age labels can only belong to two unique series (abridged, `abridged reconciled with complete`).
##    Complete age labels can only belong to two unique series (complete, `complete reconciled with abridged`)
## 4. Each group of records should have ages 0, 1-4, 0-4, 5-9 as start ages with values == 0, i.e if they do not exist already
##

## -------------------------------------------------------------------------------------------------------------------
## PART 2: FILTER AVAILABLE SERIES, KEEPING ONLY THOSE THAT CONTAIN A FULL AGE DISTRIBUTION
# AND THE POST-RECONCILIATION ABRIDGED AND COMPLETE SERIES, WHERE APPLICABLE
## -------------------------------------------------------------------------------------------------------------------

if (nrow(vitals_std_all) > 0) {

  ## list the unique id series that exist in the data
  id_sers <- unique(vitals_std_all$id_series)

  ## create a placeholder for the object that will hold the final output (in this case all series that are full)
  id_series_full <- NULL

  ## for each id series
  for (i in 1:length(id_sers)) {

    ## subset the data to only have data for a particular id series
    ## Remember id series this time is a combination of the original id and the series after harmonization
    vitals_one_series <- vitals_std_all %>%
                          dplyr::filter(id_series == id_sers[i])

    ## check if the series is abridged or not
    abridged <- substr(vitals_one_series$series[1],1,1) == "a"

    ## check if the data(series) is full
    check_full <- dd_series_isfull(vitals_one_series %>%
                                     dplyr::filter(SexID == 3),
                                     abridged = abridged)

    ## if it is full, then we keep the series ... **
    if (check_full & nrow(vitals_one_series) >= 8) {
      id_series_full <- c(id_series_full, id_sers[i])
    }
  }

## subset the data to only be left with data where the series is full
## we are losing so much data **
  vitals_std_full <- vitals_std_all %>%
    dplyr::filter(id_series %in% id_series_full) %>%
    mutate(id_sex = paste(id, SexID, sep = " - "))

} else { vitals_std_full <- vitals_std_all }


## for each id-sex combo of full series, keep the reconciled series if it is available and discard the original abridged or complete

if (nrow(vitals_std_full) > 0) {

  ## identify unique id and sex combination
  ids_sex <- unique(vitals_std_full$id_sex)

  ## create a placeholder for the object that will hold the final output (in this case )
  vitals_privilege_recon <- NULL

  ## for each id and sex combination
  for (i in 1:length(ids_sex)) {

    ## subset the data to get data where the series is abridged or `abridged reconciled with complete`
    abr <- vitals_std_full %>%
             dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "a")


    ## if there are some abridged records, and `abridged reconciled with complete` exists, then this is the series we will keep
    ## and discard the original one.
    if (nrow(abr) > 0) {
      if ("abridged reconciled with complete" %in% abr$series) {
        abr <- abr %>%
          dplyr::filter(series == "abridged reconciled with complete")
      }
    }

    ## if there are some complete records, and `complete reconciled with abridged` exists, then this is the series we will keep
    ## and discard the original one.
    cpl <- vitals_std_full %>%
      dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "c")
    if (nrow(cpl) > 0) {
      if ("complete reconciled with abridged" %in% cpl$series) {
        cpl <- cpl %>%
          dplyr::filter(series == "complete reconciled with abridged")
      }
    }

    ## the reconciled series will be our final series. append the two
    vitals_privilege_recon <- vitals_privilege_recon %>%
                                bind_rows(abr) %>%
                                bind_rows(cpl)

  }

  ## drop id_sex and id_series variables
  vitals_std_full <- vitals_privilege_recon %>%
    select(-id_sex, -id_series)

} else { vitals_std_full <- vitals_std_full }


