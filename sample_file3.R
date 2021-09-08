## This file describes the general workflow of the DDharmonize_validate_BirthCounts() function.
## We will use Sweden as an example
## 0. Load the packages required

require(DDSQLtools)
require(DemoTools)
require(tidyverse)
require(dplyr)
require(rddharmony)
require(googlesheets4)
require(devtools)
require(usethis)

## Call with Tim
## State in the vignette explicitly that only 3 functions are of most importance
## include examples for only these main functions
## is there a way we can ensure that the main functions appear at the top? Think of editing the function names

DDharmonize_validate_BirthCounts <- function(locid,
                                             times,
                                             process = c("census", "vr"),
                                             return_unique_ref_period = TRUE, # if true, then only most authoratative series will be returned for each reference period, per dd_rank_id_vitals()
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/") {
## -------------------------------------------------------------------------------------------------------------------
## PART 1: EXTRACT VITAL COUNTS (Census and VR) FROM DEMO DATA AND HARMONIZE TO STANDARD
## ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES
## -------------------------------------------------------------------------------------------------------------------


## 1. Extract all vital counts for a given country over the period specified in times
locid <- 404
times <- c(1950,2050)
process = c("census", "vr")
return_unique_ref_period <- TRUE # if true, then only most authoratative series will be returned for each reference period, per dd_rank_id_vitals()
DataSourceShortName = NULL
DataSourceYear = NULL
retainKeys = FALSE
server = "https://popdiv.dfs.un.org/DemoData/api/"



dd_extract <- DDextract_VitalCounts(locid = locid,
                                    type = "births",
                                    process = process,
                                    start_year = times[1],
                                    end_year = times[length(times)],
                                    DataSourceShortName = DataSourceShortName,
                                    DataSourceYear = DataSourceYear)
if (!is.null(dd_extract)) {

   # get data process id
  dpi <- ifelse(process == "census", 2, 36)

  ## 2. Drop sub-national censuses (data process "vr" does not work with get_datacatalog?)
  if (dpi == 2) {

    # Gets all DataCatalog records
    DataCatalog <- get_datacatalog(locIds = locid, dataProcessTypeIds = 2, addDefault = "false")
    DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]
  }

  if(nrow(DataCatalog) > 0) {
    # Drop sub national censuses (isSubnational== TRUE i.e if dd_extract$DataCatalogID `notin` DataCatalog$DataCatalogID)
    dd_extract <- dd_extract %>%
      dplyr::filter(DataProcessID == 36 |(DataProcessID == 2 & DataCatalogID %in% DataCatalog$DataCatalogID))
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

## 5. for births by age of mother, use only both sexes combined ** why?
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
    # vitals_raw <- dd_extract %>%
    #      dplyr::filter(id == "404 - Kenya - VR - Births - 2011 - Register - Demographic Yearbook - Year of occurrence - Direct - Low")

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



  ## 12. fill in zeros for births at young ages, if missing
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

# x <- vitals_std_all %>%
#   dplyr::filter(id == "752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair")
# x <- vitals_std_all %>%
#   dplyr::filter(id == "752 - Sweden - VR - Births - 2015 - Register - Eurostat Database - Year of occurrence - Direct - High quality")
#
# unique(x$series)

## Potential tests at the end of part one:------------------------------------------------------------------------
## 1. At the end of part one, if the data has both abridged and complete series, it should have 4 unique series:
       ## 1) abridged , 2) complete , 3)  abridged reconciled with complete , 4) complete reconciled with abridged

## 2. An age label can either be abridged or complete, but not both
## 3. Abridged age labels can only belong to two unique series (abridged, `abridged reconciled with complete`).
##    Complete age labels can only belong to two unique series (complete, `complete reconciled with abridged`)
## 4. Each group of records should have ages 0, 1-4, 0-4, 5-9 as start ages with values == 0, i.e if they do not exist already
## 5. Check the id_series (after removing the series) that are not in the original ids are either records
## of indicator 159 (Total) or records without age labels. This will ensure that we are not dropping
## any data.

incl_ids <- vitals_std_all %>% distinct(id) %>% pull()
excl_ids <- ids[!ids %in% incl_ids]

print(paste0(length(excl_ids), " out of ", length(ids),
             " ids have been dropped from the data at this point. We need to investigate the data further to ensure that we are not losing any data"))
excl_ids

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
  vitals_std_full <- vitals_std_all %>%
    dplyr::filter(id_series %in% id_series_full) %>%
    mutate(id_sex = paste(id, SexID, sep = " - "))

} else { vitals_std_full <- vitals_std_all }

## At this point we are losing a lot of data, we need to check this
print_dropped_ids( indata_before = vitals_std_all, indata_after = vitals_std_full)

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


################################## -------------------------------------------------------------------------------------------------------------------
## PART 3: VALIDATE THE REMAINING SERIES, CHECKING FOR BOTH SEX TOTALS THAT MATCH BY SEX,
# CORRECT FOR ANY INSTANCES WHERE DATA FOR SEX-AGE GROUP COMBINATIONS ARE MISSING, AND
# CHECK WHETHER SUM OVER AGE MATCHES THE REPORTED TOTALS
################################## -------------------------------------------------------------------------------------------------------------------

## validate totals over age (if the reported and actual totals exist, if computed is greater than reported,
## then replace reported with computed, if computed is less than reported, then add difference to "Unknown" age)
## and distribute unknowns
if (nrow(vitals_std_full) > 0) {

  ## identify the unique ids in the data
  ids <- unique(vitals_std_full$id)

  ## create a place holder for the final output which is ...
  vitals_std_valid <- list()

  for (i in 1:length(ids)) {

    ## subset the data to only be left with data for one id
    dd_one_id <- vitals_std_full %>%
      dplyr::filter(id == ids[i])

    ## reconcile reported and computed totals over age
    dd_one_id <- dd_validate_totals_over_age(data = dd_one_id)

    ## distribute unknowns by age
    dd_one_id <- dd_distribute_unknowns(data = dd_one_id)

    vitals_std_valid[[i]] <- dd_one_id

  }

  vitals_std_valid <- do.call(rbind, vitals_std_valid) %>%
    mutate(five_year = abridged == TRUE & AgeSpan %in% c(-1,5),
           abridged = abridged == TRUE & AgeLabel != "0-4")

} else { vitals_std_valid <- vitals_std_full }

## At this point, the difference between vitals_std_full and vitals_std_valid should be qual to
## length(vitals_std_full[vitals_std_full$AgeLabel == "Unknown","AgeLabel"])

assertthat::are_equal(abs(nrow(vitals_std_full) - nrow(vitals_std_valid)),
                      length(vitals_std_full[vitals_std_full$AgeLabel == "Unknown","AgeLabel"]))

## When there is more than one id for a given census year, select the most authoritative

if (nrow(vitals_std_valid) > 0) {

  if (return_unique_ref_period == TRUE) {

    vitals_valid_id <- vitals_std_valid %>% dd_rank_id_vitals
    ##  Cannot find `discard_these_dups` used in the dd_rank_id_vitals function

  } else { vitals_valid_id <- vitals_std_valid }

## arrange the data, with priority columns on the left and data loader keys on the right
  first_columns <- c("id", "LocID", "LocName", "DataProcess", "ReferencePeriod", "TimeStart", "TimeMid", "SexID",
                     "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue", "note", "abridged", "five_year",
                     "complete", "non_standard")
  keep_columns <- names(vitals_std_all)
  keep_columns <- keep_columns[!(keep_columns %in% c("series", "id_series", "DataSeriesID", first_columns))]

  out_all <- vitals_valid_id %>%
    mutate(non_standard = FALSE,
           DataTypeName = "Direct (age standardized)",
           note = NA) %>%
    select(all_of(first_columns), all_of(keep_columns))

} else { out_all <- NULL }

## Look for years that are in raw data, but not in output. If there are series with non-standard age groups, then add these to output as well

first_columns <- first_columns[!(first_columns %in% c("five_year", "abridged", "complete", "non_standard", "note"))]
skipped <- dd_extract %>%
  dplyr::filter(!(TimeLabel %in% out_all$TimeLabel)) %>%
  select(all_of(first_columns), all_of(keep_columns)) %>%
  mutate(five_year = FALSE,
         abridged = FALSE,
         complete = FALSE,
         non_standard = TRUE,
         note = "Not harmonized or validated due to non-standard age groups") %>%
  arrange(id, SexID, AgeSort) %>%
  distinct()

out_all <- rbind(out_all, skipped) %>%
  arrange(id, SexID, abridged, AgeSort) %>%
  mutate(IndicatorName = NA,
         IndicatorName = replace(IndicatorName, abridged == TRUE, "Births by age and sex - abridged"),
         IndicatorName = replace(IndicatorName, five_year == TRUE, "Births by age and sex - abridged"),
         IndicatorName = replace(IndicatorName, complete == TRUE, "Births by age and sex - complete"),
         AgeUnit = "Year",
         SexName = NA,
         SexName = replace(SexName, SexID == 0, "Unknown"),
         SexName = replace(SexName, SexID == 1, "Male"),
         SexName = replace(SexName, SexID == 2, "Female"),
         SexName = replace(SexName, SexID == 3, "Both sexes"))

if (retainKeys == FALSE) {
  out_all <- out_all %>%
    select(id, LocID, LocName, TimeLabel, TimeMid, TimeEnd, DataProcessType, DataSourceName, StatisticalConceptName,
           DataTypeName, DataReliabilityName, five_year, abridged, complete, non_standard, SexID, AgeStart, AgeEnd,
           AgeLabel, AgeSpan, AgeSort, DataValue, note)
}

} else { # if no birth counts were extracted from DemoData
  print(paste0("There are no birth counts by age available for LocID = ",locid," and dataprocess = ", process," for the time period ", times[1], " to ", times[length(times)]))
  out_all <- NULL
}
}


my_data <- DDharmonize_validate_BirthCounts(locid,
                                             times,
                                             process = c("census", "vr"),
                                             return_unique_ref_period = TRUE, # if true, then only most authoratative series will be returned for each reference period, per dd_rank_id_vitals()
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/")
