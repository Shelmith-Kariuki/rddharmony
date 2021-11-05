require(rddharmony)
require(DemoTools)
require(DDSQLtools)
require(dplyr)
require(testthat)

#locid <-  312
locid <- sample(get_locations()$PK_LocID, 1)
clean_df <- DDharmonize_validate_PopCounts(locid = c(404),
                                             times = c(2020),
                                             process = c("census", "estimate", "register"),
                                             return_unique_ref_period = TRUE,
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/")

if(!is.null(clean_df) >0 ){
  ## Filter non-harmonized ids
  non_harmonized_ids <- clean_df %>%
    filter(!is.na(note)) %>%
    pull(id) %>%
    unique()

  if(length(non_harmonized_ids) >0 ){
    clean_df <- clean_df %>% filter(!id %in% non_harmonized_ids)
  }


options(dplyr.summarise.inform=F)


if(nrow(clean_df) >0 ){

## These tests will only work in situations where indicator 170 data exists

test_that("The data has a unique locid", {
  expect_length(unique(clean_df$LocID), 1)
})

test_that("The data has a unique locname", {
  expect_length(unique(clean_df$LocName), 1)
})

test_that("Each id has 3 sexes", {
  tab <- clean_df %>%
          distinct(id, SexID) %>%
          group_by(id) %>%
          summarise(counter = length(unique(SexID)))

  expect_true(unique(tab$counter) == 3)
})

test_that("An age label cannot be complete and abridged at the same time", {
  tab <- clean_df %>%
    mutate(checker = ifelse(abridged != complete | AgeLabel == "0-4" , TRUE, FALSE))
  expect_true(all(tab$checker == TRUE))
})


test_that("Abridged labels should either contain a 0, a range e.g 5-9 , an open age group (with a +) and a Total", {
  abridged_labs <- clean_df %>%
    filter(abridged == TRUE) %>%
    mutate(checker = ifelse(AgeLabel %in% grep("-|\\+|0|Total", AgeLabel, value = TRUE,
                                               ignore.case = TRUE), TRUE, FALSE))
  expect_true(all(abridged_labs$checker == TRUE))
})

test_that("Complete cases do not contain wide age groups", {
  complete_labs <- clean_df %>%
    filter(complete == TRUE) %>%
    mutate(checker = ifelse(!AgeLabel %in% grep("-", AgeLabel, value = TRUE,
                                                ignore.case = TRUE), TRUE, FALSE))
  expect_true(all(complete_labs$checker == TRUE))
})

# test_that("Every id has a closing age group",{
#   tab <- clean_df %>%
#     # filter(is.na(note)) %>%
#     group_by(id, SexID, complete) %>%
#     mutate(oag_present = ifelse(any(AgeLabel %in% grep("\\+", AgeLabel, value = TRUE,ignore.case = TRUE)),
#                                 TRUE, FALSE))
#   expect_true(all(tab$oag_present == TRUE))
# })

test_that("Every id has a closing age group, and if not, computed total is equal to the reported total",{
  tab <- clean_df %>%
    group_by(id, SexID, complete) %>%
    mutate(oag_present = ifelse(any(AgeLabel %in% grep("\\+", AgeLabel, value = TRUE,ignore.case = TRUE)),
                                TRUE, FALSE)) %>%
    ungroup()

  if(!all(tab$oag_present == TRUE)){

    tab2 <- tab %>%
      group_by(id, SexID, complete) %>%
      mutate(rec_tot = ifelse(any(AgeLabel == "Total"), DataValue[AgeLabel == "Total"], NA),
             calc_tot = ifelse(all(c("0-4", "1-4") %in% AgeLabel),
                                      sum(DataValue[AgeLabel != "Total" & AgeLabel!= "0-4"], na.rm = TRUE),
                                      sum(DataValue[AgeLabel != "Total"], na.rm = TRUE)) ,
             diff = floor(abs(rec_tot - calc_tot))) %>%
      filter(!is.na(rec_tot) & AgeSort!=999)

  }

  expect_true(ifelse(!all(tab$oag_present == TRUE),all(tab2$diff == 0), TRUE))

})

# test_that("All Data values add up to the Total, for each id", {
#
#   # This test fails for the id below, because the data reported on abridged is different from that reported on complete.
#   # Abridged: 0-4, 1763542
#   # Complete: 0 (428912), sum(1-4) == 1313866 so 0-4 should be 1742778, different from the value on abridged.
#   #"410 - Republic of Korea - Census - 1955 - Demographic Yearbook - De-facto - Population by age and sex - Fair"
#   # Same case with "312 - Guadeloupe - Estimate - 2003 - Demographic Yearbook - De-jure - Population by age and sex - Fair"
#   # "156 - China - Estimate - 1985 - Global Burden of Disease Study 2017 (GBD2017): Population Estimates 1950-2017 - Unknown - Model-based estimates - Fair" also has large differences
#   tab <- clean_df %>%
#     group_by(id, SexID, complete) %>%
#     mutate(DataValue = floor(DataValue)) %>%
#     mutate(rec_tot = ifelse(any(AgeLabel == "Total"), DataValue[AgeLabel == "Total"], NA),
#            calc_tot = ifelse(all(c("0-4", "1-4") %in% AgeLabel),
#                                     sum(DataValue[AgeLabel != "Total" & AgeLabel!= "0-4"], na.rm = TRUE),
#                                     sum(DataValue[AgeLabel != "Total"], na.rm = TRUE)) ,
#            diff = floor(abs(rec_tot - calc_tot))) %>%
#     filter(!is.na(rec_tot))
#
#   expect_true(all(tab$diff < 10))
#
# })

test_that("There isn't any unknown in the data, and if it exists, then the total value is not reported", {
  unknown_df <- clean_df %>%
    group_by(id, SexID, complete) %>%
    mutate(checker = ifelse(any(AgeLabel == "Total") & AgeLabel == "Unknown", "flag", "okay")) %>%
    filter(checker == "flag" )

  expect_true(nrow(unknown_df)==0)

})

test_that("There aren't any negative data values", {
  negs <- clean_df %>%
    filter(DataValue < 0)

  expect_true(nrow(negs)==0)

})

test_that("The complete age labels begin with a 0", {

  tab <- clean_df %>%
    filter(complete == TRUE)

  if(nrow(tab) > 0){


    tab2 <- tab %>%
      group_by(id, SexID) %>%
      mutate(min_agesort = min(AgeSort, na.rm = TRUE)) %>%
      mutate(min_agelabel = ifelse(AgeSort == min_agesort, AgeLabel, NA)) %>%
      mutate(min_agelabel = ifelse(is.na(min_agelabel), min_agelabel[!is.na(min_agelabel)], min_agelabel))
  }

  expect_equal(ifelse(nrow(tab)>0, unique(tab2$min_agelabel), "0"), "0")
})

test_that("Abridged series start with the following values: 0, 0-4, 1-4, 5-9, 10-14", {


  abr_agelabs <- clean_df %>%
    filter(complete == FALSE)

  abr_agelabs2 <- abr_agelabs%>%
    pull(AgeLabel) %>%
    unique()

  expected_labs <- c("0", "0-4", "1-4", "5-9", "10-14")

  expect_equal(ifelse(nrow(abr_agelabs)>0, all(expected_labs %in% abr_agelabs2), TRUE), TRUE)

})

test_that("Each age label is unique per id", {
  tab <- clean_df %>%
    # filter(is.na(note)) %>%
    group_by(id, complete, SexID, AgeLabel) %>%
    summarise(counter = n())

  expect_true(unique(tab$counter)==1)
})


test_that("There is only one unique id per time label",{
  tab <- clean_df %>%
    # filter(is.na(note)) %>%
    group_by(TimeLabel) %>%
    summarise(counter = length(unique(id)))

  expect_true(unique(tab$counter)==1)

})

test_that("If the data has both complete and abridged series, the totals should be equal or differ by less than 5 maybe",{

  tab <- clean_df %>%
    select(id, SexID, complete, AgeLabel, DataValue) %>%
    # select(id,IndicatorID, SexID, complete, AgeLabel, DataValue) %>%
    filter(AgeLabel == "Total") %>%
    group_by(id) %>%
    mutate(diff = ifelse(length(id) == 2, abs(DataValue[complete == TRUE] - DataValue[complete == FALSE]),0))

  expect_true(max(tab$diff) <= 5)

})

test_that("There are no missing age groups",{

  min_agestart <- min(clean_df$AgeStart[clean_df$AgeSpan >0], na.rm = TRUE)
  max_ageend <- max(clean_df$AgeEnd[clean_df$AgeSpan >0], na.rm = TRUE)
  std_sub <- std_age_function() %>%
    filter(AgeStart >= min_agestart & AgeEnd <= max_ageend & complete == FALSE) %>%
    distinct(AgeLabel) %>%
    pull()

  expect_true(all(std_sub %in% clean_df$AgeLabel))
})


test_that("Only one total should exist per id and indicator ID",{

  agesorts <- clean_df %>%
    select(id, SexID, complete, AgeLabel, AgeSort, note) %>%
    # select(id, IndicatorID,SexID, complete, AgeLabel, AgeSort, note) %>%
    # filter(AgeLabel == "Total" & is.na(note)) %>%
    filter(AgeLabel == "Total") %>%
    group_by(id, SexID) %>%
    mutate(complete = zoo::na.locf0(complete)) %>%
    ungroup() %>%
    group_by(id,SexID, complete) %>%
    mutate(counter = length(unique(AgeSort)),
           list_agesort = paste(AgeSort, collapse = "_")) %>%
    ungroup() %>%
    pull(list_agesort) %>%
    unique()

  expect_identical(ifelse(length(agesorts) > 0, agesorts, "184"), "184")

})


}else skip("Data does not contain harmonised population counts")
}else skip ("Data does not exist")
