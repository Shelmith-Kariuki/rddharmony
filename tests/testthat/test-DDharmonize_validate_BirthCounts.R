clean_df <- DDharmonize_validate_BirthCounts(locid = sample(get_locations()$PK_LocID, 1),
                                             times = c(1950, 2020),
                                             process = c("census", "vr"),
                                             return_unique_ref_period = TRUE,
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/")


test_that("The data has a unique locid", {
  expect_length(unique(clean_df$LocID), 1)
})

test_that("The data has a unique locname", {
  expect_length(unique(clean_df$LocName), 1)
})

test_that("At most one indicatorid == 159 'Total' age label exists per id", {
  tab <- clean_df %>%
          group_by(id) %>%
          summarise(n = sum(IndicatorID == 159 & AgeLabel == "Total", na.rm = TRUE)) %>%
          ungroup()
  expect_true(max(tab$n) == 1)
})

test_that("Only one indicatorid == 170 'Total' age label exists per id", {
  tab <- clean_df %>%
    filter(IndicatorID == 170 & AgeLabel == "Total") %>%
    group_by(id, abridged) %>%
    summarise(n = n()) %>%
    ungroup()
  expect_true(unique(tab$n) == 1)
})

test_that("In cases where indicatorid == 159 'Total' age label exists, it should be equal to indicatorid == 170 'Total' age label", {
  tab <- clean_df %>%
          filter(AgeLabel == "Total") %>%
          select(id, IndicatorID, DataValue) %>%
          group_by(id) %>%
          mutate(diff = ifelse(any(IndicatorID == 159),
                               DataValue[IndicatorID == 159] - DataValue[IndicatorID == 170],
                               0))

  expect_true(all(tab$diff == 0))
})

test_that("An age label cannot be complete and abridged at the same time", {
  tab <- clean_df %>%
          mutate(checker = ifelse(abridged != complete | AgeLabel == "0-4" |
                                    IndicatorID == 159 |!is.na(note), TRUE, FALSE))
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


test_that("Every id has a closing age group",{
  tab <- clean_df %>%
          group_by(id) %>%
          mutate(oag_present = ifelse(!is.na(note)|
                                  is.na(note) & any(AgeLabel %in% grep("\\+", AgeLabel, value = TRUE,ignore.case = TRUE)),
                                      TRUE, FALSE))
  expect_true(all(tab$oag_present == TRUE))

})


test_that("All Data values add up to the Total, for each id", {

})

test_that("There isn't any unknown in the data", {

})


test_that("There aren't any negative data values", {

})

test_that("The age labels begin with a 0, whether abridged or complete", {

})

test_that("Abridged series start with the following values: 0, 0-4, 1-4, 5-9, 10-14", {

})

test_that("Each age label is unique per id:", {

})

test_that("There is only one unique id per time label",{

})

test_that("if the data has both complete and abridged series, they should be equal",{

})

test_that("There are no missing age groups",{

})

test_that("If two totals exist per id, one has to be AgeSort == 184 (indicator 170) and the other has to be AgeSort == 999 (indicator 159).",{

})
