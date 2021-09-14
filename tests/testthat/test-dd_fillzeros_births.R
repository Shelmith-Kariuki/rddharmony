
df_abr <- dd_fillzeros_births(data = vitals_abr_cpl1_f0, abridged = TRUE)
df_cpl <- dd_fillzeros_births(data = vitals_abr_cpl2_f0, abridged = FALSE)
abr_min_agestart <- min(vitals_abr_cpl1_f0$AgeStart[vitals_abr_cpl1_f0$AgeSpan >= 1],na.rm = TRUE)
abr_added_labs <-  df_abr %>%
  filter(AgeStart < abr_min_agestart & AgeSpan >=1) %>%
  pull(AgeLabel)

cpl_min_agestart <- min(vitals_abr_cpl2_f0$AgeStart[vitals_abr_cpl2_f0$AgeSpan >= 1],na.rm = TRUE)
cpl_added_labs <-  df_cpl %>%
  filter(AgeStart < cpl_min_agestart & AgeSpan >=1) %>%
  pull(AgeLabel)

test_that("the resulting abridged dataset has additional ages younger than 15", {
  expect_true(all(abr_added_labs %in% df_abr$AgeLabel))
})

test_that("the resulting complete dataset has additional ages younger than 15", {
  expect_true(all(cpl_added_labs %in% df_cpl$AgeLabel))
})

test_that("the resulting abridged dataset has additional ages younger than 15 and their datavalues are not empty", {
  dvals <- df_abr %>%
    filter(AgeLabel %in% abr_added_labs) %>%
    pull(DataValue)
  expect_false(any(is.na(dvals)))
})
test_that("the resulting complete dataset has additional ages younger than 15 and their datavalues are not empty", {
  dvals <- df_cpl %>%
            filter(AgeLabel %in% cpl_added_labs) %>%
            pull(DataValue)
  expect_false(any(is.na(dvals)))
})



