df_cpl <- vitals1_std
df_cpl <- dd_births_abridged2single_1014(df_cpl, 4)
df_abr <- vitals5_std

test_that("the function results into 10 - 14 single years", {
  expect_true(any(c("10", "11", "12", "13", "14") %in% df_cpl$AgeLabel[df_cpl$abridged == FALSE]))

})

test_that("10 is the minimum age label",{
  expect_true(suppressWarnings(min(as.numeric(df_cpl$AgeLabel), na.rm = TRUE)) == 10)
})

test_that("the minimum age label in abridged data is the same as the minimum age label in the complete data",{
  expect_true(suppressWarnings(min(as.numeric(df_cpl$AgeLabel), na.rm = TRUE)) ==
                suppressWarnings(min(as.numeric(df_abr$AgeStart[df_abr$AgeSpan >0 ]), na.rm = TRUE)))
})

test_that("the datavalues of the new age labels sum up to the second argument, in this case 4",{
  val <- df_cpl %>%
          filter(AgeLabel %in% c("10", "11", "12", "13", "14")) %>%
          summarise(summation = sum(DataValue, na.rm = TRUE)) %>%
          pull(summation)
  expect_equal(val, 4)
})
