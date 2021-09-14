test_that("the resulting dataset has all the standard age labels", {

  # get standard age groups
  std_ages <- std_age_function() %>% filter(abridged == TRUE)

  # read in the dataset
  df <- firstages_compute_births_df
  df <- dd_age_standard(df)

  expect_true(all(std_ages$AgeLabel %in% df$AgeLabel))
})
