
test_that("the starting age needed for the open age group that closes the series for abridged data is a multiple of 5", {

  df <- series_isfull_df

  oag_start <- dd_oag_agestart(df, multiple5 = TRUE)

  expect_equal(oag_start %% 5, 0)

})


