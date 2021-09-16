df <- vitals_std_valid_sample %>% filter(series == "complete reconciled with abridged")
df <- dd_distribute_unknowns(df)

test_that("unknowns no longer exist after running this function", {

    expect_false(any("Unknowns" %in% df$AgeLabel))

})


test_that("computed totals are equal to reported totals after unknowns are dropped", {

  reported_tot <- df$DataValue[df$AgeLabel == "Total"]
  reported_calc <- sum(df$DataValue[df$AgeLabel != "Total"], na.rm = TRUE)
  expect_true(reported_tot == reported_calc)

})
