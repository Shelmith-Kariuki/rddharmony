test_that("if the computed total is greater than the reported total, replace reported with the computed", {
  df <- vitals_std_full_sample %>% filter(series == "complete reconciled with abridged")
  reported_tot <- df$DataValue[df$AgeLabel == "Total"]
  reported_calc <- sum(df$DataValue[df$AgeLabel != "Total"], na.rm = TRUE)
  expect_true(reported_tot == reported_calc)
})


# test_that("if the computed total is less than the reported total, then add difference to 'Unknown' age", {
#   expect_equal(2 * 2, 4)
# })
