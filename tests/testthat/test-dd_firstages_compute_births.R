test_that("the output of this function is a dataframe", {

  df <- firstages_compute_births_df
  expect_true("data.frame" %in% class(df))
})


test_that("if 10-14 is missing and 0-14 is present, 0-14 becomes 10-14", {
  df <- firstages_compute_births_df
  dv10_14 <- any("10-14" %in% df$AgeLabel)
  dv0_14 <- any("0-14" %in% df$AgeLabel)
  df2 <- dd_firstages_compute_births(df)
  dv10_14_2 <- any("10-14" %in% df2$AgeLabel)

  expect_equal(if_else(dv10_14==FALSE & dv0_14 == TRUE, TRUE, FALSE), dv10_14_2)

})

# test_that("if 0-19 is present and 0-14 is also present and 15-19 is missing, then the function generates '15-19'",{
#   df <- firstages_compute_births_df
#   dv0_19 <- any("0-19" %in% df$AgeLabel)
#   dv0_14 <- any("0-14" %in% df$AgeLabel)
#   dv15_19 <- any("15_19" %in% df$AgeLabel)
#   df2 <- dd_firstages_compute_births(df)
#   dv15_19_2 <- any("15-19" %in% df2$AgeLabel)
#
#   expect_equal(if_else(dv0_19==TRUE & dv0_14 == TRUE & dv15_19 == FALSE, TRUE, FALSE), dv15_19_2)
#
# })


# test_that("if 0-19 is present and 0-14 is also present and 15-19 is missing, then '15-19' = '0-19' - '0-14'",{
#
#     df <- firstages_compute_births_df
#     dv0_19 <- any("0-19" %in% df$AgeLabel)
#     dv0_14 <- any("0-14" %in% df$AgeLabel)
#     dv15_19 <- any("15_19" %in% df$AgeLabel)
#     df2 <- dd_firstages_compute_births(df)
#     dv15_19_2 <- any("15-19" %in% df2$AgeLabel)
#     val0_19 <- df$DataValue[df$AgeLabel == "0-19"]
#     val0_14 <- df$DataValue[df$AgeLabel == "0-14"]
#     val15_19 <- df2$DataValue[df2$AgeLabel == "15-19"]
#
#     expect_equal(if_else(dv0_19==TRUE & dv0_14 == TRUE & dv15_19 == FALSE, val0_19 - val0_14, 0), val15_19)
#
#   })
