test_that("the output of this function should be boolean (TRUE / FALSE)", {

  out <- dd_oag_multiple(oag_multiple_df)
  expect_equal(class(out), "logical")

})


test_that("the output of this function is of length 1", {

  out <- dd_oag_multiple(oag_multiple_df)
  expect_length(out, 1)

})

test_that("if the output of the function is FALSE, then there is none, or only one age label that is open", {
  out <- dd_oag_multiple(oag_multiple_df)

  oags <- oag_multiple_df$AgeLabel[grepl("\\+", oag_multiple_df$AgeLabel)]

  expect_true(if_else(out == FALSE & length(oags) <= 1, TRUE, FALSE ))
})

# test_that("if the output of the function is TRUE, then there is more than one age label that is open", {
#   out <- dd_oag_multiple(oag_multiple_df)
#
#   oags <- oag_multiple_df$AgeLabel[grepl("\\+", oag_multiple_df$AgeLabel)]
#
# expect_true(if_else(out == TRUE & length(oags) > 1, TRUE, FALSE ))
# })
