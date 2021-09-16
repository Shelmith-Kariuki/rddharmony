
test_that("the resulting dataset has only one series ID", {
  df <- vitals_std_valid_sample2
  df <- dd_rank_id_vitals(df)
  tab <- df %>%
          group_by(TimeLabel) %>%
          summarise(counter = length(unique(id)))

  expect_length(unique(tab$counter), 1)
})
