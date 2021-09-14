
df <- vitals5_df
df <- DDharmonize_Vitals5(df, type = "births")

test_that("the resulting data has an open age group that closes the series", {
  cag <- df$AgeLabel[grepl("\\+", df$AgeLabel)]
  expect_false(purrr::is_empty(cag))
})

test_that("the resulting data has an `Unknown` age label", {
  uag <- df$AgeLabel[grepl("Unknown", df$AgeLabel)]
  expect_true(!purrr::is_empty(uag))
})

test_that("the resulting data does not have wide age groups", {
  wags <- df$AgeLabel[df$AgeSpan > 5]
  expect_true(purrr::is_empty(wags))
})

test_that("the resulting data has no missing age group", {
  min_agestart <- min(df$AgeStart[df$AgeSpan >0], na.rm = TRUE)
  max_ageend <- max(df$AgeEnd[df$AgeSpan >0], na.rm = TRUE)
  std_sub <- std_age_function() %>%
             filter(AgeStart >= min_agestart & AgeEnd <= max_ageend & complete == FALSE) %>%
              distinct(AgeLabel) %>%
              pull()

  expect_true(all(std_sub %in% df$AgeLabel))
})


test_that("the resulting data has a `Total` age group", {
  tag <- df$AgeLabel[grepl("Total", df$AgeLabel)]
  expect_true(!purrr::is_empty(tag))
})

test_that("the abridged age labels either have a `-` or a `+`",{
  als <- df$AgeLabel[grepl("\\+|-", df$AgeLabel)]
  expect_true(any(!df$AgeLabel %in% als))
})

test_that("the data has a note, abridged and complete variable",{
  expect_true(all(c("note", "abridged", "complete") %in% names(df)))
})
