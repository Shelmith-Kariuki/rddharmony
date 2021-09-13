test_that("if two identical age labels exist, then they definitely have the same DataSource year", {
  df <- latest_source_year_df
  df <- dd_latest_source_year(df)

  dups <- df$AgeLabel[duplicated(df$AgeLabel[df$AgeLabel != "Total"])]
  dsy <- df$DataSourceYear[df$AgeLabel == dups]

  expect_equal(length(unique(dsy)), 0)
})
