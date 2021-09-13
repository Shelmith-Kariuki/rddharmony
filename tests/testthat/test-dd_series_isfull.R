## data type of the output should be boolean (TRUE / FALSE)

test_that("the data type of the output is boolean (TRUE / FALSE)", {

  df <- series_isfull_df

  output <- dd_series_isfull(df, abridged = TRUE)

  expect_identical(class(output), "logical")
})

## if the series is full, it should contain all the agelabels

test_that("if the series is full, it should contain all the agelabels (abridged == TRUE)", {

  df <- series_isfull_df
  oag <- dd_oag_agestart(series_isfull_df)
  output <- dd_series_isfull(df, abridged = TRUE)
  ages_closed <- c(0,seq(5,max(5,oag-5),5))
  all_closed <- all(ages_closed %in% df$AgeStart[df$AgeSpan > 0])
  all_closed_and_oag <- all_closed & paste0(oag,"+") %in% df$AgeLabel
  all_closed_and_total <- all_closed & "Total" %in% df$AgeLabel
  ages_child_abr <- all(c("0","1-4") %in% df$AgeLabel)
  ages_child_five <- all(c("0-4","5-9") %in% df$AgeLabel)

  is_full <- any(all_closed_and_oag, all_closed_and_total)
  is_full <- is_full & (ages_child_abr | ages_child_five)

  expect_identical(output, is_full)
})

test_that("if the series is full, it should contain all the agelabels (abridged == FALSE)", {

  df <- series_isfull_df
  oag <- dd_oag_agestart(series_isfull_df)
  output <- dd_series_isfull(df, abridged = FALSE)
  ages_closed <- c(0,seq(5,max(5,oag-5),5))
  all_closed <- all(ages_closed %in% df$AgeStart[df$AgeSpan > 0])
  all_closed_and_oag <- all_closed & paste0(oag,"+") %in% df$AgeLabel
  all_closed_and_total <- all_closed & "Total" %in% df$AgeLabel

  is_full <- any(all_closed_and_oag, all_closed_and_total)

  expect_identical(output, is_full)

})
