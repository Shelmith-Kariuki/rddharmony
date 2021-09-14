
df <- dd_single2abridged(vitals_abr_cpl2)

test_that("the function does not distort data values", {

df2 <- df %>%
       mutate(AgeLabel = as.character(AgeLabel),
              AgeStart = as.character(AgeStart)) %>%
       filter(AgeSpan > 1)  %>%
      select(AgeLabel, AgeStart, DataValue) %>%
      rename(AgeLabel_new = AgeLabel,
             DataValue_new = DataValue)

master_df <- vitals_abr_cpl2 %>%
  filter(AgeSpan >= 1)  %>%
  full_join(., df2, by = c("AgeLabel" = "AgeStart")) %>%
  mutate(AgeLabel_new = zoo::na.locf0(AgeLabel_new),
         DataValue_new = zoo::na.locf0(DataValue_new))

tab <- master_df %>%
        group_by(AgeLabel_new, DataValue_new) %>%
        summarise(DataValue_cpl = sum(DataValue, na.rm = TRUE)) %>%
        mutate(dif = abs(DataValue_cpl - DataValue_new))



  expect_equal(sum(tab$dif, na.rm = TRUE), 0)
})


test_that("the resulting data has no missing age groups", {
  min_agestart <- min(df$AgeStart[df$AgeSpan >0], na.rm = TRUE)
  max_ageend <- max(df$AgeEnd[df$AgeSpan >0], na.rm = TRUE)
  std_sub <- std_age_function() %>%
    filter(AgeStart >= min_agestart & AgeEnd <= max_ageend & complete == FALSE) %>%
    distinct(AgeLabel) %>%
    pull()

  expect_true(all(std_sub %in% df$AgeLabel))
})
