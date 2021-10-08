# selected_id <- sample(unique(dd_extract$id), 1)




selected_id <- sample(unique(vitals_std_all$id), 1)

x <- dd_extract %>%
  filter(id ==  selected_id ) %>%
  select(id, IndicatorID, IndicatorName, SexName, DataSourceYear, TimeMid, TimeLabel, AgeStart, AgeSpan, AgeLabel, DataValue)

x1 <- dd_extract_194_195 %>%
  filter(id ==  selected_id ) %>%
  select(id, IndicatorID, IndicatorName, SexName, DataSourceYear, TimeMid, TimeLabel, AgeStart, AgeSpan, AgeLabel, DataValue)

x2 <- dd_extract_188 %>%
  filter(id ==  selected_id ) %>%
  select(id, IndicatorID, IndicatorName, SexName, DataSourceYear, TimeMid, TimeLabel, AgeStart, AgeSpan, AgeLabel, DataValue)

x3 <- vitals_std_all %>%
       filter(id == selected_id)

x4 <- vitals_std_full %>%
  filter(id == selected_id)
