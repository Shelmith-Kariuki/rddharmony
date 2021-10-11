# selected_id <- sample(unique(dd_extract$id), 1)

length(unique(vitals_std_all$id))

selected_id <- sample(unique(vitals_std_all$id), 1)
selected_id

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

x4 <- vitals_std_valid %>%
  filter(id == selected_id) %>%
  filter(AgeLabel=="Total") %>%
  group_by(complete, id) %>%
  mutate(sum_bf = sum(DataValue[SexID <3], na.rm = TRUE)) %>%
  mutate(tot_bs = ifelse(any(SexID == 3), DataValue[SexID == 3], NA)) %>%
  mutate(diff = sum_bf - tot_bs)

val <- sum(x4$diff)
val


x5 <- out_all %>%
  group_by(TimeLabel) %>%
  summarise(n = length(unique(id)))

all(x5$n == 1)

View(dd_extract[dd_extract$TimeLabel %in% missing_timelabs,])
