## Notes:
## "404 - Kenya - Estimate - 2014 - International Data Base (IDB) - Unknown - Model-based projections/extrapolations - High quality"
## Has both abridged and complete series
## Doesn't have totals
## Why does
## "218 - Ecuador - Estimate - 2014 - Global Burden of Disease Study 2017 (GBD2017): Population Estimates 1950-2017 - Unknown - Model-based estimates - Fair"
##  not have reconciled series? SexId == 3. The reason is coz total_abr == 15944697 and total_cpl = 15944697.3326416.
##  In the DDharmonize_AbridgedAndComplete, can we edit total_match <- total_diff == 0 to total_match <- total_diff <= 0.5. Issue sorted. Check with Sara
## "674 - San Marino - Census - 2010 - Demographic Yearbook - De-facto - Population by age and sex - Unknown" check this but it's okay for now
## 724 Spain has a lot of skipped data. Why?
## "364 - Iran (Islamic Republic of) - Census - 2011 - Demographic Yearbook - De-jure - Population by age and sex - Fair" hs missing DataSourceTypes
##
idx <- sample(ids, 1)
# idx <- "674 - San Marino - Census - 2010 - Demographic Yearbook - De-facto - Population by age and sex - Unknown"

xyz <- pop_std_all %>%
        filter(id == idx) %>%
          filter(SexID == sample(SexID, 1))

table(xyz$series, xyz$SexID)


xyz2 <- pop_std_full %>%
  filter(id == idx) %>%
  filter(SexID == unique(xyz$SexID))
table(xyz2$series, xyz2$SexID)

xyz3 <- pop_std_valid %>%
  filter(id == idx)

table(xyz3$abridged, xyz3$SexID, useNA = "always")

x <- out_all %>%
  group_by(TimeMid) %>%
  summarise(n = length(unique(id)))
unique(x$n)





locid <- sample(get_locations()$PK_LocID,1)


pop_df <- DDharmonize_validate_PopCounts(locid,
                                              c(1950,2020),
                                              process = c("census", "estimate", "register"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")

sample_timelabel <- sample(unique(pop_df$TimeLabel), 1)

sample_pop <- pop_df %>%
  filter(TimeLabel == sample_timelabel)

