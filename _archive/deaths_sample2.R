require(DDSQLtools)
require(DemoTools)
require(tidyverse)
require(rddharmony)

locid <- sample(get_locations()$PK_LocID, 1)
clean_df <- DDharmonize_validate_DeathCounts(locid,
                                              c(1950,2020),
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")


sweden_df <- DDharmonize_validate_BirthCounts(752,
                                              c(1950,2020),
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")

length(unique(clean_df$id))

## **********************************************************************************************
selected_id <- sample(unique(clean_df$id), 1)
selected_id

per_id <- clean_df %>%
  filter(id ==  selected_id)

#104 - Myanmar - VR - Deaths - 2005 - Register - Demographic Yearbook - Year of registration - Direct - Low indicator id and indicator name missing
# something is wrong with that
# "104 - Myanmar - VR - Deaths - 2010 - Register - Demographic Yearbook - Year of registration - Direct - Low" dd_rank_id_vitals should be per sex.
# But then given that both sexes is from Demographic year book and Males, Females are from WHO Country Consultation, the totals
# aint matching. Waiting to hear from Sara
# In max(AgeStart) : no non-missing arguments to max; returning -Inf warning is being generated in Part 2: dd_series_isfull
# Warning messages: 1: In if (check_latest_full) { :the condition has length > 1 and only the first element will be used
# Case study: 104 - Myanmar - VR - Deaths - 2012 - Register - Demographic Yearbook - Year of registration - Direct - Low
# Edited DDharmonize_Vitals1(), ##6.  Keep the latest datasource year from if (check_latest_full) to if (any(check_latest_full)) because we have cases
# where the series are two, one full and another not full but have the same data source year. So  check_latest_full  <- unique(df$check_full[df$DataSourceYear == latest_source_year]) ==
# c(FALSE, TRUE)
# From Sara
# Thank you for the fast response. So does it mean that for each id in the deaths data, the following should occur?
# 1) Male (available), Female(available), Both Sexes (available) ... KEEP
# 2) Male (available), Female(available), Both Sexes (missing) ... KEEP
# 3) Male (available), Female(missing), Both Sexes (available) ... DROP
# 4) Male (missing), Female(available), Both Sexes (available) ... DROP
# 5) Male (missing), Female(missing), Both Sexes (available) ... DROP
# 6) Male (available), Female(missing), Both Sexes (missing) ... DROP
# 7) Male (missing), Female(available), Both Sexes (missing) ... DROP

