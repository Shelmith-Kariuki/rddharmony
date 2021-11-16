library(DemoTools)
library(DDSQLtools)
library(rddharmony)
locid2 = sample(get_locations()$PK_LocID, 1)
# locid2 = 533
times = c(2000, 2010)
process = c("census", "vr")
process_pops = c("census","register", "estimate")
return_unique_ref_period = TRUE
DataSourceShortName = NULL
DataSourceYear = NULL
retainKeys = FALSE
server = "https://popdiv.dfs.un.org/DemoData/api/"

## Package functions
# births_df <- DDharmonize_validate_BirthCounts(locid = locid2,
#                                               times = times,
#                                               process = process,
#                                               return_unique_ref_period = return_unique_ref_period,
#                                               retainKeys = retainKeys)
#
#
#
# deaths_df <- DDharmonize_validate_DeathCounts(locid = locid2,
#                                               times = times,
#                                               process = process,
#                                               return_unique_ref_period = return_unique_ref_period,
#                                               retainKeys = retainKeys)
#
#

popcounts_df <- DDharmonize_validate_PopCounts(locid = locid2,
                                                 times = times,
                                                 process = process_pops,
                                                 return_unique_ref_period = return_unique_ref_period,
                                                 retainKeys = retainKeys)

table(raw_df$DataProcess, raw_df$DataProcessSort, useNA = "always")
table(raw_df$DataProcessType, raw_df$DataProcessTypeSort, useNA = "always")
table(raw_df$DataReliabilityName, raw_df$DataReliabilitySort, useNA = "always")
