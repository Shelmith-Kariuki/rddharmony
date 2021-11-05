library(DemoTools)
library(DDSQLtools)
library(rddharmony)
locid2 = sample(get_locations()$PK_LocID, 1)
times = c(1950, 2020)
process = c("census", "vr")
process_pops = c("census","register", "estimate")
return_unique_ref_period = TRUE
DataSourceShortName = NULL
DataSourceYear = NULL
retainKeys = FALSE
server = "https://popdiv.dfs.un.org/DemoData/api/"

## Package function
births_df <- DDharmonize_validate_BirthCounts(locid = locid2,
                                              times = times,
                                              process = process,
                                              return_unique_ref_period = return_unique_ref_period,
                                              retainKeys = retainKeys)

deaths_df <- DDharmonize_validate_DeathCounts(locid = locid2,
                                              times = times,
                                              process = process,
                                              return_unique_ref_period = return_unique_ref_period,
                                              retainKeys = retainKeys)

popcounts_df <- DDharmonize_validate_PopCounts(locid = locid2,
                                                 times = times,
                                                 process = process_pops,
                                                 return_unique_ref_period = return_unique_ref_period,
                                                 retainKeys = retainKeys)
