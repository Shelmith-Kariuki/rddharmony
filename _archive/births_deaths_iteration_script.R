require(DDSQLtools)
require(DemoTools)
require(tidyverse)
require(rddharmony)

locid <- sample(get_locations()$PK_LocID, 1)
# locid <- 340
births_df <- DDharmonize_validate_BirthCounts(locid,
                                              c(1950,2020),
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")

deaths_df <- DDharmonize_validate_DeathCounts(104,
                                              2010,
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")


## Myanmar 104 2010
## If the datasource of ind 188 does not match the datasource of the rest, drop
## what of datasource year?

## **********************************************************************************************
sample_timelabel <- sample(unique(births_df$TimeLabel), 1)

sample_births <- births_df %>%
  filter(TimeLabel == sample_timelabel)

sample_deaths <- deaths_df %>%
  filter(TimeLabel == sample_timelabel)

##
## 412 - Kosovo giving the warning In max(AgeStart) : no non-missing arguments to max; returning -Inf, In rm(abr, check_abr, abr_oag) : object 'check_abr' not found
##
