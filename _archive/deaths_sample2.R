require(DDSQLtools)
require(DemoTools)
require(tidyverse)
require(rddharmony)

locid <- sample(get_locations()$PK_LocID, 1)
clean_df <- DDharmonize_validate_DeathCounts(840,
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
#
