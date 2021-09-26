library(tidyverse)
library(DemoTools)
library(DDSQLtools)
clean_df <- DDharmonize_validate_BirthCounts(locid = sample(get_locations()$PK_LocID, 1),
                                             times = c(1950, 2020),
                                             process = c("census", "vr"),
                                             return_unique_ref_period = TRUE,
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/")

# dd_extract <- DDextract_VitalCounts(24,
#                                    type = c("births"),
#                                    process = c("census","vr"),
#                                    1950,
#                                    2050,
#                                    DataSourceShortName = NULL,
#                                    DataSourceYear = NULL)


## Tests for the clean data
## 1. The data has unique locid and locname
## 2. Only one indicator id == 159 per id
## 3. Indicator 159 Total value should be equal to indicator 170 Total value
## 4. An age label cannot be complete and abridged at the same time
## 5. Abridged labels should either contain a 0, a range e.g 5-9 and or an open age group (with a +)
## 6. Complete cases should not contain wide age groups
## 7. Every id should have a closing age group
## 8. All Data values should add up to the Total, for each id
## 9. There should not be any unknown in the data
## 10.There shouldn't be any negative data values
## 11. Complete series should begin with 0
## 12 Abridged series should include the following values: 0, 0-4, 1-4, 5-9, 10-14 as the start values
## 13. Each age label should be unique per id
## 14. We should have one unique id per time label
## 15. If the data has both complete and abridged series, the totals should be equal. Use "276 - Germany - VR - Births - 2007 - Register - Eurostat Database - Year of occurrence - Direct - High quality" to test this.
## 16. We should not have missing age groups.
## 17. Newzealand 2020 has unknowns == 0 so remove them
## 18. Check the warnings in 474: Martinique
## 19. Check that in cases where indicator 170 data does not exist, a message is returned
## 20. Check the warning in 807 (North Macedonia)
## 21.
