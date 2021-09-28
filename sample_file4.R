library(tidyverse)
library(DemoTools)
library(DDSQLtools)
library(rddharmony)
clean_df <- DDharmonize_validate_BirthCounts(locid = sample(get_locations()$PK_LocID, 1),
                                             times = c(1950, 2020),
                                             process = c("census", "vr"),
                                             return_unique_ref_period = TRUE,
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/")


range(clean_df$DataValue)
any(clean_df$DataValue == "Unknown")

# dd_extract <- DDextract_VitalCounts(474,
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
## 15. If the data has both complete and abridged series, the totals should be equal.
##  Use "276 - Germany - VR - Births - 2007 - Register - Eurostat Database - Year of occurrence - Direct - High quality" to test this.
##  Use "470 - Malta - VR - Births - 2019 - Register - Eurostat Database - Year of occurrence - Direct - High quality"
## 16. We should not have missing age groups.
## 17. If 2 totals exist per id, one has to be AgeSort == 184 (indicator 170) and the other has to be AgeSort == 999 (indicator 159).
## Use "492 - Monaco - VR - Births - 1950 - Register - Demographic Yearbook - Year of occurrence - Direct - Low" to test this
## 18. Check for locations that are in get_locations() and not in get_datasources(). ASk Sara why this is so.
## 19. Check that in cases where indicator 170 data does not exist, a message is returned

## Issues to be dealt with ----------------------------------------------------
## 17. Newzealand 2020 has unknowns == 0 so remove them
## Unknowns in "784 - United Arab Emirates - VR - Births - 1991 - Register - Births and Deaths 1991 - Year of occurrence - Direct - Fair"
## Unknowns in 60 - Bermuda - VR - Births - 2006 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair
## 18. Check the warnings in 474: Martinique, 304: Greenland, 412: Kosovo
## 20. Check the warning in 807 (North Macedonia), 690 (Seychelles), 20 (Andorra) : 498 (Republic of Moldova)
## 21. Locid == 686 (Senegal) has a lot of data values dropped. Investigate this.
## 23. Check warnings for locid = 484 , locname = Mexico, locid = 234, locname = Faeroe Islands.
## 24. Find out why "484 - Mexico - VR - Births - 2012 - Register - Estadísticas de Natalidad - Year of occurrence - Direct - Fair" is not harmonised due to non-standard age groups.
## 26. 504 - Morocco - VR - Births - 2018 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair seems to have totals that are not equal. What happened?
## 27. Check 674 (San Marino) warnings
## 28. 226. Why is the raw data not being published when indicator 170 data does not exist?
## 504 - Morocco - VR - Births - 2018 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair
## Abridged totals not equal to complete totals
