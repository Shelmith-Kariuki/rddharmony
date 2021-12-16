## This script runs DDharmonize_validate_BirthCounts() and reports if there are negative values in the data.
##
## Load the packages
library(rddharmony)
library(tidyverse)
library(googlesheets4)

## Create the gSheet
# ss <- gs4_create("rddharmony_missing_data", sheets = c("Births", "Deaths", "Population"))

## Read in the sheet
ss <- "https://docs.google.com/spreadsheets/d/1ufEdlGNzuYAMZktEHK984d8DcC34bg8sbNWbyYR7mzc/edit#gid=2141389750"

## List of location ids
locids = get_locations()$PK_LocID

## Run the code
for (i in 1: length(locids)){

  clean_df <- DDharmonize_validate_BirthCounts(locid = locids[i],
                                               times = c(1950, 2021),
                                               process = c("census", "vr"),
                                               return_unique_ref_period = TRUE,
                                               retainKeys = FALSE)

  if(!is.null(clean_df) & length(missing_timelabs) > 0){
   negs_df <-  clean_df %>%
     filter(DataValue < 0)

   if(nrow(negs) > 0){
    sheet_append(ss = ss,  negs_df, sheet = "Births_negatives" )
   }
  }
  negs_df <- tibble()
}

## Reached 710: SouthAfrica as of 16th Dec at 18:00hrs
