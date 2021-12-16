## This script runs DDharmonize_validate_PopCounts() and reports if there are time labels present in the
## raw data but not in the clean data
##
## Load the packages
library(rddharmony)
library(tidyverse)
library(googlesheets4)

## Create the gSheet
# ss <- gs4_create("rddharmony_missing_data", sheets = c("Births", "Deaths", "Population"))

## List of location ids
locids = get_locations()$PK_LocID

# locs <- get_locations() %>% select(PK_LocID, Name) %>% ungroup() %>%  unnest(cols = c())
# write_sheet(locs, ss = ss, sheet = "LocIDs")

## Read in the sheet
ss <- "https://docs.google.com/spreadsheets/d/1ufEdlGNzuYAMZktEHK984d8DcC34bg8sbNWbyYR7mzc/edit#gid=2141389750"

## Run the code
for (i in 1: length(locids)){

  clean_df <- DDharmonize_validate_PopCounts(locid = locids[i],
                                               times = c(1950, 2021),
                                               process = c("census", "estimate", "register"),
                                               return_unique_ref_period = TRUE,
                                               retainKeys = FALSE)

  if(!is.null(clean_df) & length(missing_timelabs) > 0){
    sheet_append(ss = ss,  missing_data, sheet = "Population" )
  }
  missing_timelabs <- 0
  missing_data <- tibble()
}

## This script is ran on the terminal using the code:
## Rscript deaths_missing_timelabels.R
## shelmith@shelmith-Precision-5510:~/Documents/Github/rddharmony/_archive$ Rscript pop_missing_timelabels.R
