#' DDextract_VitalCounts
#'
#' Extracts births and/or deaths data for a given country and time period from the UNDP portal.
#'
#' @param locid The Location Id for each country. Run `DemoTools::get_locations()` to get information about available locations. The ids are indicated in the `PK_LocID` variable.
#' @param type The type of data to be pulled i.e. births/deaths
#' @param process The data collection process i.e. Census (census) or Vital Registrations (vr)
#' @param start_year The minimum year for the data available for each country
#' @param end_year The maximum year for the data available for each country
#' @param DataSourceShortName NULL.
#' @param DataSourceYear NULL.
#' @return A dataset showing counts for each location, type of data (births or deaths), process, year, sex and age label.
#' @export
#'
#' @examples
#' dd_extract <- DDextract_VitalCounts(404, #Kenya
#'                                    type = c("births"),
#'                                    process = c("census","vr"),
#'                                    1950,
#'                                    2050,
#'                                    DataSourceShortName = NULL,
#'                                    DataSourceYear = NULL)
DDextract_VitalCounts <- function(locid,
                                  type = c("births","deaths"),
                                  process = c("census","vr"),
                                  start_year, end_year,
                                  DataSourceShortName = NULL,
                                  DataSourceYear = NULL) {

## List the indicator ids for each type of data
  if (type == "births") {
    indicator_ids <- c(159,170) # total births and births by age of mother
  } else if (type == "deaths") {
    indicator_ids <- c(194, 195, 188)
  }

  ## Indicate the data process id. dpi == 2 if process is census and 36 if process is vr
  dpi <- ifelse(process == "census", 2, 36)

  ## Extract the data from the UNDP portal and return NULL if the data does not exist
  tryCatch({
    vital_counts <- DDSQLtools::get_recorddata(locIds = locid,
                                   dataProcessIds = dpi, # Census or register
                                   indicatorIds = indicator_ids,
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = 2, # whole area (as opposed to urban/rural or some other sub-national unit)
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear)
  }, error=function(e){check_locid(locid)})


  ## If the data exists, it will appear on the environment as a dataframe, else it will appear as NULL
  if (exists('vital_counts')) {

    vital_counts <- vital_counts

    ## Print a text message showing the locid and the locname of the data extracted
    cat("Location ID: ", unique(vital_counts$LocID),"\n",
        "Location Name: ", unique(vital_counts$LocName))

  } else { vital_counts <- NULL }

  outdata <- vital_counts

  return(outdata)

}
