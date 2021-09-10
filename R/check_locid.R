#' check_locid
#'
#' Throws an error message if a location id is not part of the location ids in the UNDP database, and a confirmation message showing the location name if the location id is valid.
#'
#' @param locid location id
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return The message: "`locid` is not a valid location id. Please run View(get_locations()) to get a list of plausible location ids. They are listed in the `PK_LocID` variable" in the case
#' where the user has inserted an invalid location id, and in cases where the location id is valid, the following message is returned: "`locid` is a valid location id. The location name is `location name`"
#'
#' @export
#'
#' @examples
#' check_locid(12345) ## invalid location id
#' check_locid(404) ## valid location id

check_locid <- function(locid){

  ## Create a list of plausible location ids
  possible_ids <- DDSQLtools::get_locations() %>% distinct(PK_LocID, Name)

  if (!locid %in% possible_ids$PK_LocID) {

    ## If a location id is not part of the possible ids, throw an error message.
    print( paste0(locid," is not a valid location id. Please run View(get_locations()) to get a list of plausible location ids. They are listed in the `PK_LocID` variable"))

  }else{
    ## If a location id is part of the possible ids, show a confirmatory message.
    print( paste0(locid," is a valid location id. The location name is ", possible_ids$Name[possible_ids$PK_LocID == locid]))

    }
}
