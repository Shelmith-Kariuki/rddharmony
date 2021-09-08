#' check_locid
#'
#' Produces an error message if a location id is not part of the possible location ids in the UNDP database
#'
#' @param locid location id
#'
#' @return The message: `locid` is not a valid location id. Please run View(get_locations()) to get a list of plausible location ids. They are listed in the `PK_LocID` variable
#' @import dplyr
#' @export
#'
#' @examples
#' check_locid(12345)
check_locid <- function(locid){
  possible_ids <- get_locations() %>% distinct(PK_LocID) %>% pull()
  if (!locid %in% possible_ids) {
    print( paste0(locid," is not a valid location id. Please run View(get_locations()) to get a list of plausible location ids. They are listed in the `PK_LocID` variable"))
  }
}
