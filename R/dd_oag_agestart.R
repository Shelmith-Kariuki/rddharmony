#' dd_oag_agestart
#'
#' Identify the starting age needed for the open age group that closes the series. For abridged data, it must be a multiple of 5
#'
#' @param data The dataset to be harmonized
#' @param multiple5 Whether the series contains 5 year age labels or not
#'
#' @return A value that has to be a multiple of 5
#'
#' @import DDSQLtools
#' @import DemoTools
#' @import tidyverse
#' @import dplyr
#' @export

dd_oag_agestart <- function(data, multiple5 = TRUE){

  maxage <- data %>%
    dplyr::filter(AgeSpan > 0) %>%
    dplyr::filter(AgeStart == max(AgeStart))

  oag_start <- maxage$AgeEnd

  if (multiple5 == TRUE) {
    # ensure it is a multiple of 5
    oag_start <- floor(oag_start/5) * 5
  }

  return(oag_start)
}
