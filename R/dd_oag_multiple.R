#' dd_oag_multiple
#'
#' Check whether the series has more than one open age group
#'
#' @param data The data to be harmonised
#'
#' @return `TRUE/FALSE`
#' @export
#'
dd_oag_multiple <- function(data){
  # require(tidyverse)
  oags <- data$AgeLabel[data$AgeSpan == -1 & data$AgeStart != 0]

  check <- length(oags) > 1

  return(check)
}
