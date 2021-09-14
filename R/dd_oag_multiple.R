#' dd_oag_multiple
#'
#' Check whether the series has more than one open age group
#'
#' @param data The data to be harmonized
#'
#' @return `TRUE/FALSE`
#'
#' @export
#'
#' @examples
#' dd_oag_multiple(oag_multiple_df)

dd_oag_multiple <- function(data){

  oags <- data$AgeLabel[data$AgeSpan == -1 & data$AgeStart != 0]

  check <- length(oags) > 1

  return(check)
}
