#' dd_oag_agestart
#'
#' Identify the starting age needed for the open age group that closes the series. For abridged data, it must be a multiple of 5
#'
#' @param data The dataset to be harmonized
#' @param multiple5 Whether the series contains 5 year age labels or not
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A value that would close the series. It has to be a multiple of 5
#'
#' @export
#'
#' @examples
#' df <- series_isfull_df
#' oag_start <- dd_oag_agestart(df, multiple5 = TRUE)

dd_oag_agestart <- function(data, multiple5 = TRUE){

  ## Shel added suppressWarnings() to remove the following warning: In max(AgeStart) : no non-missing arguments to max; returning -Inf
  ## "748 - Eswatini - VR - Deaths - 2016 - Register - Demographic Yearbook - Year of registration - Direct - Low"
  maxage <- suppressWarnings(data %>%
    dplyr::filter(AgeSpan > 0) %>%
    dplyr::filter(AgeStart == max(AgeStart)))

  oag_start <- maxage$AgeEnd

  if (multiple5 == TRUE) {
    # ensure it is a multiple of 5
    oag_start <- floor(oag_start/5) * 5
  }

  return(oag_start)
}
