#' dd_latest_source_year
#'
#' Select the latest data source year. For some periods of reference, there are multiple values in the **DataSourceYear** variable. For these cases, the criteria is to choose the latest **DataSourceYear**
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with a unique **DataSourceYear**
#'
#' @export
#' @examples
#' df <- vitals5_df
#' df <- dd_latest_source_year(df)

dd_latest_source_year <- function(data){
  data.out <- data %>%
    group_by(AgeLabel) %>%
    slice(which.max(DataSourceYear)) %>%
    ungroup()
  return(data.out)
}
