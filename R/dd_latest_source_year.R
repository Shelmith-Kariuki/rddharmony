#' dd_latest_source_year
#' Select the latest data source year. For some periods of reference, there are multiple values in the **DataSourceYear** variable. For these cases, the criteria is to choose the latest **DataSourceYear**
#' @param data The data to be harmonized
#'
#' @return A dataset with a unique **DataSourceYear**
#' @import tidyverse
#' @export

dd_latest_source_year <- function(data){
  # require(tidyverse)
  data.out <- data %>%
    group_by(AgeLabel) %>%
    slice(which.max(DataSourceYear)) %>%
    ungroup()
  return(data.out)
}
