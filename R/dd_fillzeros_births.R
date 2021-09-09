#' dd_fillzeros_births
#'
#' Fill in zeros for mother ages younger than 15 if there is no data reported
#'
#' @param data The data to be harmonized
#' @param abridged `TRUE/FALSE`
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with zeros filled in for ages younger than 15 if there is no data reported

#' @export
#'
#' @examples
#' df_abr <- dd_fillzeros_births(data = vitals_abr_cpl1 %>% select(-AgeSort), abridged = TRUE)
#' df_cpl <- dd_fillzeros_births(data = vitals_abr_cpl2 %>% select(-AgeSort), abridged = FALSE)

dd_fillzeros_births <- function(data, abridged = TRUE){

  # require(tidyverse)

  sexes <- unique(data$SexID)

  df_sex <- NULL
  for (sex in sexes) {

    df <- dd_age_standard(data %>% dplyr::filter(SexID == sex), abridged = abridged) %>%
      mutate(DataValue = replace(DataValue, is.na(DataValue & AgeStart < 15 & AgeSpan > 0), 0),
             SexID = sex) %>%
      dplyr::filter(!is.na(DataValue))

    df_sex <- rbind(df_sex, df)
  }

  return(df_sex)

}


