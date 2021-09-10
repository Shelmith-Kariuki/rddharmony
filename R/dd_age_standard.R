#' dd_age_standard
#'
#' Add standard age groups in the dataset
#'
#' @param data The data to be harmonized
#' @param abridged `TRUE/FALSE`
#'
#' @return The DemoData dataset with standard age groups appended to it.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' df <- firstages_compute_births_df
#' df <- dd_age_standard(df)

dd_age_standard<- function(data, abridged = TRUE){

  # get stanadard age groups
  std_ages <- std_age_function()

  if (abridged) {
    std_ages <- std_ages %>%
      dplyr::filter(abridged == TRUE)
  } else { std_ages <- std_ages %>%
    dplyr::filter(complete==TRUE)
  }
  std_ages <- std_ages %>%
    select(-abridged, -complete)


  # join standard groups with the data from DemoData
  data.out <- data %>%
    full_join(x=std_ages,
              by=c("AgeStart","AgeEnd","AgeLabel","AgeSpan")) %>%
    arrange(AgeSort)

  return(data.out)
}
