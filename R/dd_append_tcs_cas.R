
#' dd_append_tcs_cas
#'
#' @param indata Births / Deaths data that is already harmonized
#' @param type births or deaths
#' @param tcs_data Total counts by sex data
#' @param ind 159 (where type == "births) or 188 (where type == "deaths)
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @return A dataset that has both Total counts by sex data and Counts by age and sex
#' @export
#'
#' @examples
#'
dd_append_tcs_cas <- function(indata, type, tcs_data, ind){

## Total counts by sex data refers to either Total births by sex (indicator 159) or Total deaths by sex (indicator 188)

## Append the harmonized data with the Total counts by sex data
merged_df <- indata %>%
  bind_rows(., tcs_data %>% select(any_of(names(indata)))) %>%
  mutate(serial_no = seq_along(id)) %>%
  arrange(id, desc(IndicatorID))

## If Total counts by sex record exists and the harmonized data does not exist, drop the former
merged_df2 <- merged_df %>%
              group_by(id, SexID) %>%
              mutate(todrop = ifelse(any(IndicatorID == ind) & !any(IndicatorID!=ind) & IndicatorID==ind, "drop", "")) %>%
              filter(todrop != "drop") %>%
              select(-todrop) %>%
              ungroup()

## If the Total counts by sex record does not match the total value of the harmonized data, drop it
if(nrow(merged_df2) >0){
merged_df3 <- merged_df2 %>%
              group_by(id, SexID) %>%
              mutate(present_tcs = ifelse(any(IndicatorID == ind), 1,0)) %>%
              mutate(eq = ifelse(all(present_tcs == 1) & IndicatorID == ind &
                                  !DataValue %in% DataValue[IndicatorID != ind & AgeLabel == "Total"],
                                "drop", "")) %>%
              filter(eq != "drop") %>%
              select(-eq, -present_tcs)%>%
              ungroup()
}else{
  merged_df3 <- merged_df2
}


## Ensure we have one id per time label
merged_df3 <- merged_df3 %>%
             group_by(TimeLabel) %>%
             dd_rank_id_vitals() %>%
              ungroup()

## Drop duplicate records (especially where we may have duplicate Total counts by sex records)
merged_df3 <- merged_df3 %>%
              distinct(id, SexID, AgeLabel, DataValue,abridged, complete, .keep_all = TRUE) %>%
              arrange(id, SexID, desc(IndicatorID))

## Generate a dataset of the dropped records
if(type == "births"){

skipped <- merged_df %>%
            filter(!serial_no %in% merged_df2$serial_no)%>%
            mutate(note = "Record to be disregarded since either births by age of mother and sex of child don't exist for this id and sex,
                   the record does not match the births by age of mother and sex of child total value, or it is a duplicate")
}else{
  skipped <- merged_df %>%
    filter(!serial_no %in% merged_df2$serial_no)%>%
    mutate(note = "Record to be disregarded since either deaths by age and sex don't exist for this id and sex,
the record does not match the deaths by age and sex total value, or it is a duplicate")

}

## Append the dropped records to the new data
out <- merged_df3 %>%
        bind_rows(.,skipped) %>%
        arrange(id, SexID, desc(IndicatorID)) %>%
        select(-serial_no)

## Do we return only the clean records (merged_df3) or everything including the dropped records (out), to confirm with Sara
## Returning everthing for now for testing purposes. But the final function should have return(merged_df3)
return(out)
}

