
## Total counts by sex data refers to either Total births by sex (indicator 159) or Total deaths by sex (indicator 188)
dd_append_tcs_cas <- function(indata, type, tcs_data, ind){

## Append the harmonized data with the Total counts by sex data
merged_df <- indata %>%
  bind_rows(., tcs_data %>% select(any_of(names(indata)))) %>%
  mutate(serial_no = seq_along(id)) %>%
  arrange(id, desc(IndicatorID)) #%>%
  # filter(id == "710 - South Africa - VR - Deaths - 2007 - Register - Demographic Yearbook - Year of occurrence - Direct - Low")

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
              distinct() %>%
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

return(out)
}

indata <- out_all
tcs_data <- dd_extract_188
ind <- 188
type = "deaths"

hm <- dd_append_tcs_cas(indata,type, tcs_data,ind)


