vitals5_df <- df_abr_std
use_data(vitals5_df)

df <- vital5_df
oag_start <- dd_oag_agestart(df, multiple5 = TRUE)

df <- vitals5_df
dd_series_isfull(df, abridged = TRUE)

df <- vitals5_df
df <- dd_firstages_compute_births(df)

vitals5_wags<- abr
use_data(vitals5_wags)

df <- vitals5_wags
df <- dd_firstages_compute_births(df)

vitals5_wags_rec<- df
use_data(vitals5_wags_rec)

df <- vitals5_wags_rec %>% select(-AgeSort)
df <- dd_age_standard(df)


use_data(vitals_abr_cpl1)
use_data(vitals_abr_cpl2)

df_abr <- dd_fillzeros_births(data = vitals_abr_cpl1 %>%
                      select(-AgeSort), abridged = TRUE)

df_cpl <- dd_fillzeros_births(data = vitals_abr_cpl2 %>%
                                select(-AgeSort), abridged = FALSE)


df <- vitals_abr_cpl %>% select(-AgeSort)
df <- dd_age_standard(df)

vitals_std_full_sample <- vitals_std_full %>%
   dplyr::filter(id == "752 - Sweden - VR - Births - 2015 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair")
use_data(vitals_std_full_sample)

df <- vitals_std_full_sample
df <- dd_validate_totals_over_age(df)

vitals_std_valid_sample <- dd_one_id
use_data(vitals_std_valid_sample, overwrite = TRUE)

df <- vitals_std_valid_sample
df <- dd_validate_totals_over_age(df)


vitals_std_valid_sample2 <- vitals_std_valid %>%
                               filter(TimeLabel == "2015")
use_data(vitals_std_valid_sample2)

df <- vitals_std_valid_sample2
df <- dd_rank_id_vitals(df)

vitals5_wags_rec2 <- vitals5_wags_rec %>% select(-AgeSort)
use_data(vitals5_wags_rec2)

vitals_abr_cpl1_f0 <- vitals_abr_cpl1 %>% select(-AgeSort)
vitals_abr_cpl2_f0 <- vitals_abr_cpl2 %>% select(-AgeSort)
use_data(vitals_abr_cpl1_f0, overwrite = TRUE)
use_data(vitals_abr_cpl2_f0, overwrite = TRUE)
