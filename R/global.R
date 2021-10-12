utils::globalVariables(c("AgeSpan", "AgeStart",#dd_oag_agestart
                         "DataSourceYear", "AgeLabel", #dd_latest_source_year
                         "complete", "AgeSort", #dd_age_standard
                         "DataValue", "AgeSort", #dd_firstages_compute_births
                         "AgeEnd", #dd_oag2closed #dd_oag_compute
                         "abridged", #dd_oag_compute
                         "SexID", "DataStatusName", "SeriesID", "abr_std", #DDharmonize_Vitals5
                         "note", #DDharmonize_AbridgedAndComplete
                         "PK_LocID","Name", #check_locid
                         "pct_dist", "add_unk", #dd_distribute_unknowns
                         "TimeLabel", "num.id", "StatisticalConceptSort", "DataStatusSort",
                         "DataProcessSort", "DataProcessTypeSort", "DataReliabilitySort",
                         "num.serie", "maxage", "DataSourceName", "has_dyb", "keep_dyb","DataProcessType", #dd_rank_id_vitals
                         "id_series", #print_dropped_ids
                         "dups", #dd_rank_id_vitals
                         "DataProcessID", "DataCatalogID", "LocID", "PK_DataSourceID",
                         "DataSourceTypeName", "DataSourceStatusName", "DataTypeName",
                         "LocName", "DataProcess", "StatisticalConceptName", "DataReliabilityName",
                         "series", "id_sex", "IndicatorName", "five_year", "SexName", "TimeMid",
                         "TimeEnd", "non_standard","IndicatorID", "todrop", "total_159", "total_170",
                         "oag", "tot_without_oag", "tot_reported", #DDharmonize_validate_BirthCounts
                        "n_id", "max_length_id", "majority_id","majority_idname", "max_agestart", #DDharmonize_validate_DeathCounts
                        ".", "present_tcs", "eq", "serial_no", #dd_append_tcs_cas
                        "X1", "X2" #dd_validate_totals_over_sex
                        ))
