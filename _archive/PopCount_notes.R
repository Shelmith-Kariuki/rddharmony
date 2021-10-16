## Notes:
## "404 - Kenya - Estimate - 2014 - International Data Base (IDB) - Unknown - Model-based projections/extrapolations - High quality"
## Has both abridged and complete series
## Doesn't have totals

idx <- sample(ids, 1)
xyz <- pop_std_all %>% filter(id == idx)
table(xyz$series, xyz$SexID)
