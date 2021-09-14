
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/Shelmith-Kariuki/rddharmony/branch/master/graph/badge.svg)](https://codecov.io/gh/Shelmith-Kariuki/rddharmony?branch=master)
<!-- badges: end -->

# rddharmony

The goal of rddharmony is to implement a workflow for births, deaths and
population data extracted from vital registration databases and census.
This workflow includes extracting data from DemoData, harmonizing age
groups, identifying full series, validating totals and by sex,
eventually producing clean and harmonised datasets for each location and
each data type (births, deaths or population).

## Installation

You can install the released version of rddharmony from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rddharmony")
```

*Note: This package is not yet on CRAN*

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Shelmith-Kariuki/rddharmony")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rddharmony)
kenya_df <- DDharmonize_validate_BirthCounts(locid = 404,
                                              c(1950,2020),
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE, 
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/structureddatarecords?dataProcessIds=2,36&startYear=1950&endYear=2020&indicatorIds=159,170&locIds=404&locAreaTypeIds=2&subGroupIds=2 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/datacatalogs?dataProcessTypeIds=2&locIds=404&addDefault=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/locations?addDefault=false&includeDependencies=false&includeFormerCountries=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/dataprocesses?addDefault=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/dataprocesstypes?addDefault=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/datasources?dataProcessTypeIds=2,36&locIds=404&addDefault=false 
#> 
#>  Location ID:  404 
#>  Location Name:  Kenya

dim(kenya_df)
#> [1] 189  23
```
