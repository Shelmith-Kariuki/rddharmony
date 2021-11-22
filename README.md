
# rddharmony <a href='https://github.com/Shelmith-Kariuki/rddharmony'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Shelmith-Kariuki/rddharmony/workflows/R-CMD-check/badge.svg)](https://github.com/Shelmith-Kariuki/rddharmony/actions)
[![pkgdown](https://github.com/Shelmith-Kariuki/rddharmony/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Shelmith-Kariuki/rddharmony/actions)
[![Codecov test
coverage](https://codecov.io/gh/Shelmith-Kariuki/rddharmony/branch/master/graph/badge.svg)](https://codecov.io/gh/Shelmith-Kariuki/rddharmony?branch=master)

<!-- badges: end -->

## Overview

The goal of [rddharmony](https://github.com/Shelmith-Kariuki/rddharmony)
is to implement a workflow for births, deaths and population data
downloaded from the UNPD portal. This data is extracted from vital
registration databases and census’ of different countries. This workflow
includes extracting data from from the portal using the
[DDSQLtools](https://github.com/timriffe/DDSQLtools) package,
harmonizing age groups, identifying full series, validating totals and
by sex, eventually producing clean and harmonised datasets for each
location and each data type (births, deaths or population).

rddharmony contains three major functions:

  - `DDharmonize_validate_BirthCounts()`: Extracts, processes and
    produces clean birth counts data

  - `DDharmonize_validate_DeathCounts()`: Extracts, processes and
    produces clean death counts data

  - `DDextract_PopCounts()`: Extracts, processes and produces clean
    population counts data

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

`DDharmonize_validate_BirthCounts()` contains several arguments, the key
one being `locid` which is the location id. Each country has a unique
location id. The list of location ids can be viewed by running
`View(get_locations())`. The location id is indicated in the `PK_LocID`
variable.

The example below extracts Kenyan data, whose `locid == 404`. The output
of the function is a dataset with 25 variables.

``` r
library(rddharmony)
kenya_df <- DDharmonize_validate_BirthCounts(locid = 404,
                                              c(1950,2022),
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/structureddatarecords?dataProcessIds=2,36&startYear=1950&endYear=2022&indicatorIds=159,170&locIds=404&locAreaTypeIds=2&subGroupIds=2 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/datacatalogs?dataProcessTypeIds=2&locIds=404&addDefault=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/locations?addDefault=false&includeDependencies=false&includeFormerCountries=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/dataprocesses?addDefault=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/dataprocesstypes?addDefault=false 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/datasources? 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/datasources?dataProcessTypeIds=2,36&locIds=404&addDefault=false 
#> [1] 1
#> [1] "404 - Kenya - Census - Births - 1999 - Census - MMEIG census data compilation for maternal mortality analysis - De-facto - Recent births - Fair"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 2
#> [1] "404 - Kenya - Census - Births - 2009 - Census - MMEIG census data compilation for maternal mortality analysis - De-facto - Recent births - Fair"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 3
#> [1] "404 - Kenya - Census - Births - 2019 - Census - Demographic Yearbook - De-facto - Recent births - Unknown"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 4
#> [1] "404 - Kenya - VR - Births - 2000 - Register - Demographic Yearbook - Year of occurrence - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 5
#> [1] "404 - Kenya - VR - Births - 2008 - Register - Demographic Yearbook - Year of occurrence - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 6
#> [1] "404 - Kenya - VR - Births - 2009 - Register - Demographic Yearbook - Year of occurrence - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 7
#> [1] "404 - Kenya - VR - Births - 2011 - Register - Demographic Yearbook - Year of occurrence - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 8
#> [1] "404 - Kenya - VR - Births - 2012 - Register - Demographic Yearbook - Year of occurrence - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 9
#> [1] "404 - Kenya - VR - Births - 2016 - Register - Demographic Yearbook - Year of registration - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 10
#> [1] "404 - Kenya - VR - Births - 2017 - Register - Demographic Yearbook - Year of registration - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 11
#> [1] "404 - Kenya - VR - Births - 2018 - Register - Demographic Yearbook - Year of registration - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 12
#> [1] "404 - Kenya - VR - Births - 2019 - Register - Demographic Yearbook - Year of registration - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> [1] 13
#> [1] "404 - Kenya - VR - Births - 2020 - Register - Demographic Yearbook - Year of registration - Direct - Low"
#> [1] "harmonizing vital counts by 5-year age group"
#> 
#>  Location ID:  404 
#>  Location Name:  Kenya
```

For a case where the user inserts an invalid location id, an informative
error message is displayed.

``` r
df <- DDharmonize_validate_BirthCounts(locid = 12345678,
                                              c(1950,2022),
                                              process = c("census", "vr"),
                                              return_unique_ref_period = TRUE,
                                              DataSourceShortName = NULL,
                                              DataSourceYear = NULL,
                                              retainKeys = FALSE,
                                              server = "https://popdiv.dfs.un.org/DemoData/api/")
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/structureddatarecords?dataProcessIds=2,36&startYear=1950&endYear=2022&indicatorIds=159,170&locIds=12345678&locAreaTypeIds=2&subGroupIds=2 
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/locations? 
#> [1] "12345678 is not a valid location id. Please run View(get_locations()) to get a list of plausible location ids. They are listed in the `PK_LocID` variable"
#> 
#>  https://popdiv.dfs.un.org/DemoData/api/locations?
```
