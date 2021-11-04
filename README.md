
# rddharmony <a href='https://github.com/Shelmith-Kariuki/rddharmony'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Shelmith-Kariuki/rddharmony/workflows/R-CMD-check/badge.svg)](https://github.com/Shelmith-Kariuki/rddharmony/actions)
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
# kenya_df <- DDharmonize_validate_BirthCounts(locid = 404,
#                                               c(1950,2022),
#                                               process = c("census", "vr"),
#                                               return_unique_ref_period = TRUE, 
#                                               DataSourceShortName = NULL,
#                                               DataSourceYear = NULL,
#                                               retainKeys = FALSE,
#                                               server = "https://popdiv.dfs.un.org/DemoData/api/")
```

For a case where the user inserts an invalid location id, an informative
error message is displayed.

``` r
# df <- DDharmonize_validate_BirthCounts(locid = 12345678,
#                                               c(1950,2022),
#                                               process = c("census", "vr"),
#                                               return_unique_ref_period = TRUE, 
#                                               DataSourceShortName = NULL,
#                                               DataSourceYear = NULL,
#                                               retainKeys = FALSE,
#                                               server = "https://popdiv.dfs.un.org/DemoData/api/")
```
