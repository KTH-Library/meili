
<!-- README.md is generated from README.Rmd. Please edit that file -->

# meili

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/KTH-Library/meili/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/meili/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[Meilisearch](https://meilisearch.com) is a RESTful search API. It aims
to be a ready-to-go solution for everyone who wants a fast and relevant
search experience.

The goal of the R package `meili` is to provide an R client which is an
API wrapper against the `meilisearch` API, see
<https://www.meilisearch.com/docs/reference/api/overview>.

## Installation

You can install the development version of meili like so:

``` r
remotes::install_github("KTH-Library/meili")
```

## Environment variables

This package expects two environment variables to be set in `.Renviron`:

MEILI_KEY: The key used to authorize API requests, see [the API
docs](https://www.meilisearch.com/docs/learn/security/master_api_keys#communicating-with-a-protected-instance)

MEILI_URL: The public server name where the meilisearch index is
accessible

Settings used can be checked using `meili::meili_config(verbose = TRUE)`

## Example

This is a basic example which shows you how to create a search index,
retrieve documents from it and finally delete it:

``` r
library(meili)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tibble)

# verify settings

my_config <- meili:::meili_config(verbose = TRUE)
#> Found setting for MEILI_KEY environment variable!
#> Please provide MEILI_URL in .Renviron!
#> Reverting to default value for MEILI_URL: https://search.bibliometrics.lib.kth.se

# ingest an in-memory data frame or an on-disk CSV file

co2 <- CO2 |> rownames_to_column(var = "rowid")
task <- meili:::meili_ingest_csv(index = "co2", csvfile = co2)
is_ingested <- meili:::wait_for_task(task$taskUid)

# retrieve three documents from the co2 index
meili:::meili_documents("co2", limit = 3) |> 
  select(-any_of("rowid"))
#> # A tibble: 3 × 5
#>   Plant Type   Treatment  conc  uptake
#>   <chr> <chr>  <chr>      <chr> <chr> 
#> 1 Qn1   Quebec nonchilled 95    16    
#> 2 Qn1   Quebec nonchilled 175   30.4  
#> 3 Qn1   Quebec nonchilled 250   34.8

# run a search for documents matching "quebec 95" w scores > 0.5 (on scale 0..1)
meili:::meili_search("co2", query = "quebec 95") |> 
  filter(`_rankingScore` > 0.5)
#> # A tibble: 6 × 7
#>   rowid Plant Type   Treatment  conc  uptake `_rankingScore`
#>   <chr> <chr> <chr>  <chr>      <chr> <chr>            <dbl>
#> 1 1     Qn1   Quebec nonchilled 95    16               0.852
#> 2 8     Qn2   Quebec nonchilled 95    13.6             0.852
#> 3 15    Qn3   Quebec nonchilled 95    16.2             0.852
#> 4 22    Qc1   Quebec chilled    95    14.2             0.852
#> 5 29    Qc2   Quebec chilled    95    9.3              0.852
#> 6 36    Qc3   Quebec chilled    95    15.1             0.852

# delete the index

task <- meili:::meili_deleteindex("co2")
is_deleted <- meili:::wait_for_task(task$taskUid)
```

## Searching using several queries

To make multiple search queries against the same index:

``` r

meili:::search_name(c("Oscar", "backe"), index = "hrfile") |> 
  select(fullname, unit_name) |> 
  head(6)
#> # A tibble: 6 × 2
#>   fullname              unit_name                
#>   <chr>                 <chr>                    
#> 1 Tjernberg, Oscar      MATERIAL- OCH NANOFYSIK  
#> 2 Hessling, Oscar       ENHETEN PROCESSER        
#> 3 Danielsson, Oscar     EECS SKOLAN              
#> 4 Skirfors, Oscar       MOBILITET OCH PARTNERSKAP
#> 5 Wasström, Oscar       ABE SKOLAN               
#> 6 Rohde Dahlberg, Oscar KTH
```

In order to be able to filter on attributes, first the fields that
should be possible to use as filtered attributes needs to be defined
(this triggers a reindexing in the background):

``` r

# make meili aware of filterable attributes for a specific index
meili:::meili_index_create_filters("hrfile", fields = c("unit_abbr", "title_en"))

# these attributes can now be used in filters, for example:
meili_documents("hrfile", limit = 3e4, 
  fields = c("kthid", "title_en", "unit_abbr"), 
  filter = "unit_abbr = ITM"
) 
```
