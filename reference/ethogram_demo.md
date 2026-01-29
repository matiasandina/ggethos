# Minimal ethogram demo datasets

Deterministic datasets designed to make ethogram outputs easy to verify.

## Usage

``` r
ethogram_demo

ethogram_demo_duration
```

## Format

Each is a tibble

## Examples

``` r
ethogram_demo
#> # A tibble: 24 × 8
#>    subject trial frame seconds behaviour exp_dt              start_dt           
#>    <chr>   <int> <int>   <dbl> <chr>     <dttm>              <dttm>             
#>  1 alpha       1     1       0 rest      2020-09-13 12:26:40 2020-09-13 12:26:40
#>  2 alpha       1     2       5 rest      2020-09-13 12:26:40 2020-09-13 12:26:45
#>  3 alpha       1     3      10 move      2020-09-13 12:26:40 2020-09-13 12:26:50
#>  4 alpha       1     4      15 move      2020-09-13 12:26:40 2020-09-13 12:26:55
#>  5 alpha       1     5      20 eat       2020-09-13 12:26:40 2020-09-13 12:27:00
#>  6 alpha       1     6      25 eat       2020-09-13 12:26:40 2020-09-13 12:27:05
#>  7 alpha       2     1       0 rest      2020-09-13 13:26:40 2020-09-13 13:26:40
#>  8 alpha       2     2       5 move      2020-09-13 13:26:40 2020-09-13 13:26:45
#>  9 alpha       2     3      10 rest      2020-09-13 13:26:40 2020-09-13 13:26:50
#> 10 alpha       2     4      15 move      2020-09-13 13:26:40 2020-09-13 13:26:55
#> # ℹ 14 more rows
#> # ℹ 1 more variable: end_dt <dttm>
ethogram_demo_duration
#> # A tibble: 14 × 7
#>    subject trial behaviour start_seconds end_seconds start_dt           
#>    <chr>   <int> <chr>             <dbl>       <dbl> <dttm>             
#>  1 alpha       1 rest                  0          10 2020-09-13 12:26:40
#>  2 alpha       1 move                 10          20 2020-09-13 12:26:50
#>  3 alpha       1 eat                  20          30 2020-09-13 12:27:00
#>  4 alpha       2 rest                  0           5 2020-09-13 13:26:40
#>  5 alpha       2 move                  5          10 2020-09-13 13:26:45
#>  6 alpha       2 rest                 10          15 2020-09-13 13:26:50
#>  7 alpha       2 move                 15          20 2020-09-13 13:26:55
#>  8 alpha       2 eat                  20          30 2020-09-13 13:27:00
#>  9 beta        1 move                  0          15 2020-09-13 14:26:40
#> 10 beta        1 rest                 15          25 2020-09-13 14:26:55
#> 11 beta        1 eat                  25          30 2020-09-13 14:27:05
#> 12 beta        2 eat                   0           5 2020-09-13 15:26:40
#> 13 beta        2 rest                  5          15 2020-09-13 15:26:45
#> 14 beta        2 move                 15          30 2020-09-13 15:26:55
#> # ℹ 1 more variable: end_dt <dttm>
```
