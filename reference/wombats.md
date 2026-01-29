# Wombats datasets

These datasets contain toy data to experiment with the way \`ggethos\`
expects data to be given

## Usage

``` r
wombats

wombats_duration
```

## Format

Each is a tibble

## Examples

``` r
wombats
#> # A tibble: 823 × 9
#>    wombat trial behaviour trial_frame frame seconds exp_dt             
#>    <chr>  <int> <chr>           <int> <int>   <dbl> <dttm>             
#>  1 gimli      1 NA                  1     1       0 2019-08-28 06:37:44
#>  2 gimli      1 NA                  2     2       5 2019-08-28 06:37:44
#>  3 gimli      1 NA                  3     3      10 2019-08-28 06:37:44
#>  4 gimli      1 NA                  4     4      15 2019-08-28 06:37:44
#>  5 gimli      1 NA                  5     5      20 2019-08-28 06:37:44
#>  6 gimli      1 digging             6     6      25 2019-08-28 06:37:44
#>  7 gimli      1 digging             7     7      30 2019-08-28 06:37:44
#>  8 gimli      1 digging             8     8      35 2019-08-28 06:37:44
#>  9 gimli      1 digging             9     9      40 2019-08-28 06:37:44
#> 10 gimli      1 digging            10    10      45 2019-08-28 06:37:44
#> # ℹ 813 more rows
#> # ℹ 2 more variables: start_dt <dttm>, end_dt <dttm>
wombats_duration
#> # A tibble: 74 × 10
#>    wombat trial behaviour start_seconds end_seconds exp_dt             
#>    <chr>  <int> <chr>             <dbl>       <dbl> <dttm>             
#>  1 gimli      1 digging               5          13 2019-08-28 06:37:44
#>  2 gimli      1 pondering            20          33 2019-08-28 06:37:44
#>  3 gimli      1 digging              33          41 2019-08-28 06:37:44
#>  4 gimli      2 snuffling           215         477 2019-08-28 06:37:44
#>  5 gimli      2 digging             217         500 2019-08-28 06:37:44
#>  6 gimli      2 digging             253         518 2019-08-28 06:37:44
#>  7 gimli      3 pondering           481         669 2019-08-28 06:37:44
#>  8 gimli      3 pondering           497         687 2019-08-28 06:37:44
#>  9 gimli      4 digging             655        1024 2019-08-28 06:37:44
#> 10 gimli      4 pondering           659        1034 2019-08-28 06:37:44
#> # ℹ 64 more rows
#> # ℹ 4 more variables: trial_start <dbl>, trial_end <dbl>, start_dt <dttm>,
#> #   end_dt <dttm>
```
