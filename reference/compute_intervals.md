# Compute Ethogram from Intervals

\`r lifecycle::badge("experimental")\`

## Usage

``` r
compute_intervals(
  data,
  x,
  xend,
  y,
  behaviour = NULL,
  group = NULL,
  colour = NULL,
  remove_nas = TRUE
)
```

## Arguments

- data:

  A data frame.

- x:

  Column name for start times.

- xend:

  Column name for end times.

- y:

  Column name for the y axis.

- behaviour:

  Column name for behaviour labels.

- group:

  Column name for grouping (optional).

- colour:

  Column name for colour (optional).

- remove_nas:

  Remove rows with \`NA\` behaviour values.

## Details

Intervals mode is strict about time columns:

- \`x\` and \`xend\` must be numeric or datetime
  (POSIXct/Date/difftime).

- \`x\` and \`xend\` must be complete (no missing or non-finite values).

- \`xend \< x\` is an error; \`xend == x\` emits a warning.

## Examples

``` r
```
