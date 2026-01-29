# Compute Ethogram from Implied Order

\`r lifecycle::badge("experimental")\`

## Usage

``` r
compute_implied(
  data,
  y,
  behaviour,
  group = NULL,
  colour = NULL,
  remove_nas = TRUE
)
```

## Arguments

- data:

  A data frame.

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
