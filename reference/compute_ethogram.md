# Compute Ethogram (Dispatcher)

\`r lifecycle::badge("experimental")\`

## Usage

``` r
compute_ethogram(
  data,
  mode = c("auto", "intervals", "samples", "implied"),
  x,
  xend,
  y,
  behaviour,
  group = NULL,
  colour = NULL,
  interval = NULL,
  interval_mode = c("guess", "explicit"),
  remove_nas = TRUE
)
```

## Arguments

- data:

  A data frame.

- mode:

  One of \`"auto"\`, \`"intervals"\`, \`"samples"\`, \`"implied"\`.

- x:

  Column name for start/sample times.

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

- interval:

  Fixed interval between samples (optional).

- interval_mode:

  Use \`"explicit"\` to require interval or \`"guess"\` to infer.

- remove_nas:

  Remove rows with \`NA\` behaviour values.
