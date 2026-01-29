# Compute Ethogram from Samples

\`r lifecycle::badge("experimental")\`

## Usage

``` r
compute_samples(
  data,
  x,
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

- x:

  Column name for sample times.

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

## Details

Samples mode enforces time consistency:

- \`x\` must be numeric or datetime (POSIXct/Date/difftime).

- \`x\` must be complete (no missing or non-finite values).

- \`x\` must be monotonic within each group; time reversals are errors.

- Duplicate \`x\` values emit a warning (zero-length segments).

- \`interval_mode = "explicit"\` requires a finite \`interval \> 0\`.

## Examples

``` r
```
