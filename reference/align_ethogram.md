# Align Ethogram

\`r lifecycle::badge("experimental")\`

## Usage

``` r
align_ethogram(data, by, mode = c("zero", "time", "midnight"))
```

## Arguments

- data:

  A data frame with \`x\` and \`xend\`.

- by:

  Columns to align within.

- mode:

  One of \`"zero"\`, \`"time"\`, \`"midnight"\`.

## Details

Alignment requires complete time columns. If \`x\`/\`xend\` contain
missing or non-finite values, alignment fails with a clear error.

## Examples

``` r
```
