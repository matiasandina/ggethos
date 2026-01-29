# Geom Segment Wrapper for Ethograms

\`r lifecycle::badge("experimental")\` geom_ethogram() calculates the
necessary \`x\` and \`xend\` (unless provided) to draw ethograms
utilizing \[ggplot2::geom_segment()\]. Calculations are provided by
\`ggethos:::StatEtho()\`, which is kept internal to avoid users having
to call \`geom_segment()\` after \`stat_etho()\`.

## Usage

``` r
geom_ethogram(
  mapping = NULL,
  data = NULL,
  stat = "etho",
  position = "identity",
  ...,
  size = 5,
  arrow = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  align_trials = FALSE,
  remove_nas = TRUE,
  mode = c("auto", "intervals", "samples", "implied"),
  interval = NULL,
  interval_mode = c("guess", "explicit"),
  align_by = NULL,
  align_mode = c("zero")
)
```

## Arguments

- mapping:

  Set of aesthetics created by \`aes()\` or \`aes\_()\`. If specified
  and \`inherit.aes = TRUE\` (the default), it is combined with the
  default mapping at the top level of the plot. You must supply
  \`mapping\` if there is no plot mapping.

- data:

  Data provided for the plot if not provided through previous
  \`ggplot(data, ...)\` layer

- stat:

  The statistical transformation to use on the data for this layer, as a
  string. The default ("etho") will use \`StatEtho\` from \`ggethos\` to
  plot ethograms by computing the bounds to call
  \[ggplot2::geom_segment()\]. Changing this will not generate
  ethograms.

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function.

- ...:

  Other arguments passed on to \[ggplot2::layer()\]. These are often
  aesthetics, used to set an aesthetic to a fixed value, like colour =
  "red" or size = 3. They may also be parameters to the paired
  geom/stat.

- size:

  Line size. Default=5, increase for thicker ethogram plots.

- arrow:

  specification for arrow heads, as created by arrow().

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- na.rm:

  If \`FALSE\`, the default, missing values are removed with a warning.
  If \`TRUE\`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? \`NA\`, the
  default, includes if any aesthetics are mapped. \`FALSE\` never
  includes, and \`TRUE\` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If \`FALSE\`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g. \[ggplot2::borders()\]

- align_trials:

  boolean indicating whether to align all trials to zero (default =
  FALSE)

- remove_nas:

  boolean indicating whether to remove the \`NAs\` in the data or not
  (default = TRUE)

- mode:

  one of \`"auto"\`, \`"intervals"\`, \`"samples"\`, \`"implied"\` to
  control input mode selection.

- interval:

  fixed interval between samples when \`mode = "samples"\` (optional)

- interval_mode:

  use \`"explicit"\` to require an \`interval\`, or \`"guess"\` to infer
  it

- align_by:

  columns used for alignment (optional)

- align_mode:

  alignment mode; currently \`"zero"\` is supported

## See also

\[ggplot2::geom_segment()\]
