#' @title Compute ethogram
#' @keywords internal
#' @description `r lifecycle::badge("experimental")`
#' @importFrom vctrs vec_identify_runs
#'
compute_ethogram_stat <- function (data,
                                   scales,
                                   align_trials,
                                   remove_nas,
                                   mode = c("auto", "intervals", "samples", "implied"),
                                   interval = NULL,
                              interval_mode = c("guess", "explicit"),
                                   align_by = NULL,
                                   align_mode = c("zero")) {
  compute_ethogram_data(data,
                        mode = mode,
                        interval = interval,
                        interval_mode = interval_mode,
                        remove_nas = remove_nas,
                        align_trials = align_trials,
                        align_by = align_by,
                        align_mode = align_mode)
}


#' @keywords internal
StatEtho <- ggplot2::ggproto("StatEtho", ggplot2::Stat,
                    compute_panel = function(data,
                                             scales,
                                             align_trials,
                                             remove_nas,
                                             mode,
                                             interval,
                                             interval_mode,
                                             align_by,
                                             align_mode) {
                      compute_ethogram_stat(data,
                                            scales,
                                            align_trials,
                                            remove_nas,
                                            mode = mode,
                                            interval = interval,
                                            interval_mode = interval_mode,
                                            align_by = align_by,
                                            align_mode = align_mode)
                      },
                    required_aes = c("y")
)

#' @title Geom Segment Wrapper for Ethograms
#' @description `r lifecycle::badge("experimental")` geom_ethogram() calculates the necessary `x` and `xend` (unless provided) to draw ethograms utilizing [ggplot2::geom_segment()]. Calculations are provided by `ggethos:::StatEtho()`, which is kept internal to avoid users having to call `geom_segment()` after `stat_etho()`.
#' @param mapping Set of aesthetics created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data Data provided for the plot if not provided through previous `ggplot(data, ...)` layer
#' @param align_trials boolean indicating whether to align all trials to zero (default = FALSE)
#' @param remove_nas boolean indicating whether to remove the `NAs` in the data or not (default = TRUE)
#' @param mode one of `"auto"`, `"intervals"`, `"samples"`, `"implied"` to control input mode selection.
#' @param interval fixed interval between samples when `mode = "samples"` (optional)
#' @param interval_mode use `"explicit"` to require an `interval`, or `"guess"` to infer it
#' @param align_by columns used for alignment (optional)
#' @param align_mode alignment mode; currently `"zero"` is supported
#' @param stat The statistical transformation to use on the data for this layer, as a string. The default ("etho") will use `StatEtho` from `ggethos` to plot ethograms by computing the bounds to call [ggplot2::geom_segment()]. Changing this will not generate ethograms.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param size Line size. Default=5, increase for thicker ethogram plots.
#' @param arrow specification for arrow heads, as created by arrow().
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? `NA`, the default, includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE` always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. [ggplot2::borders()]
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @seealso [ggplot2::geom_segment()]
#' @export
geom_ethogram <- function(mapping = NULL,
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
                          align_mode = c("zero")) {

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }
  map_attr <- if (!is.null(data)) attr(data, "ethogram_mapping") else NULL
  if (!is.null(map_attr)) {
    if (!identical(stat, "identity")) {
      lifecycle::deprecate_warn(
        when = "0.0.0.9000",
        what = "Using computed ethogram data with stat != \"identity\"",
        details = "This data already contains ethogram segments; set `stat = \"identity\"` to avoid recomputation."
      )
    }
    required <- c("x", "xend", "y", "yend")
    for (key in required) {
      if (is.null(mapping[[key]]) && !is.null(map_attr[[key]])) {
        mapping[[key]] <- rlang::new_quosure(rlang::sym(map_attr[[key]]))
      }
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linewidth = size,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      align_trials = align_trials,
      remove_nas = remove_nas,
      mode = mode,
      interval = interval,
      interval_mode = interval_mode,
      align_by = align_by,
      align_mode = align_mode,
      ...
    )
  )
}
