#' @keywords internal
#' @description `r lifecycle::badge("experimental")`
compute_ethogram <- function(data, scales, align_trials, remove_nas){
  #print("First line of compute_panel()")
  #print(data)
  # Skip if there is nothing to plot for this panel
  #if (nrow(data) == 0) {
  #  return(data)
  #}

  # yend is always y
  data$yend <- data$y

  # If x and xend are provided, these values are passed
  # directly to GeomSegment
  if (all(c("x", "xend") %in% names(data))) {
    if(nrow(data) > 10^5){
      warning("data contains >10^5 rows, might be slow to plot")
    }
    return(data)
  }
  # If x is provided but not xend, behaviours are assumed
  # to represent fixed intervals, which will be guessed
  # from the smallest interval between provided values.
  # In future users will be able to manually override
  # this with an explicit value and thus suppress the
  # below warning.
  if (("x" %in% names(data)) & (! "xend" %in% names(data))) {
    #print("call has x but no xend")

    data <- do.call("rbind", lapply(split(data, data$y), function(s) {

      s <- s[order(s$x), ]
      diffs <- diff(s$x)
      diffs <- diffs[! diffs == 0]
      if (length(diffs) > 0) {
        interval <- min(diffs)
        message("No observation interval provided, using guessed interval ",
                interval
        )
      } else {
        warning("No observation interval provided and unable to guess, some behaviours will not be drawn")
        interval <- 0
      }
      s$xend <- s$x + interval

      # For efficiency of drawing, runs of identical
      # behaviours are collapsed. We will first check to
      # ensure that there are no repeated x values in the
      # data.
      if (! length(s$x) == length(unique(s$x))) {
        warning("Some behaviours will be drawn with the same value for 'x' - is this a mistake?")
      } else if ("behaviour" %in% names(s)) {
        #print("aggregating behavior")
        runs <- rle(s$behaviour)
        s <- do.call("rbind", lapply(1:length(runs$lengths), function(i) {
          r <- sum(runs$lengths[0:(i-1)]) + 1
          d <- s[r, ]
          d$xend <- s[r + runs$lengths[i] - 1, "xend"]
          d
        }))
      }
      s
    }
    ))

    # If no x is provided, behaviours are set to unit width
    # in the order they appear in the data
  }

  if (! "x" %in% names(data)) {
    data <- do.call("rbind", lapply(split(data, data$y), function(s) {

      # For efficiency of drawing, runs of identical
      # behaviours are collapsed
      if ("behaviour" %in% names(s)) {
        runs <- rle(s$behaviour)
        s <- do.call("rbind", lapply(1:length(runs$lengths), function(i) {
          x <- 1 + sum(runs$lengths[0:(i-1)])
          d <- s[x, ]
          d$x <- x
          d$xend <- x + runs$lengths[i]
          d
        }))
      } else {
        s$xend <- seq_along(s$y)
        s$x <- s$xend - 1
      }
      s
    }))
  }

  # Align trials, if so instructed
  if (align_trials) {
    data <- do.call("rbind", lapply(split(data, data$y), function(s) {
      zero <- min(s$x)
      s$x <- s$x - zero
      s$xend <- s$xend - zero
      s
    }))
  }

  # Remove NA values for colour, unless asked not to. This
  # is intentionally done right at the end, as NA values
  # are considered to be observations where no behaviour
  # was observed (rather than missing observations).
  if (remove_nas) {
    #if ("colour" %in% names(data)) {
      data <- data[which(! is.na(data$behaviour )), ]
    #}
  }

  #print(head(data))
  return(data)

}

#' @keywords internal
StatEtho <- ggplot2::ggproto("StatEtho", ggplot2::Stat,
                    compute_panel = function(data, scales, align_trials, remove_nas) {
                      #print(head(data))
                      #print(scales)
                      compute_ethogram(data, scales, align_trials, remove_nas)
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
                          remove_nas = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      align_trials = align_trials,
      remove_nas = remove_nas,
      ...
    )
  )
}
