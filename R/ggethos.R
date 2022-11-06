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


StatEtho <- ggplot2::ggproto("StatEtho", ggplot2::Stat,
                    compute_panel = function(data, scales, align_trials, remove_nas) {
                      #print(head(data))
                      #print(scales)
                      compute_ethogram(data, scales, align_trials, remove_nas)
                      },
                    required_aes = c("y")
)

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
