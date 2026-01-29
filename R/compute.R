#' @title Guess Axis Interval
#' @description `r lifecycle::badge("experimental")` This function guesses the interval using differences in the x axis provided to geom_ethogram().
#' @keywords internal
guess_interval <- function(diffs) {
  if (length(diffs) > 0) {
    interval <- min(diffs)
    cli::cli_alert_info("No observation interval provided, using guessed interval {interval}.")
  } else {
    cli::cli_warn("No observation interval provided and unable to guess; some behaviours will not be drawn.")
    interval <- 0
  }
  return(interval)
}

is_time_like <- function(x) {
  is.numeric(x) || inherits(x, c("POSIXct", "POSIXt", "Date", "difftime"))
}

has_missing_time <- function(x) {
  if (is.numeric(x) || inherits(x, "difftime")) {
    return(any(!is.finite(x)))
  }
  any(is.na(x))
}

validate_time_column <- function(data, name, context) {
  if (!(name %in% names(data))) {
    cli::cli_abort("{context} requires column `{name}` in `data`.")
  }
  x <- data[[name]]
  if (!is_time_like(x)) {
    cli::cli_abort("{context} requires `{name}` to be numeric or datetime (POSIXct/Date/difftime).")
  }
  if (has_missing_time(x)) {
    cli::cli_abort("{context} requires complete `{name}` values; missing or non-finite values were found.")
  }
}

validate_interval_value <- function(interval) {
  if (!is.numeric(interval) || length(interval) != 1 || is.na(interval) || !is.finite(interval)) {
    cli::cli_abort("`interval` must be a single finite numeric value when interval_mode = \"explicit\".")
  }
  if (interval <= 0) {
    cli::cli_abort("`interval` must be > 0 when interval_mode = \"explicit\".")
  }
}

validate_monotonic <- function(data, x_name, group_syms) {
  by_names <- vapply(group_syms, rlang::as_name, character(1))
  bad <- data %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::summarise(
      .non_monotonic = any(diff(.data[[x_name]]) < 0),
      .has_duplicates = any(diff(.data[[x_name]]) == 0),
      .groups = "drop"
    )
  if (any(bad$.non_monotonic, na.rm = TRUE)) {
    cli::cli_abort("`{x_name}` must be monotonic within each group; found time reversal in at least one group.")
  }
  if (any(bad$.has_duplicates, na.rm = TRUE)) {
    cli::cli_warn("`{x_name}` has duplicate values within at least one group; this can create zero-length segments.")
  }
}

resolve_sym <- function(data, quo, default_name, required = TRUE) {
  if (rlang::quo_is_missing(quo) || rlang::quo_is_null(quo)) {
    if (default_name %in% names(data)) {
      return(rlang::sym(default_name))
    }
    if (required) {
      cli::cli_abort("`{.arg {default_name}}` is required.")
    }
    return(NULL)
  }
  name <- rlang::as_name(rlang::ensym(quo))
  if (!(name %in% names(data))) {
    cli::cli_abort("`{.arg {name}}` is not a column in `data`.")
  }
  return(rlang::sym(name))
}

infer_colour_sym <- function(data, colour_sym) {
  if (!is.null(colour_sym)) {
    return(colour_sym)
  }
  if ("colour" %in% names(data)) {
    return(rlang::sym("colour"))
  }
  if ("color" %in% names(data)) {
    return(rlang::sym("color"))
  }
  return(NULL)
}

mapping_from_syms <- function(x_sym, xend_sym, y_sym, yend_sym, behaviour_sym, group_sym, colour_sym) {
  mapping <- list(
    x = if (!is.null(x_sym)) rlang::as_name(x_sym) else NULL,
    xend = if (!is.null(xend_sym)) rlang::as_name(xend_sym) else NULL,
    y = if (!is.null(y_sym)) rlang::as_name(y_sym) else NULL,
    yend = if (!is.null(yend_sym)) rlang::as_name(yend_sym) else NULL
  )
  if (!is.null(behaviour_sym)) {
    mapping$behaviour <- rlang::as_name(behaviour_sym)
  }
  if (!is.null(group_sym)) {
    mapping$group <- rlang::as_name(group_sym)
  }
  if (!is.null(colour_sym)) {
    mapping$colour <- rlang::as_name(colour_sym)
  }
  mapping[!vapply(mapping, is.null, logical(1))]
}

attach_ethogram_mapping <- function(data, mapping) {
  attr(data, "ethogram_mapping") <- mapping
  class(data) <- unique(c("ethogram_tbl", class(data)))
  data
}

group_syms_from_data <- function(data, group_sym, y_sym) {
  group_syms <- list()
  if ("PANEL" %in% names(data)) {
    group_syms <- c(group_syms, list(rlang::sym("PANEL")))
  }
  if (!is.null(group_sym)) {
    group_syms <- c(group_syms, list(group_sym))
  } else {
    group_syms <- c(group_syms, list(y_sym))
  }
  group_syms
}

remove_na_behaviour <- function(data, behaviour_name, remove_nas) {
  if (remove_nas && !is.null(behaviour_name) && behaviour_name %in% names(data)) {
    data <- dplyr::filter(data, !is.na(.data[[behaviour_name]]))
  }
  dplyr::ungroup(data)
}

compute_intervals_data <- function(data,
                                   x_sym,
                                   xend_sym,
                                   y_sym,
                                   yend_sym,
                                   behaviour_sym,
                                   group_sym,
                                   colour_sym,
                                   remove_nas) {
  y_name <- rlang::as_name(y_sym)
  yend_name <- rlang::as_name(yend_sym)
  if (!(yend_name %in% names(data))) {
    data[[yend_name]] <- data[[y_name]]
  }

  behaviour_name <- if (!is.null(behaviour_sym)) rlang::as_name(behaviour_sym) else NULL
  data <- remove_na_behaviour(data, behaviour_name, remove_nas)

  mapping <- mapping_from_syms(x_sym, xend_sym, y_sym, yend_sym,
                               behaviour_sym, group_sym, colour_sym)
  attach_ethogram_mapping(data, mapping)
}

compute_samples_data <- function(data,
                                 x_sym,
                                 y_sym,
                                 behaviour_sym,
                                 group_sym,
                                 colour_sym,
                                 interval,
                                 interval_mode,
                                 remove_nas,
                                 xend_name,
                                 yend_name) {
  if (is.null(behaviour_sym)) {
    cli::cli_abort("`{.arg behaviour}` is required for samples mode.")
  }

  x_name <- rlang::as_name(x_sym)
  validate_time_column(data, x_name, "Samples mode")

  if (is.null(interval)) {
    if (interval_mode == "explicit") {
      cli::cli_abort("`{.arg interval}` must be provided when interval_mode = \"explicit\".")
    }
    interval <- guess_interval(abs(diff(data[[x_name]])))
  } else if (interval_mode == "explicit") {
    validate_interval_value(interval)
  }

  group_syms <- group_syms_from_data(data, group_sym, y_sym)
  behaviour_name <- rlang::as_name(behaviour_sym)
  y_name <- rlang::as_name(y_sym)
  colour_name <- if (!is.null(colour_sym)) rlang::as_name(colour_sym) else NULL

  validate_monotonic(data, x_name, group_syms)

  summarise_cols <- rlang::list2(
    !!rlang::sym(behaviour_name) := rlang::expr(dplyr::first(!!behaviour_sym)),
    # Ensure xend uses the original x values (avoid x shadowing in summarise)
    !!rlang::sym(xend_name) := rlang::expr(dplyr::last(!!x_sym) + interval),
    !!rlang::sym(x_name) := rlang::expr(dplyr::first(!!x_sym)),
    !!rlang::sym(y_name) := rlang::expr(dplyr::first(!!y_sym)),
    !!rlang::sym(yend_name) := rlang::expr(dplyr::first(!!y_sym))
  )
  if (!is.null(colour_sym)) {
    summarise_cols[[colour_name]] <- rlang::expr(dplyr::first(!!colour_sym))
  }

  data <- data %>%
    dplyr::mutate(.run_id = vctrs::vec_identify_runs(!!behaviour_sym)) %>%
    dplyr::group_by(!!!group_syms, .run_id) %>%
    dplyr::summarise(!!!summarise_cols, .groups = "keep")

  data <- remove_na_behaviour(data, behaviour_name, remove_nas)
  return(data)
}

compute_implied_data <- function(data,
                                 y_sym,
                                 behaviour_sym,
                                 group_sym,
                                 colour_sym,
                                 remove_nas,
                                 x_name,
                                 xend_name,
                                 yend_name) {
  if (is.null(behaviour_sym)) {
    cli::cli_abort("`{.arg behaviour}` is required for implied mode.")
  }

  group_syms <- group_syms_from_data(data, group_sym, y_sym)
  behaviour_name <- rlang::as_name(behaviour_sym)
  y_name <- rlang::as_name(y_sym)
  colour_name <- if (!is.null(colour_sym)) rlang::as_name(colour_sym) else NULL

  summarise_cols <- rlang::list2(
    !!rlang::sym(behaviour_name) := rlang::expr(dplyr::first(!!behaviour_sym)),
    !!rlang::sym(x_name) := rlang::expr(dplyr::first(.sample)),
    !!rlang::sym(xend_name) := rlang::expr(dplyr::last(.sample) + 1),
    !!rlang::sym(y_name) := rlang::expr(dplyr::first(!!y_sym)),
    !!rlang::sym(yend_name) := rlang::expr(dplyr::first(!!y_sym))
  )
  if (!is.null(colour_sym)) {
    summarise_cols[[colour_name]] <- rlang::expr(dplyr::first(!!colour_sym))
  }

  data <- data %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::mutate(.sample = dplyr::row_number()) %>%
    dplyr::mutate(.run_id = vctrs::vec_identify_runs(!!behaviour_sym)) %>%
    dplyr::group_by(!!!group_syms, .run_id) %>%
    dplyr::summarise(!!!summarise_cols, .groups = "keep")

  data <- remove_na_behaviour(data, behaviour_name, remove_nas)
  return(data)
}

compute_ethogram_data <- function(data,
                                  mode = c("auto", "intervals", "samples", "implied"),
                                  interval = NULL,
                                  interval_mode = c("guess", "explicit"),
                                  remove_nas = TRUE,
                                  align_trials = FALSE,
                                  align_by = NULL,
                                  align_mode = c("zero")) {
  mode <- match.arg(mode)
  interval_mode <- match.arg(interval_mode)
  align_mode <- match.arg(align_mode)

  if (mode == "auto") {
    has_x <- "x" %in% names(data)
    has_xend <- "xend" %in% names(data)
    if (isTRUE(has_x) && isTRUE(has_xend)) {
      mode <- "intervals"
    } else if (isTRUE(has_x)) {
      mode <- "samples"
    } else {
      mode <- "implied"
    }
  }

  x_sym <- if (mode != "implied") rlang::sym("x") else NULL
  xend_sym <- if (mode == "intervals") rlang::sym("xend") else NULL
  y_sym <- rlang::sym("y")
  behaviour_sym <- if (mode != "intervals") rlang::sym("behaviour") else NULL
  group_sym <- if ("group" %in% names(data)) rlang::sym("group") else NULL
  colour_sym <- infer_colour_sym(data, NULL)

  if (mode == "intervals") {
    validate_time_column(data, "x", "Intervals mode")
    validate_time_column(data, "xend", "Intervals mode")
    if (any(data[["xend"]] < data[["x"]], na.rm = TRUE)) {
      cli::cli_abort("Intervals mode requires `xend >= x`; found negative durations.")
    }
    if (any(data[["xend"]] == data[["x"]], na.rm = TRUE)) {
      cli::cli_warn("Intervals mode contains `xend == x`; zero-length segments will be drawn.")
    }
    data <- data %>%
      dplyr::mutate(yend = y)
    data <- remove_na_behaviour(data, "behaviour", remove_nas)
  } else if (mode == "samples") {
    data <- compute_samples_data(data,
                                 x_sym = x_sym,
                                 y_sym = y_sym,
                                 behaviour_sym = behaviour_sym,
                                 group_sym = group_sym,
                                 colour_sym = colour_sym,
                                 interval = interval,
                                 interval_mode = interval_mode,
                                 remove_nas = remove_nas,
                                 xend_name = "xend",
                                 yend_name = "yend")
  } else {
    data <- compute_implied_data(data,
                                 y_sym = y_sym,
                                 behaviour_sym = behaviour_sym,
                                 group_sym = group_sym,
                                 colour_sym = colour_sym,
                                 remove_nas = remove_nas,
                                 x_name = "x",
                                 xend_name = "xend",
                                 yend_name = "yend")
  }

  if (is.null(align_by) && isTRUE(align_trials)) {
    align_by <- "y"
  }
  if (!is.null(align_by)) {
    align_by <- as.character(align_by)
    data <- align_ethogram(data, by = dplyr::all_of(align_by), mode = align_mode)
  }

  return(data)
}

#' @title Compute Ethogram from Intervals
#' @description `r lifecycle::badge("experimental")`
#' @details
#' Intervals mode is strict about time columns:
#' \itemize{
#'   \item `x` and `xend` must be numeric or datetime (POSIXct/Date/difftime).
#'   \item `x` and `xend` must be complete (no missing or non-finite values).
#'   \item `xend < x` is an error; `xend == x` emits a warning.
#' }
#' @examples
#' if (FALSE) {
#'   df <- data.frame(x = c(1, 2), xend = c(2, 1), y = 1, behaviour = c("a", "b"))
#'   compute_intervals(df, x, xend, y, behaviour) # error: xend < x
#' }
#' @param data A data frame.
#' @param x Column name for start times.
#' @param xend Column name for end times.
#' @param y Column name for the y axis.
#' @param behaviour Column name for behaviour labels.
#' @param group Column name for grouping (optional).
#' @param colour Column name for colour (optional).
#' @param remove_nas Remove rows with `NA` behaviour values.
#' @export
compute_intervals <- function(data,
                              x,
                              xend,
                              y,
                              behaviour = NULL,
                              group = NULL,
                              colour = NULL,
                              remove_nas = TRUE) {
  x_sym <- resolve_sym(data, rlang::enquo(x), "x", required = TRUE)
  xend_sym <- resolve_sym(data, rlang::enquo(xend), "xend", required = TRUE)
  y_sym <- resolve_sym(data, rlang::enquo(y), "y", required = TRUE)
  behaviour_sym <- resolve_sym(data, rlang::enquo(behaviour), "behaviour", required = FALSE)
  group_sym <- resolve_sym(data, rlang::enquo(group), "group", required = FALSE)
  colour_sym <- infer_colour_sym(data, resolve_sym(data, rlang::enquo(colour), "colour", required = FALSE))

  validate_time_column(data, rlang::as_name(x_sym), "Intervals mode")
  validate_time_column(data, rlang::as_name(xend_sym), "Intervals mode")
  if (any(data[[rlang::as_name(xend_sym)]] < data[[rlang::as_name(x_sym)]], na.rm = TRUE)) {
    cli::cli_abort("Intervals mode requires `xend >= x`; found negative durations.")
  }
  if (any(data[[rlang::as_name(xend_sym)]] == data[[rlang::as_name(x_sym)]], na.rm = TRUE)) {
    cli::cli_warn("Intervals mode contains `xend == x`; zero-length segments will be drawn.")
  }

  y_name <- rlang::as_name(y_sym)
  yend_sym <- rlang::sym(paste0(y_name, "_end"))

  compute_intervals_data(data,
                         x_sym = x_sym,
                         xend_sym = xend_sym,
                         y_sym = y_sym,
                         yend_sym = yend_sym,
                         behaviour_sym = behaviour_sym,
                         group_sym = group_sym,
                         colour_sym = colour_sym,
                         remove_nas = remove_nas)
}

#' @title Compute Ethogram from Samples
#' @description `r lifecycle::badge("experimental")`
#' @details
#' Samples mode enforces time consistency:
#' \itemize{
#'   \item `x` must be numeric or datetime (POSIXct/Date/difftime).
#'   \item `x` must be complete (no missing or non-finite values).
#'   \item `x` must be monotonic within each group; time reversals are errors.
#'   \item Duplicate `x` values emit a warning (zero-length segments).
#'   \item `interval_mode = "explicit"` requires a finite `interval > 0`.
#' }
#' @examples
#' if (FALSE) {
#'   df <- data.frame(x = c(1, NA, 3), y = 1, behaviour = c("a", "a", "a"))
#'   compute_samples(df, x, y, behaviour) # error: missing x
#' }
#' @param data A data frame.
#' @param x Column name for sample times.
#' @param y Column name for the y axis.
#' @param behaviour Column name for behaviour labels.
#' @param group Column name for grouping (optional).
#' @param colour Column name for colour (optional).
#' @param interval Fixed interval between samples (optional).
#' @param interval_mode Use `"explicit"` to require interval or `"guess"` to infer.
#' @param remove_nas Remove rows with `NA` behaviour values.
#' @export
compute_samples <- function(data,
                            x,
                            y,
                            behaviour,
                            group = NULL,
                            colour = NULL,
                            interval = NULL,
                            interval_mode = c("guess", "explicit"),
                            remove_nas = TRUE) {
  interval_mode <- match.arg(interval_mode)
  x_sym <- resolve_sym(data, rlang::enquo(x), "x", required = TRUE)
  y_sym <- resolve_sym(data, rlang::enquo(y), "y", required = TRUE)
  behaviour_sym <- resolve_sym(data, rlang::enquo(behaviour), "behaviour", required = TRUE)
  group_sym <- resolve_sym(data, rlang::enquo(group), "group", required = FALSE)
  colour_sym <- infer_colour_sym(data, resolve_sym(data, rlang::enquo(colour), "colour", required = FALSE))

  x_name <- rlang::as_name(x_sym)
  y_name <- rlang::as_name(y_sym)
  xend_name <- paste0(x_name, "_end")
  yend_name <- paste0(y_name, "_end")

  data <- compute_samples_data(data,
                               x_sym = x_sym,
                               y_sym = y_sym,
                               behaviour_sym = behaviour_sym,
                               group_sym = group_sym,
                               colour_sym = colour_sym,
                               interval = interval,
                               interval_mode = interval_mode,
                               remove_nas = remove_nas,
                               xend_name = xend_name,
                               yend_name = yend_name)

  mapping <- mapping_from_syms(x_sym, rlang::sym(xend_name), y_sym, rlang::sym(yend_name),
                               behaviour_sym, group_sym, colour_sym)
  attach_ethogram_mapping(data, mapping)
}

#' @title Compute Ethogram from Implied Order
#' @description `r lifecycle::badge("experimental")`
#' @param data A data frame.
#' @param y Column name for the y axis.
#' @param behaviour Column name for behaviour labels.
#' @param group Column name for grouping (optional).
#' @param colour Column name for colour (optional).
#' @param remove_nas Remove rows with `NA` behaviour values.
#' @export
compute_implied <- function(data,
                            y,
                            behaviour,
                            group = NULL,
                            colour = NULL,
                            remove_nas = TRUE) {
  y_sym <- resolve_sym(data, rlang::enquo(y), "y", required = TRUE)
  behaviour_sym <- resolve_sym(data, rlang::enquo(behaviour), "behaviour", required = TRUE)
  group_sym <- resolve_sym(data, rlang::enquo(group), "group", required = FALSE)
  colour_sym <- infer_colour_sym(data, resolve_sym(data, rlang::enquo(colour), "colour", required = FALSE))

  y_name <- rlang::as_name(y_sym)
  x_name <- "sample"
  xend_name <- "sample_end"
  yend_name <- paste0(y_name, "_end")

  data <- compute_implied_data(data,
                               y_sym = y_sym,
                               behaviour_sym = behaviour_sym,
                               group_sym = group_sym,
                               colour_sym = colour_sym,
                               remove_nas = remove_nas,
                               x_name = x_name,
                               xend_name = xend_name,
                               yend_name = yend_name)

  mapping <- mapping_from_syms(rlang::sym(x_name), rlang::sym(xend_name), y_sym, rlang::sym(yend_name),
                               behaviour_sym, group_sym, colour_sym)
  attach_ethogram_mapping(data, mapping)
}

#' @title Compute Ethogram (Dispatcher)
#' @description `r lifecycle::badge("experimental")`
#' @param data A data frame.
#' @param mode One of `"auto"`, `"intervals"`, `"samples"`, `"implied"`.
#' @param x Column name for start/sample times.
#' @param xend Column name for end times.
#' @param y Column name for the y axis.
#' @param behaviour Column name for behaviour labels.
#' @param group Column name for grouping (optional).
#' @param colour Column name for colour (optional).
#' @param interval Fixed interval between samples (optional).
#' @param interval_mode Use `"explicit"` to require interval or `"guess"` to infer.
#' @param remove_nas Remove rows with `NA` behaviour values.
#' @export
compute_ethogram <- function(data,
                             mode = c("auto", "intervals", "samples", "implied"),
                             x,
                             xend,
                             y,
                             behaviour,
                             group = NULL,
                             colour = NULL,
                             interval = NULL,
                             interval_mode = c("guess", "explicit"),
                             remove_nas = TRUE) {
  mode <- match.arg(mode)

  if (mode == "auto") {
    has_x <- !rlang::quo_is_missing(rlang::enquo(x)) || ("x" %in% names(data))
    has_xend <- !rlang::quo_is_missing(rlang::enquo(xend)) || ("xend" %in% names(data))
    if (has_x && has_xend) {
      mode <- "intervals"
    } else if (has_x) {
      mode <- "samples"
    } else {
      mode <- "implied"
    }
  }

  if (mode == "intervals") {
    return(compute_intervals(data,
                             x = {{ x }},
                             xend = {{ xend }},
                             y = {{ y }},
                             behaviour = {{ behaviour }},
                             group = {{ group }},
                             colour = {{ colour }},
                             remove_nas = remove_nas))
  }
  if (mode == "samples") {
    return(compute_samples(data,
                           x = {{ x }},
                           y = {{ y }},
                           behaviour = {{ behaviour }},
                           group = {{ group }},
                           colour = {{ colour }},
                           interval = interval,
                           interval_mode = interval_mode,
                           remove_nas = remove_nas))
  }
  compute_implied(data,
                  y = {{ y }},
                  behaviour = {{ behaviour }},
                  group = {{ group }},
                  colour = {{ colour }},
                  remove_nas = remove_nas)
}

#' @title Align Ethogram
#' @description `r lifecycle::badge("experimental")`
#' @details
#' Alignment requires complete time columns. If `x`/`xend` contain missing or
#' non-finite values, alignment fails with a clear error.
#' @examples
#' if (FALSE) {
#'   df <- data.frame(x = c(NA, 1), xend = c(NA, 2), y = 1)
#'   align_ethogram(df, by = y) # error: missing x
#' }
#' @param data A data frame with `x` and `xend`.
#' @param by Columns to align within.
#' @param mode One of `"zero"`, `"time"`, `"midnight"`.
#' @export
align_ethogram <- function(data,
                           by,
                           mode = c("zero", "time", "midnight")) {
  mode <- match.arg(mode)
  if (missing(by)) {
    cli::cli_abort("`{.arg by}` must be provided for alignment.")
  }
  by_vars <- tidyselect::eval_select(rlang::enquo(by), data)
  if (length(by_vars) == 0) {
    cli::cli_abort("`{.arg by}` must select at least one column.")
  }
  if (mode != "zero") {
    cli::cli_abort("Only mode = \"zero\" is currently supported.")
  }

  by_names <- names(by_vars)
  mapping <- attr(data, "ethogram_mapping")
  data_class <- setdiff(class(data), "grouped_df")

  if (!is.null(mapping)) {
    x_name <- mapping$x
    xend_name <- mapping$xend
  } else {
    x_name <- "x"
    xend_name <- "xend"
  }
  validate_time_column(data, x_name, "Alignment")
  validate_time_column(data, xend_name, "Alignment")
  assertthat::assert_that(all(c(x_name, xend_name) %in% names(data)),
                          msg = "`x` and `xend` are required for alignment.")
  x_sym <- rlang::sym(x_name)
  xend_sym <- rlang::sym(xend_name)

  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_names))) %>%
    dplyr::mutate(.align_min = min(.data[[x_name]]),
                  !!x_sym := .data[[x_name]] - .align_min,
                  !!xend_sym := .data[[xend_name]] - .align_min) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.align_min)

  attr(data, "ethogram_mapping") <- mapping
  class(data) <- data_class

  return(data)
}
