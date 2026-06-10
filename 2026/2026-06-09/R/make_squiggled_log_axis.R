# ============================================================
# Squiggled Log Axis
#
# Experimental ggplot2 helper for drawing a visual warning cue
# on logarithmic axes.
#
# The function does not transform the data. The data scale should
# still be set with scale_y_log10(), scale_y_continuous(trans = "log10"),
# or another valid ggplot2 scale.
# ============================================================

# Create coordinates for a squiggled logarithmic y-axis.
make_squiggled_log_axis <- function(
  ymin,
  ymax,
  x0,
  amplitude,
  base = 10,
  waves = 35,
  n = 4000,
  amplitude_power = 2.2,
  wave_power = 1.2
) {
  if (
    !is.numeric(ymin) ||
      !is.numeric(ymax) ||
      length(ymin) != 1 ||
      length(ymax) != 1
  ) {
    stop("ymin and ymax must be single numeric values.")
  }

  if (ymin <= 0 || ymax <= ymin) {
    stop("For a log axis, ymin must be > 0 and ymax must be greater than ymin.")
  }

  if (base <= 1) {
    stop("base must be greater than 1.")
  }

  if (n < 100) {
    warning("n is small. The squiggle may look angular.")
  }

  # Equal spacing in displayed log coordinates.
  y_log <- seq(log(ymin, base = base), log(ymax, base = base), length.out = n)
  y <- base^y_log

  # Position along the displayed log axis: 0 to 1.
  t_log <- (y_log - min(y_log)) / (max(y_log) - min(y_log))

  # Position along the original y units: 0 to 1.
  # This makes the upper part of the log axis more visually active,
  # because high values represent many more original units.
  t_unit <- (y - ymin) / (ymax - ymin)

  # Amplitude grows upward.
  amp <- amplitude * t_log^amplitude_power

  # Wave phase is unit-weighted.
  # This is the key idea: the visible distortion increases where
  # log compression hides many original y-units.
  phase <- 2 * pi * waves * t_unit^wave_power

  x <- x0 + amp * sin(phase)

  data.frame(
    x = x,
    y = y,
    y_log = y_log,
    t_log = t_log,
    t_unit = t_unit,
    amplitude = amp
  )
}

# Add a squiggled log y-axis spine to a ggplot.
annotation_squiggled_log_y_axis <- function(
  ymin,
  ymax,
  x0,
  amplitude,
  base = 10,
  waves = 35,
  n = 4000,
  amplitude_power = 2.2,
  wave_power = 1.2,
  linewidth = 0.45,
  colour = "black",
  alpha = 1
) {
  axis_df <- make_squiggled_log_axis(
    ymin = ymin,
    ymax = ymax,
    x0 = x0,
    amplitude = amplitude,
    base = base,
    waves = waves,
    n = n,
    amplitude_power = amplitude_power,
    wave_power = wave_power
  )

  ggplot2::geom_path(
    data = axis_df,
    ggplot2::aes(x = x, y = y),
    inherit.aes = FALSE,
    linewidth = linewidth,
    colour = colour,
    alpha = alpha
  )
}

# Approximate line length by log-scale intervals.
# This is a diagnostic only, useful for documenting how the squiggle
# becomes visually longer where the original y-units are compressed.
summarise_squiggle_length_by_interval <- function(
  axis_df,
  breaks = c(1, 10, 100, 1000),
  base = 10
) {
  if (!all(c("x", "y") %in% names(axis_df))) {
    stop("axis_df must contain x and y columns.")
  }

  dx <- diff(axis_df$x)
  dy_log <- diff(log(axis_df$y, base = base))
  segment_length <- sqrt(dx^2 + dy_log^2)

  y_mid <- sqrt(axis_df$y[-1] * axis_df$y[-nrow(axis_df)])

  out <- data.frame(
    y_mid = y_mid,
    segment_length = segment_length
  )

  out$interval <- cut(
    out$y_mid,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE
  )

  aggregate(segment_length ~ interval, data = out, sum)
}
