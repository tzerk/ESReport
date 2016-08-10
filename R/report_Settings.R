#' Insert a Experimental Settings Section
#'
#' Insert a section on spectrometer settings in RMarkdown reports. Use this function within
#' a code chunk and make sure to set the chunk option \code{results = 'asis'}.
#'
#' @param x \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.settings}}.
#' Consider using \code{\link{.settings}} as a constructor.
#'
#' @return
#'
#' Text output via \code{\link{cat}}.
#'
#' @examples
#'
#' report_Settings(.settings(device = "Bruker ESP300-E",
#'                           mod_amp = 0.485,
#'                           receiver_gain = 2,
#'                           freq = 9.628,
#'                           conv_time = 20.48,
#'                           time_const = 163.84,
#'                           sweep_time = 20.972,
#'                           power = 25.3,
#'                           center_field = 3430,
#'                           sweep_width = 40,
#'                           scans = c(10, 20, 30, 40)))
#'
#' @rdname report_Settings
#' @export
report_Settings <- function(x = .settings()) {

  entries <- c("Device",
               "Modulation Amplitude",
               "Receiver gain",
               "Microwave Frequency (GHz)",
               "Conversion time (ms)",
               "Time constant (ms)",
               "Sweep time (s)",
               "Microwave power (mW)",
               "Center field (G)",
               "Sweep width (G)",
               "Number of scans",
               "File")

  for (i in 1:length(entries))
    cat(paste0("+ **", entries[i], "**:"), paste(x[[i]], collapse = ", "), "\n\n")

}

#' @param device \code{\link{character}}: Name or model number of the ESR spectrometer.
#'
#' @param mod_amp \code{\link{numeric}}: Modulation Amplitude
#'
#' @param receiver_gain \code{\link{numeric}}: Receiver Gain
#'
#' @param freq \code{\link{numeric}}: Microwave Frequency
#'
#' @param conv_time \code{\link{numeric}}: Conversion Time
#'
#' @param time_const \code{\link{numeric}}: Time Constant
#'
#' @param sweep_time \code{\link{numeric}}: Sweep Time
#'
#' @param power \code{\link{numeric}}: Microwave Power
#'
#' @param center_field \code{\link{integer}}: Center Field
#'
#' @param sweep_width \code{\link{integer}}: Sweep Width
#'
#' @param scans \code{\link{integer}}: Number of scans
#'
#' @param file \code{\link{character}}: File name
#'
#' @rdname report_Settings
#'
#' @export
.settings <- function(device = c("Bruker ESP300-E", "Bruker ELEXYS 500")[1],
                      mod_amp = NA, receiver_gain = NA, freq = NA, conv_time = NA,
                      time_const = NA, sweep_time = NA, power = NA, center_field = NA,
                      sweep_width = NA, scans = NA, file = NA) {

  list(device = device, mod_amp = mod_amp, receiver_gain = receiver_gain, freq = freq,
       conv_time = conv_time, time_const = time_const, sweep_time = sweep_time,
       power = power, center_field = center_field, sweep_width = sweep_width, scans = scans,
       file = file)

}
