#' Insert a Sample Information Section
#'
#' Insert a sample information section in RMarkdown reports. Use this function within
#' a code chunk and make sure to set the chunk option \code{results = 'asis'}.
#'
#' @param x \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.sample}}.
#' Consider using \code{\link{.sample}} as a constructor.
#'
#' @param delim \code{\link{character}} \bold{(required)}: Keyword specifying
#' the delimiter insterted before the section. Usually \code{"<br>"} or \code{"<hr>"}
#' for HTML and \code{"\\\\newpage"} for PDF reports.
#'
#' @return
#'
#' Text output via \code{\link{cat}}.
#'
#' @examples
#'
#' report_Sample(x = list("K9999", "Coral", "Holocene", 20, "29/02/2016", 1:20),
#'               delim = "\\newpage")
#'
#' report_Sample(.sample(sample = "K9999",
#'                       type = "Coral",
#'                       exp_age = "Holocene",
#'                       n_aliquots = 20,
#'                       measurement_dates = "29/02/2016",
#'                       irr_dose = 1:20),
#'                delim = "\\newpage")
#'
#' @rdname report_Sample
#' @export
report_Sample <- function(x = .sample(), delim) {

  .section(1, "Sample information", delim = delim)

  entries <- c("Sample", "Type", "Expected age range",
               "Number of aliquots", "Date of measurement(s)",
               "Irradiation doses (Gy)")

  for (i in 1:length(entries))
    cat(paste0("**", entries[i], "**:"), paste(unique(x[[i]]), collapse = ", "), "\n\n")

}

#' @param sample \code{\link{character}}: Sample name or laboratory code.
#'
#' @param type \code{\link{character}}: Sample type (e.g., "Coral" or "Mollusc").
#'
#' @param exp_age \code{\link{character}}: The expected age range.
#'
#' @param n_aliquots \code{\link{character}}: Number of aliquots used to determine
#' the equivalent dose.
#'
#' @param measurement_dates \code{\link{character}}: When were the measurements
#' conducted?
#'
#' @param irr_doses \code{\link{character}}: Irradiation doses (in Gy) of the
#' individual aliquots.
#'
#' @rdname report_Sample
#'
#' @export
.sample <- function(sample = NA, type = NA, exp_age = NA, n_aliquots = NA,
                    measurement_dates = NA, irr_doses = NA) {

  list(sample = sample, type = type, exp_age = exp_age, n_aliquots = n_aliquots,
       measurement_dates = measurement_dates, irr_doses = irr_doses)

}
