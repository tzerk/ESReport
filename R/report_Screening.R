#' Insert a Screening Section
#'
#' Insert a spectrum screening section in RMarkdown reports. Use this function within
#' a code chunk and make sure to set the chunk option \code{results = 'asis'}.
#'
#' @param files \code{\link{character}} \bold{(required)}: file path or directory where the spectra files are stored.
#'
#' @param meta \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.meta}}.
#' Consider using \code{\link{.meta}} as a constructor.
#'
#' @param settings \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.settings}}.
#' Consider using \code{\link{.settings}} as a constructor.
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
#' \dontrun{
#' report_Screening(files = "~/PATH/TO/FILES/",
#'                  meta = .meta(person = "Max Mustermann",
#'                               date = "Yesterday",
#'                               file_suffix = c("_a", "_b"),
#'                               file_path = "~/ESR/"),
#'                  settings = .settings(device = "Bruker ESP300-E",
#'                                       mod_amp = 0.485,
#'                                       receiver_gain = 2,
#'                                       freq = 9.628,
#'                                       conv_time = 20.48,
#'                                       time_const = 163.84,
#'                                       sweep_time = 20.972,
#'                                       power = 25.3,
#'                                       center_field = 3430,
#'                                       sweep_width = 40,
#'                                       scans = c(10, 20, 30, 40)),
#'                  delim = "\\newpage")
#' }
#'
#' @export
report_Screening <- function(files, meta = NULL, settings = NULL, delim) {

  ## Read files to R
  spectra <- ESR::read_Spectrum(files, verbose = FALSE)

  ## Header
  .section(1, "Spectrum screening", delim = delim)

  # meta information
  report_Meta(meta)

  # Experimental settings
  .header(2, "Experimental settings")
  report_Settings(settings)

  # Screening loop
  for (i in 1:length(spectra)) {

    center_field <- as.numeric(spectra[[i]]$parameter[grepl("HCF", spectra[[i]]$parameter[,1]), 2])
    sweep_width <- as.numeric(spectra[[i]]$parameter[grepl("HSW", spectra[[i]]$parameter[,1]), 2])
    header <- paste0("Nr. ", as.roman(i), ": ", center_field, " +/- ", sweep_width, " G")

    # sub-header
    .section(2, header, delim = delim)

    plot(spectra[[i]], mtext = "")
  }

}
