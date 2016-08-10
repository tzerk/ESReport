#' Insert a Equivalent Dose Calculation Section
#'
#' Insert a section on the calculation of the equivalent dose for a given set
#' of input files. Use this function within a code chunk and make sure to set
#' the chunk option \code{results = 'asis'}.
#'
#' @param files \code{\link{character}} \bold{(required)}:
#' file path or directory where the spectra files are stored.
#'
#' @param natural \code{\link{numeric}} (optional):
#'
#' @param dose \code{\link{character}} or \code{\link{numeric}} \bold{(required)}:
#' Either a file path to a text file containing the irradiation doses or a numeric
#' vector.
#'
#' @param meta \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.meta}}.
#' Consider using \code{\link{.meta}} as a constructor.
#'
#' @param settings \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.settings}}.
#' Consider using \code{\link{.settings}} as a constructor.
#'
#' @param sample.weight \code{\link{character}} or \code{\link{numeric}} (optional):
#' Either a file path to a text file containing the sample weights or a numeric
#' vector.
#'
#' @param interval \code{\link{numeric}} \bold{(required)}:
#' A vector of length two specifying the range of x-values where peaks are searched
#'
#' @param model \code{\link{character}} \bold{(required)}:
#' Currently implemented models: single-saturating exponential ("EXP"), linear ("LIN").
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
#' # not available yet
#'
#' @export
report_DE <- function(files, natural = c(1, 2), dose, meta = NULL, settings = NULL, sample.weight, interval, model = "EXP", delim = "\\newpage") {

  ## Read files to R
  spectra <- ESR::read_Spectrum(files, verbose = FALSE)
  dose <- as.numeric(unlist(read.delim(dose, header = FALSE)))
  sample.weight <- as.numeric(unlist(read.delim(sample.weight, header = FALSE)))

  ## Pre-calculations
  peaks <- data.frame(subsample = 1:length(spectra), min = NA, max = NA, amp = NA)

  for (i in seq_along(spectra)) {
    p <- as.data.frame(spectra[[i]]$get_peaks(interval = interval))
    peaks[i, "min"] <- min(p[,2])
    peaks[i, "max"] <- max(p[,2])
  }
  peaks$amp <- apply(peaks[ ,2:3], MARGIN = 1, dist)
  sample <- data.frame(dose = dose, amp = peaks$amp)
  names <- sapply(spectra, function(x) x$originator)
  sample.Table <- data.frame(aliquot = names, dose = dose, peaks[ ,2:ncol(peaks)],
                             weight = sample.weight, amp.corr = peaks$amp / sample.weight)
  colnames(sample.Table) <- c("Aliquot", "Dose (Gy)", "min Intensity (a.u.)", "max Intensity (a.u.)",
                              "Amplitude (a.u.)", "Sample weight (g)", "Amplitude corrected (a.u.)")

  ## Header
  .section(1, "Equvialent dose estimation", delim = delim)

  # meta information
  report_Meta(meta)

  # Experimental settings
  .header(2, "Experimental settings")
  report_Settings(settings)

  ## NATURAL ----
  .section(2, "Natural signal spectrum", delim = delim)
  cat("**File(s)**: `", paste(sample.Table$Aliquot[natural], collapse = ", "), "`\n\n")
  if (length(natural) > 1)
    cat("**Recycling ratio:**", format(round(peaks$amp[natural[1]] / peaks$amp[natural[2]], 2), nsmall = 2), "\n\n")
  ESR::plot_Spectrum(spectra[natural], cex = 0.8)

  # HIGHEST DOSE ----
  .section(2, "Highest additive dose spectrum", delim = delim)
  cat("**File**: `", paste(sample.Table$Aliquot[nrow(sample.Table)], collapse = ", "), "`\n\n")
  cat("**Dose (Gy)**:", paste(sample.Table$`Dose (Gy)`[nrow(sample.Table)], collapse = ", "), "\n\n")
  ESR::plot_Spectrum(spectra[[length(spectra)]])


  ## ALL SPECTRA ----
  .section(2, "All spectra", delim = delim)
  ESR::plot_Spectrum(spectra, cex = 0.7)

  ## PEAKS USED ----
  .section(2, "Analysed signal", delim = delim)

  plot(spectra[[length(spectra)]]$get_gvalues(), cex = 0.9, mtext = "")

  spec_temp <- spectra[[length(spectra)]]

  if ("MF" %in% spec_temp$parameter[ ,1])
    v <- as.numeric(spec_temp$parameter[which(spec_temp$parameter[,1]=="MF"), 2])
  if ("MWFQ" %in% spec_temp$parameter[,1])
    v <- as.numeric(spec_temp$parameter[which(spec_temp$parameter[,1]=="MWFQ"), 2])

  planck_const <- 6.62606957e-34 # SI: J/s
  bohr_magneton <- 9.27400968e-24 # SI: J/T
  H <- interval
  g <- (planck_const * v * 10^9) / (bohr_magneton * 10^-4 * H)

  gval_peaks <- as.data.frame(ESR::find_Peaks(spec_temp$get_gvalues(), interval = g[2:1]))
  gval_peaks <- rbind(gval_peaks[which.max(gval_peaks$ESR.intensity), ],
                      gval_peaks[which.min(gval_peaks$ESR.intensity), ])

  points(gval_peaks, pch = 16, col = "red")
  text(gval_peaks, labels = format(gval_peaks$magnetic.field, digits = 5), pos = 4)

  ## P2P AMPLITUDES ----
  .section(2, "Signal amplitudes", delim = delim)
  cat(pander::pander(sample.Table))
  # write.csv(x = sample.Table, file = "./BA26_Amplitudes_De#1.csv")

  ## CALC DE ----
  # settings
  fit.weights <- c("prop", "equal")
  main <- c("$D_E$ $(1/I^2)$", "$D_E$ (unweighted)")
  main_sub <- c("Same sample weight", "Corrected by sample weight")
  iter <- ifelse(is.null(sample.weight), 1, 2)

  for (i in 1:length(fit.weights)) {
    .section(2, main[i], delim = delim)

    for (j in seq_len(iter)) {

      if (seq_len(iter)[j] == 2)
        sample_temp <- sample.Table[ ,c(2, 7)]
      else
        sample_temp <- sample.Table[ ,c(2, 5)]

      ## SAME WEIGHT ----
      .header(3, main_sub[j])

      ### DRC (normal) ----
      .header(4, "Dose response curve (DRC)")
      if (model == "EXP") {
        cat("**Model:** Single saturating exponential")
        cat("\n\n$$y = a(1 - e^{ - \\frac{-(x+c)}{b} })$$ ")
      }
      if (model == "LIN") {
        cat("**Model:** Linear regression")
        cat("\n\n$$ y = a + b * x $$")
      }

      fit <- ESR::fit_DRC(sample_temp, model = model, fit.weights = fit.weights[i], algorithm = "LM", plot = FALSE, verbose = FALSE)

      cat("\n\n**Equivalent dose (Gy)**:", fit$output$De, "+/-", fit$output$De.Error)
      if (model == "EXP")
        cat("\n\n**Saturation dose (Gy)**:",  fit$output$d0,"+/-", fit$output$d0.error)
      cat("\n\n**Coefficient of determination**:", round(fit$output$rsquared, 4), "\n\n")

      ESR::plot_DRC(fit, cex = 0.9)

      ### DRC (bootstrap) ----
      .section(4, "Bootstrapped DRC", delim = delim)
      fit <- ESR::fit_DRC(sample_temp, model = model, fit.weights = fit.weights[i], algorithm = "LM", plot = FALSE, verbose = FALSE,
                   bootstrap = TRUE)

      cat("\n\n**Equivalent dose (Gy)**:", fit$output$De, "+/-", fit$output$De.Error)
      if (model == "EXP")
        cat("\n\n**Saturation dose (Gy)**:",  fit$output$d0,"+/-", fit$output$d0.error)
      cat("\n\n")

      ESR::plot_DRC(fit, cex = 0.9)

      ### DE-DEmax plot ----
      .section(4, "$D_E-D_{E,max}$ plot", delim = delim)
      demax_inverse <- ESR::calc_DePlateau(sample_temp, fit.weights = fit.weights[i], output.console = FALSE, cex = 0.9, show.grid = TRUE,
                                           line = 0, mar = c(5, 6, 4, 4) + 0.1)

      ### DE-DEmax numeric ----
      .section(4, "$D_E-D_{E,max}$ numeric results", delim = delim)
      demax_inverse.Table <- setNames(demax_inverse$output, c("#", "DE (Gy)", "DE error (Gy)", "Max. Dose (Gy)"))
      cat(pander::pander(demax_inverse.Table))
      .newPage(delim)
    }
  }


  ## INDIVIDUAL SPECTRA ----
  .section(2, "Individual spectra", delim = delim)
  par(mfrow = c(3, 2))
  for (i in seq_along(spectra)) {
    plot(spectra[[i]], mtext = "")
    p <- as.data.frame(spectra[[i]]$get_peaks(interval = interval))
    p.min <- p[p[,2] == min(p[,2]), ]
    p.max <- p[p[,2] == max(p[,2]), ]
    points(rbind(p.min, p.max), col = "red", pch = 13)
    cat("\n\n")
  }
  par(mfrow = c(1, 1))

}
