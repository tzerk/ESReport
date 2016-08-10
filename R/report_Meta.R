#' Insert a Meta-information Section
#'
#' Insert meta information in RMarkdown reports. Use this function within
#' a code chunk and make sure to set the chunk option \code{results = 'asis'}.
#'
#' @param x \code{\link{list}} \bold{(required)}: A named \code{list} with
#' information on the following elements given in \code{\link{.meta}}.
#' Consider using \code{\link{.meta}} as a constructor.
#'
#' @return
#'
#' Text output via \code{\link{cat}}.
#'
#' @examples
#'
#' report_Meta(x = list("Max Mustermann", "Yesterday", c("_a", "_b"), "~/ESR/"))
#'
#' report_Meta(.meta(person = "Max Mustermann",
#'                   date = "Yesterday",
#'                   file_suffix = c("_a", "_b"),
#'                   file_path = "~/ESR/"))
#'
#' @rdname report_Meta
#' @export
report_Meta <- function(x = .meta()) {

  entries <- c("Person", "Date", "File suffix", "File path")
  for (i in 1:length(entries))
    cat(paste0("**", entries[i], "**:"), paste(x[[i]], collapse = ", "), "\n\n")

}

#' @param person \code{\link{character}} (optional): Name of the person that
#' conducted the experiment/measurement.
#'
#' @param date \code{\link{character}} (optional): Date of measurement.
#'
#' @param file_suffix \code{\link{character}} (optional): Do the raw measurement
#' files have a common file suffix (pattern)?
#'
#' @param file_path \code{\link{character}} (optional): Path where the raw
#' measurement files are stored on the ESR spectrometer.
#'
#' @rdname report_Meta
#' @export
.meta <- function(person = NA, date = NA, file_suffix = NA, file_path = NA) {
  list(person = person,
       date = date,
       file_suffix = paste0("`", file_suffix, "`"),
       file_path = paste0("`", file_path, "`"))
}
