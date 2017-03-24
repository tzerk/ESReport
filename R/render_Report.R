#' Render R Markdown (Wrapper)
#'
#' Render the input file to the specified out ouput format using pandoc. This is
#' a convenient wrapper for \code{\link[rmarkdown]{render}} to produce HTML and
#' PDF output simultaneously.
#'
#' @param file \code{\link{character}} \bold{(required)}: Input file (R script, Rmd, or plain markdown).
#'
#' @param output_format \code{\link{character}}:
#' R Markdown output format to convert to. Pass "all" to render all formats defined within the file.
#' Pass the name of a format (e.g. "html_document") to render a single format or pass a vector of
#' format names to render multiple formats. Alternatively you can pass an output format object; e.g. html_document().
#' If NULL is passed then the output format is the first one defined in the YAML
#' metadata of the input file (defaulting to HTML if none is specified).
#'
#' @param backup \code{\link{logical}} (with default):
#' By default, any existing report files in the working folder are copied to a new folder
#' named 'archived_reports' before overwriting the old report.
#'
#' @param ... further arguments passed to \code{\link[rmarkdown]{render}}.
#'
#' @return
#'
#' A compiled document is written into the output file, and the path of the output file is returned.
#'
#'
#' @examples
#'
#' \dontrun{
#' render_Report("~/PATH/TO/FILE.rmd")
#' }
#'
#' @export
render_Report <- function(file, output_format = c("pdf_document", "html_document"), backup = TRUE, ...) {


  ## ---- Archive old report first
  if (backup) {
    if ("pdf_document" %in% output_format)
      backup(file = file, type = "pdf")
    if ("html_document" %in% output_format)
      backup(file = file, type = "html")
  }

  ## ---- Write new file
  delim <- c("\\newpage", "<hr>")

  if (!is.null(output_format)) {
    for (i in 1:length(output_format)) {
      rmarkdown::render(input = file, output_format = output_format[i], params = list(delim = delim[i]))
    }
  } else {
    knitr::knit(file)
  }

}

backup <- function(file, type) {

  file_body <- gsub(pattern = "rmd$", replacement = "", x = file, ignore.case = TRUE, perl = TRUE)
  file <- gsub(pattern = "rmd$", replacement = "pdf", x = file, ignore.case = TRUE, perl = TRUE)

  # move file if it already exists
  if (file.exists(file)) {

    # create archive folder if necessary
    if (!dir.exists("archived_reports"))
      dir.create("archived_reports")


    file.copy(from = file, to = "archived_reports/", copy.date = TRUE, overwrite = TRUE)

    file.rename(from = paste0("archived_reports/", file),
                to = paste0("archived_reports/",
                            file_body,
                            format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                            ".", type))
  }


}
