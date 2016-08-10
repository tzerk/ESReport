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
render_Report <- function(file, output_format = c("pdf_document", "html_document"), ...) {

  delim <- c("\\newpage", "<hr>")

  if (!is.null(output_format)) {
    for (i in 1:length(output_format)) {
      rmarkdown::render(input = file, output_format = output_format[i], params = list(delim = delim[i]))
    }
  } else {
    knitr::knit(file)
  }

}
