#' Insert a Comment Section
#'
#' Insert a comment section in RMarkdown reports. Use this function within
#' a code chunk and make sure to set the chunk option \code{results = 'asis'}.
#'
#' @param level \code{\link{integer}} \bold{(required)}: A non-negative integer
#' specifying the section level.
#'
#' @param text \code{\link{character}} (optional): The comment text to be inserted
#' in the section.
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
#' report_Comment(level = 1, text = "My comment", delim = "\\newpage")
#'
#' @export
report_Comment <- function(level = 1, text, delim) {

  cat(paste0("\n\n", delim, "\n\n"),paste(rep("#", level), collapse = ""), "Comment\n\n",
      text,
      "\n\n", sep = "")

}
