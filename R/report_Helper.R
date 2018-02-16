#' Helper functions for markdown sectioning
#'
#' A collection of helper functions for easier sectioning of markdown documents.
#' They take care that e.g. control commands for new lines (\code{"\\newline"} or \code{"<br>"})
#' are correctly positioned by adding \code{"\\n"} before and after a structure.
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
#' .newPage("\\newpage")
#' .header(1, "My Header")
#' .section(2, "My Section", "<hr>")
#'
#' @name report_Helper
NULL

#' @export
#' @rdname report_Helper
.newPage <- function(delim) cat(delim, "\n\n", sep = "")

#' @export
#' @rdname report_Helper
.header <- function(level, text) cat(paste(rep("#", level), collapse = ""), paste("", text), "\n\n", sep = "")

#' @export
#' @rdname report_Helper
.section <- function(level, text, delim) cat(paste0("\n\n", delim,"\n\n"), paste(rep("#", level), collapse = ""), paste("", text), "\n\n", sep = "")
