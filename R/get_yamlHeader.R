#' YAML Header for ESR Reports
#'
#' Print YAML Header to be used for producing ESR reports.
#'
#' @return
#'
#' Console output.
#'
#' @examples
#'
#' # Copy the output to your rmarkdown file
#' get_yamlHeader()
#'
#' @export
get_yamlHeader <- function() {
  cat("
---
title: 'Report: ESR sample Ba26'
author: 'Christoph Burow'
date: '`r format(Sys.time())`'
output:
  pdf_document:
    fig_height: 6
    fig_width: 7
    number_sections: yes
    toc: yes
    toc_depth: 6
  html_document:
    highlight: haddock
    number_sections: yes
    theme: flatly
    title: ESR Report
    toc: yes
    toc_depth: 6
    toc_float: yes
params:
delim: '<br>'
---
  ")
}
