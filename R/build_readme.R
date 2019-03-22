TEMP_FILE <- '_README.Rmd'

#' Create Tempfile
#'
#' Creates temporary file from \code{readme_rmd} of type \code{type}
#'
#' @param output character with temporary file name
#' @param type character determining file type. Can be 'text' or 'web'
#' for example.
#' @param readme_rmd character with path to valid Rmd file.
#'
create_tempfile <- function(output, type, readme_rmd) {
  if (file.exists(output)) file.remove(output)
  lines <- readLines(readme_rmd)
  for (line in lines) {
    cat(line, file = output, append = TRUE, sep = "\n")
    if (line == "params:") {
      cat(paste0('   type: ', type), file = output, append = TRUE, sep = "\n")
    }
  }
}

#' Build Readme
#'
#' Based
#'
#' @param readme_rmd character with path to valid Rmd file. If null default path of
#' 'README.
#' @param output_md character with path of output README md file. Default: 'README.md'
#' @param output_html character with path of output html website. If NULL
#' not html is geneated.
#' @param show_html
#'
#' @return
#' @export
#'
#' @examples
build_readme <- function(readme_rmd = NULL, output_md = 'README.md',
                         output_html = "docs/index.html", show_html = TRUE) {
  if (is.null(readme_rmd))
    readme_rmd <- "README.Rmd"

  create_tempfile(TEMP_FILE, 'text', readme_rmd)
  rmarkdown::render(TEMP_FILE, output_format = "github_document", output_file = 'README.md')
  file.remove(TEMP_FILE)

  if (!is.null(output_html)) {
    create_tempfile(TEMP_FILE, 'web', readme_rmd)
    rmarkdown::render(TEMP_FILE, output_format = "html_document", output_file = "docs/index.html")
    file.remove(TEMP_FILE)
    file.remove("README.html")
  }
  if (show_html == TRUE)
    browseURL("docs/index.html")
}
