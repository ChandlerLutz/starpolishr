
#' Write to a file and compile to pdf
#'
#' \code{star_write_and_pdf_compile} will compile the table as a pdf
#' and then write using \code{star_tex_write} without headers, so that
#' table can be directly imported into a latex file
#'
#' @seealso \code{star_tex_write}
#' @param ... stargazer ouptut
#' @param starlist a list of stargazer output to write to a file
#' @param file where the file will be saved
#' @export
star_write_and_compile_pdf <- function(..., starlist = NULL, file) {

  ##Get all of the output in a list
  starlist <- c(list(...), starlist)

  ## https://stackoverflow.com/a/54082833/1317443
  ## https://stackoverflow.com/a/32259986/1317443
  tex.dir <- gsub("(.*/)[^/]+$", "\\1", file)

  file.no.path <- gsub(".*/([^/]+)$", "\\1", file)
  file.no.path.or.ext <- gsub(".tex", "", file.no.path)

  ## Write with headers so we can compile to pdf 
  star_tex_write(starlist = starlist, file = file, headers = TRUE)

  ##Get the original working directory so we can reset it later 
  orig.wd <- getwd()

  setwd(tex.dir)
  tools::texi2pdf(file, quiet = FALSE)
  ##remove the extra tex file
  files.to.remove <- paste0(file.no.path.or.ext, c(".log", ".aux"))
  file.remove(files.to.remove)
  setwd(orig.wd)

  return()
}

