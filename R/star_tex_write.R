## c:/Dropbox/Rpackages/starpolish/R/tex_write.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-20

##for functions to write to a tex file


#' Create the start of a tex
#'
#' @param file a \code{string} with the file name
#' @export
star_tex_start <- function(file) {

    ##the lines to write
    temp.lines <- paste0("%% ", file, "\n",
                         "\\documentclass[a4paper,12pt]{article}",
                         "\n",
                         "\\usepackage{caption}\n",
                         "\\usepackage{siunitx}\n",
                         "\\usepackage{rotating}\n",
                         "\\usepackage{natbib}\n",
                         "\\usepackage[flushleft]{threeparttable}",
                         "\\usepackage[a4paper, hmargin=0.5in,vmargin=0.5in]{geometry}",
                         "\\begin{document}",
                         "\n")
    ##write
    write(temp.lines, file, append = FALSE)
    return(invisible(NULL))
}

#' Create the end of a tex document
#'
#' @param file a \code{character} string with the file name
#' @param append append to the end of a tex file; defaults to TRUE
#' @export
star_tex_end <- function(file, append = TRUE) {

    ##the lines to write
    temp.lines <- c("\n\n \\end{document}")

    write(temp.lines, file, append = append)
}


#' Write a stargazer output to a tex file
#'
#' @param ... stargazer ouptut
#' @param starlist a list of stargazer output to write to a file
#' @param file where the file will be saved
#' @param headers (logical) to include the start of a tex afile
#' @param append logical to append at the end of a tex file; defaults
#' to \code{FALSE}
#' @export
star_tex_write <- function(..., starlist = NULL, file, headers = FALSE, append = FALSE) {

    ##Get all of the output in a list
    starlist <- c(list(...), starlist)

    ##remove the information on the stargazer package author
    starlist <- lapply(starlist, function(x) x[!grepl("% Table created by", x)])

    top.header <- paste0("%% ", file, "\n",
                         "\\documentclass[a4paper,12pt]{article}",
                         "\n",
                         "\\usepackage{caption}\n",
                         "\\usepackage{siunitx}\n",
                         "\\usepackage{rotating}\n",
                         "\\usepackage[flushleft]{threeparttable}",
                         "\\usepackage[a4paper, hmargin=0.5in,vmargin=0.5in]{geometry}",
                         "\\begin{document}",
                         "\n")

    bottom.header <- c("\n \\end{document}")

    if (headers == TRUE) write(top.header, file)

    if (!headers & !append) {
        ##Empty the text file
        write("", file)
    }

    ##Write the stargazer output to the file
    for (i in seq_along(starlist)) {

        write(paste0("%Tex File: ", getwd(), "/", file), file, append = TRUE)
        write(starlist[[i]], file, append = TRUE)
        write("\n\n", file, append = TRUE)
    }

    if (headers) {
        write(bottom.header, file, append = TRUE)
    }

    return(invisible(NULL))
}

