## c:/Dropbox/Rpackages/starpolish/R/star_cleaning_functions.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

##Extra functions for cleaning stargazer ouput

#' Insert a row in a stargazer table
#'
#' @param star the \code{stargazer} output
#' @param string the string of text to insert into the table
#' @param insert.after insert \code{string} after \code{inster.after}
#' @return the updated \code{stargazer} output
#' @export
star_insert_row <- function(star, string, insert.after) {
    return(c(star[1:insert.after], string, star[(insert.after + 1):length(star)]))
}


#' A function to replace asterisks with latex compatible values
#'
#' @param star the \code{stargazer} output
#' @return updated stargazer output with latex compaitble
#' asterisks
#' @export
star_asterisks <- function(star) {

    star <- gsub("(\\\\textasteriskcentered\\s?){3}", "$^{***}$", star)
    star <- gsub("(\\\\textasteriskcentered\\s?){2}", "$^{**}$", star)
    star <- gsub("(\\\\textasteriskcentered\\s?){1}", "$^*$", star)

    return(star)
}


#' A function to for scientific unit columns from the latex
#' \code{siunitx} package
#'
#' The latex \code{siunitx} package has to loaded in the preamble.
#' See this answer on tex stackexchange:
#' \url{http://tex.stackexchange.com/a/2747} This function is useful with \code{paste0} when updating column t
#'
#' @param table.format the latex \code{siunitx}. For example \code{3.2}
#'     is three integers and two decimals. \code{table.format} can be a
#'     string or numeric.
#' @param rep.times the number of times to repeat the given column
#' @export
si_col <- function(table.format, rep.times = 1) {
    out <- paste0("S[table-format=", table.format, "]")
    if (rep.times > 1) {
        out <- paste(rep(out, rep.times), collapse = "")
    }
    return(out)
}

