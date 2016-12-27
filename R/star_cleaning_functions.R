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
