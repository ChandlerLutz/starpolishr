## c:/Dropbox/Rpackages/starpolish/R/utils.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

##for some utility functions for the starpolish package:base

#' Check if stargazer output is in latex form
#'
#' @param star the \code{stargazer} output
is.latex <- function(star) grepl("tabular", star) %>% any

#' Check if stargazer output is in text form
#'
#' @param star the \code{stargazer} output
is.text <- function(star) grepl("==", star) %>% any

#' get the number of stargazer columns
#'
#' @param star the \code{stargazer} output
get_num_columns_tex <- function(star) {
    return(max(stringr::str_count(star, "&")) + 1)
}

#' Get \code{stargazer} line numbers by section
#'
#' To get the start line numbers, main line numbers, and end line numbers
#' of \code{stargazer} output
#'
#' @param star the \code{stargazer} output
#' @param reg if the stargazer is a regression
#' @return a list with the line number for the \code{start.lines},
#' \code{main.lines}, and \code{end.lines} of the \code{stargazer}
#' output
get_star_line_numbers <- function(star, reg = TRUE) {

    latex <- is.latex(star)
    text <- is.text(star)

    ##Get the start and end of the table
    if (latex) {

        ##This uses that fact that \hlines in stargazer are symmetric
        hlines <- grep("hline", star)

        if (reg) {
            ##if the stargazer output is a regression
            ##remove if hlines has a element before or after it that is also
            ##an hline as the very start and end of the tables are both hlines.
            ##only keep the middle two elements of hlines
            while(length(hlines) > 2)
                hlines <- hlines[c(-1, -length(hlines))]
        } else {
            ##not a regression, only keep the last two elements of hlines
            hlines <- hlines[(length(hlines) - 1):length(hlines)]
        }


    } else if (text) {
        ##which lines only have dashes and no whitespace?
        ##these are the horizontal lines separating the sections
        hlines <- (grepl("---", star) & !grepl("\\s", star)) %>%
            which
    }


    ##The start, main ouput and the end lines
    start.lines <- 1:hlines[1]
    main.lines <- (hlines[1] + 1):(hlines[2] - 1)
    end.lines <- hlines[2]:length(star)

    return(list(start.lines = start.lines,
                main.lines = main.lines,
                end.lines = end.lines
                )
           )
}

#' get the star lines from \code{stargazer} output
#'
#' This function will return a list
#'
#' @param star the \code{stargazer} ouput
#' @param reg (logical) set to TRUE for a regression
#' @return a list with the \code{start.lines},
#' \code{main.lines}, and \code{end.lines} of the \code{stargazer}
#' output
get_star_lines <- function(star, reg) {

    star.line.numbers <- get_star_line_numbers(star, reg)

    star.lines <- list(start.lines = star[star.line.numbers$start.lines],
                       main.lines = star[star.line.numbers$main.lines],
                       end.lines = star[star.line.numbers$end.lines]
                       )
    return(star.lines)
}



