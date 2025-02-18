## c:/Dropbox/Rpackages/starpolish/R/star_table_notes.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

## #' stargazer table notes using latex \code{caption*} package
## #'
## #' Only latex is supported
## #'
## #' @param star \code{stargazer} output
## #' @param note the note to be added to the latex table
## #' @return the updated \code{stargazer} output
## #' @examples
## #' library(stargazer)
## #' data(mtcars)
## #' ##First set up models without weight
## #' mod.mtcars.1 <- lm(mpg ~ hp, mtcars)
## #' mod.mtcars.2 <- lm(mpg ~ hp + cyl, mtcars)
## #' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2)
## #' star.out <- star_notes_caption(star.out, "custom note")
## #' print(star.out)
## star_notes_caption <- function(star, note) {

##     warning("Warning: star_notes_caption() is deprecated, use star_notes_tex()")

##     if (!is.latex(star)) stop("Only Latex currently supported")

##     note <- paste0("\\caption*{\\footnotesize{\\emph{Notes:} ",
##                    note, "}}")

##     ##First remove the standard stargazer note line
##     ##if it exists
##     if (grepl("Note:", star) %>% any) {
##         note.line <- grepl("Note:", star) %>% which
##         star <- star[-note.line]
##     }

##     end.tabular.line <- grepl("end\\{tabular\\}", star) %>% which

##     ##add the note
##     star.out <- c(star[1:end.tabular.line],
##                   note,
##                   star[(end.tabular.line + 1):length(star)]
##                   )
##     return(star.out)

## }


#' \code{stargazer} tex table notes
#'
#' @param star \code{stargazer} output
#' @param note.type the type of latex note to be used. Options include
#'     \code{"caption"} for use with the latex caption package and
#'     \code{"threeparttable"} for use with the latex threeparttable
#'     package. Only the first element of type will be used
#' @param note the note to be added to the latex table
#' @examples
#' ## -- Regression Example -- ##
#' library(stargazer)
#' data(mtcars)
#' mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
#' mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
#' mod.mtcars.3 <- lm(hp ~ wt + cyl, mtcars)
#' ##latex example
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, mod.mtcars.3,
#'                       type = "latex")
#' print(star.out)
#' ##use latex caption package
#' star_notes_tex(star.out, note = "my table notes")
#' ##use latex caption package
#' star_notes_tex(star.out, note.type = "caption", note = "my table notes")
#' ##use latex threeparttable package
#' star_notes_tex(star.out, note.type = "threeparttable", note = "my table notes")
#' @export
star_notes_tex <- function(star, note.type = c("caption", "threeparttable"), note) {

    if (!is.latex(star)) {
        stop("star_notes_tex() only accepts latex stargazer output")
    }

    ##First remove the standard stargazer note line
    ##if it exists
    if (grepl("Note:", star) %>% any) {
        note.line <- grepl("Note:", star) %>% which
        star <- star[-note.line]
    }

    note.type <- note.type[1]

    if (note.type == "caption") {

        ##The caption
        note <- paste0("\\caption*{\\footnotesize{\\textit{Notes:} ",
                       note, "}}")



        end.tabular.line <- grepl("end\\{tabular\\}", star) %>% which

        ##add the note
        star.out <- c(star[1:end.tabular.line],
                      note,
                      star[(end.tabular.line + 1):length(star)]
                      )
    } else if (note.type == "threeparttable") {

        ##Note: we have to turn start into a threeparts table.
        ##This means putting the three partstable begining and end
        ##before the start and end of the table

        ##Inser the \begin{threepartstable}
        begin.table <- grep("\\\\begin\\{table\\}", star)
        end.table <- grep("\\\\end\\{table\\}", star)
        ##for the beginning of the table notes
        end.tabular <- grep("\\\\end\\{tabular\\}", star)
        threeparttable.string <- c("\\begin{threeparttable}",
                                   paste0("\\begin{tablenotes}\n",
                                          "\\footnotesize\n",
                                          "\\item \\textit{Notes:} ", note,
                                          "\n",
                                          "\\end{tablenotes}"),
                                   "\\end{threeparttable}"
                                   )
        threeparttable.ind <- c(begin.table + 2,
                                end.tabular,
                                end.table - 1
                                )


        star.out <- star_insert_row(star, threeparttable.string, insert.after = threeparttable.ind)

    } else {
        stop("Error: in star_notes_tex(), note.type must be either 'caption' or 'threeparttable'")
    }


    return(star.out)

}


