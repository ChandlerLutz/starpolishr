## c:/Dropbox/Rpackages/starpolish/R/star_table_notes.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

#' stargazer table notes using latex \code{caption*} package
#'
#' Only latex is supported
#'
#' @param star \code{stargazer} output
#' @param note the note to be added to the latex table
#' @return the updated \code{stargazer} output
#' @examples
#' library(stargazer)
#' data(mtcars)
#' ##First set up models without weight
#' mod.mtcars.1 <- lm(mpg ~ hp, mtcars)
#' mod.mtcars.2 <- lm(mpg ~ hp + cyl, mtcars)
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2)
#' star.out <- star_notes_caption(star.out, "custom note")
#' print(star.out)
#' @export
star_notes_caption <- function(star, note) {

    if (!is.latex(star)) stop("Only Latex currently supported")

    note <- paste0("\\caption*{\\footnotesize{\\emph{Notes:} ",
                   note, "}}")

    ##First remove the standard stargazer note line
    ##if it exists
    if (grepl("Note:", star) %>% any) {
        note.line <- grepl("Note:", star) %>% which
        star <- star[-note.line]
    }

    end.tabular.line <- grepl("end\\{tabular\\}", star) %>% which

    ##add the note
    star.out <- c(star[1:end.tabular.line],
                  note,
                  star[(end.tabular.line + 1):length(star)]
                  )
    return(star.out)

}
