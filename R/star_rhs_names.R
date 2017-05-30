## c:/Dropbox/Rpackages/starpolish/R/star_rhs_names.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-19

##Functions to replace the rhs names for variables


#' Helper function for star_rhs_names
#'
#' A helper function to replace the names of the stargazer variables
#' and possibly add a second line on the standard error.
#' This function works with regression only
#'
#' @param star the stargazer output
#' @param pattern the regex pattern
#' @param line1 the first line for the variable name
#' @param line2 the second line for the variable name
#' @return the update stargazer output
.star_rhs_replace <- function(star, pattern, line1, line2) {

    ##Get the line where the variable is
    line.var <- grep(pattern, star)

    if (length(line.var) > 1) {
        stop(paste0("pattern ", pattern,
                    "matches to more than one line in the stargazer output")
             )
    }

    ##Add the second var if requested by the user
    if (!is.null(line2)) {
        ##if line2 is set, add it in
        star[line.var + 1] <- paste0(line2, star[line.var + 1])
    }
    ##Replace the main variable
    star[line.var] <- sub(pattern, line1, star[line.var])
    return(star)
}

#' To replace the names of the RHS variables in \code{stargazer} output
#'
#' To replace the names of the RHS variables in \code{stargazer}.
#' The function also allows for an optional second line.
#'
#' Note: the \code{pattern} and \code{line1} arguments must have the
#' same length. If \code{line2} is not \code{NULL}, it must have the
#' same length as \code{pattern} and \code{line1}. Also, \code{line2}
#' will be inserted into \code{star} directly.
#'
#' @param star the \code{stargazer} output
#' @param pattern the regular expression pattern
#' @param line1 the variable name in the first line
#' @param line2 the optional variable name in the second line. The
#' \code{line2} argument takes advantage of the notion that the standard
#' error (for example) of a regression is printed on the line below
#' the coefficient. NOTE: Text in \code{line2} will be inserted directly.
#' @return a character vector with the updated stargazer output
#' @examples
#' ## -- Regression example -- ##
#' library(stargazer)
#' data(mtcars)
#' mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
#' mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
#'
#' ##Latex example
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "latex")
#' print(star.out)
#' ##Update the names hp and wt using one line only
#' star.out.1 <- star_rhs_names(star.out,
#'                              pattern = c("hp", "wt"),
#'                              line1 = c("horsepower", "weight")
#'                             )
#' print(star.out.1)
#' ##Sometimes variables can have really long names and
#' ##would be best written on 2 lines
#' star.out.1 <- star_rhs_names(star.out,
#'                              pattern = c("hp", "wt"),
#'                              line1 = c("horse-", "weight"),
#'                              line2 = c("power", "")
#'                             )
#' print(star.out.1)
#'
#' ##Text example
#' ##Note that star_rhs_names() will NOT adjust the
#' ##length of the character elements if the variable names in line1
#' ##or line 2 are longer than those that they are replacing
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "text")
#' print(star.out)
#' ##Update the names hp and wt using oneline
#' star.out.1 <- star_rhs_names(star.out,
#'                              pattern = c("hp", "wt"),
#'                              line1 = c("horsepower", "weight")
#'                             )
#' print(star.out.1)
#' ##Sometimes variables can have really long names and
#' ##would be best written on 2 lines
#' star.out.1 <- star_rhs_names(star.out,
#'                              pattern = c("hp", "wt"),
#'                              line1 = c("horse-", "weight"),
#'                              line2 = c("power", "")
#'                             )
#' print(star.out.1)
#'
#'
#' ## -- Summary stats example -- ##
#' ##latex example
#' star.out.summ <- stargazer(mtcars[,c("mpg", "hp", "wt")], type = "latex")
#' print(star.out.summ)
#' star.out.summ <- star_rhs_names(star.out.summ,
#'                                 pattern = c("hp", "wt"),
#'                                 line1 = c("horsepower", "weight")
#'                                )
#' print(star.out.summ)
#' ##text example
#' star.out.summ <- stargazer(mtcars[,c("mpg", "hp", "wt")], type = "text")
#' star.out.summ <- star_rhs_names(star.out.summ,
#'                                 pattern = c("hp", "wt"),
#'                                 line1 = c("horsepower", "weight")
#'                                )
#' print(star.out.summ)
#' @export
star_rhs_names <- function(star, pattern, line1, line2 = NULL) {

    ##pattern and line1 must have the same length
    ##if line1 to is set it must have the same length as line1
    if (length(pattern) != length(line1))
        stop("pattern and line1 do not have the same length")
    if (!is.null(line2) && (length(line2) != length(line1)))
        stop("line1 and line2 do not have the same length")

    ##loop through and update star
    for (i in seq_along(pattern)) {
        star <- .star_rhs_replace(star, pattern[i], line1[i], line2[i])
    }
    return(star)

}
