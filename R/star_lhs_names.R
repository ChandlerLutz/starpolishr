## c:/Dropbox/Rpackages/starpolish/R/star_lhs_names.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-19


#' Mode function
#'
#' https://www.tutorialspoint.com/r/r_mean_median_mode.htm
#' @param v the vector
#' @return the mode
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}


#' To replace the names of the LHS variables in \code{stargazer} output
#'
#' To replace the names of the RFHS variables in \code{stargazer}.
#' The function also allows for optional second and third lines
#'
#' Note: the \code{pattern} and \code{line1} arguments must have
#' the same length. If \code{line2} and \code{line3} are not \code{NULL},
#' they must have the same length as \code{pattern} and \code{line1}
#'
#' @param star the \code{stargazer} output
#' @param pattern the regular expression pattern
#' @param line1 the variable name in the first line
#' @param line2 the optional variable name in the second line
#' @param line3 the optional variable name in teh third line
#' @return character vector with stargazer output
#' with the updated LHS variable names
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
#' ##update the LHS variable names using one line only
#' star.out.1 <- star_lhs_names(star.out,
#'                              pattern = c("mpg", "hp"),
#'                              line1 = c("miles per gallon", "horsepower")
#'                             )
#' print(star.out.1)
#' ##Sometimes variables can have really long names and
#' ##would be best written on 2 lines
#' star.out.2 <- star_lhs_names(star.out,
#'                              pattern = c("mpg", "hp"),
#'                              line1 = c("miles per", "horse-"),
#'                              line2 = c("gallon", "power")
#'                             )
#' print(star.out.2)
#' ##For this example, three lines might be even better
#' star.out.3 <- star_lhs_names(star.out,
#'                              pattern = c("mpg", "hp"),
#'                              line1 = c("miles", "horse-"),
#'                              line2 = c("per", "power"),
#'                              line3 = c("gallon", "")
#'                             )
#' print(star.out.3)
#'
#' ##Text Examples
#' ##Note that star_lhs_names() will NOT adjust the
#' ##length of the character elements if the variable names in line1,
#' ##line 2, or line3 are longer than those that they are replacing
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, mod.mtcars.3,
#'                       type = "text")
#' print(star.out)
#' ##update the LHS variable names using one line only
#' star.out.1 <- star_lhs_names(star.out,
#'                              pattern = c("mpg", "hp"),
#'                              line1 = c("miles per gallon", "horsepower")
#'                             )
#' print(star.out.1)
#' ##Sometimes variables can have really long names and
#' ##would be best written on 2 lines
#' star.out.2 <- star_lhs_names(star.out,
#'                              pattern = c("mpg", "hp"),
#'                              line1 = c("miles per", "horse-"),
#'                              line2 = c("gallon", "power")
#'                             )
#' print(star.out.2)
#' ##For this example, three lines might be even better
#' star.out.3 <- star_lhs_names(star.out,
#'                              pattern = c("mpg", "hp"),
#'                              line1 = c("miles", "horse-"),
#'                              line2 = c("per", "power"),
#'                              line3 = c("gallon", "")
#'                             )
#' print(star.out.3)
#' @export
star_lhs_names <- function(star, pattern, line1, line2 = NULL, line3 = NULL) {

    ##if not null, pattern, line1, line2, and line3 must have the same length
    ##pattern must be the same length as line1
    if (length(pattern) != length(line1)) {
        stop("pattern must be the sampe length as line 1")
    }

    ##Get either latex or text output
    latex <- grepl("tabular", star) %>% any
    text <- grepl("==", star) %>% any

    ##get the line position of the dependent variables -- use the
    ##mode to get the most likely candidate
    dep.pos <- sapply(pattern, function(p) grep(p, star)[1]) %>%
        getmode

    ##Define line1, line2 and line3 as necessary
    line1.out <- star[dep.pos]

    if (!is.null(line2)) {
        line2.out <- line1.out
    } else {
        line2.out <- NULL
    }

    ##remove any extra whitespace/newlines from line2.out.
    ##note that we nee to escape back-slashes in both R and regex
    ##So, to match \\ we need 2*4 + 2 = 10 backslashes. So, 8 escape
    ##backslashes and 2 regreular back slashes
    if (!is.null(line2)) {
        line2.out <- sub("\\\\\\\\\\[.*\\]", "", line2.out)
    }
    ##if line3 is to be use, set line3.out equal to line2.out
    if (!is.null(line3) ) {
        line3.out <- line2.out
    } else {
        line3.out <- NULL
    }

    for (i in seq_along(pattern)) {
        line1.out <- sub(pattern[i], line1[i], line1.out)
        if (!is.null(line2)) {
            line2.out <- sub(pattern[i], line2[i], line2.out)
        }
        if (!is.null(line3))
            line3.out <- sub(pattern[i], line3[i], line3.out)
    }

    dependent.var <- c(line1.out, line2.out, line3.out)

    star <- c(star[1:(dep.pos - 1)], dependent.var, star[(dep.pos + 1):length(star)])

    return(star)

}
