## c:/Dropbox/Rpackages/starpolish/R/star_rhs_order.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-19

#' Manipulate the order of right-hand side variables using regex
#'
#' Use regular expressions to manipulate the right hand side
#' variables in in a stargazer regrssion or table.
#' This function is compatible works with \code{stargazer} output of type \code{latex}
#' and \code{text}.
#'
#' @param star the stargazer table
#' @param pattern a vector of regular expressions which
#' the order with which the right-hand-side variables will
#' be used
#' @param reg (logical) set to TRUE for a regression; defaults to
#' \code{TRUE}
#' @return character vector with stargazer output
#' with the updated order
#' @examples
#' ## -- Regression example -- ##
#' library(stargazer)
#' data(mtcars)
#' mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
#' mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
#' ##latex example
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "latex")
#' print(star.out)
#' star.out <- star_rhs_order(star.out, c("cyl", "hp", "wt", "Constant"))
#' print(star.out)
#' ##text example
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "text")
#' print(star.out)
#' star.out <- star_rhs_order(star.out, c("cyl", "hp", "wt", "Constant"))
#' print(star.out)
#'
#' ## -- Summary stats example -- ##
#' ##latex example
#' star.out.summ <- stargazer(mtcars[,c("mpg", "hp", "wt")], type = "latex")
#' print(star.out.summ)
#' star.out.summ <- star_rhs_order(star.out.summ, c("hp", "mpg", "wt"), reg = FALSE)
#' print(star.out.summ)
#' ##text example
#' star.out.summ <- stargazer(mtcars[,c("mpg", "hp", "wt")], type = "text")
#' print(star.out.summ)
#' star.out.summ <- star_rhs_order(star.out.summ, c("hp", "mpg", "wt"), reg = FALSE)
#' print(star.out.summ)
#'
#' ## -- Keeping a subset of all variables -- ##
#' ##As star_rhs_order() matches based on regular expressions,
#' ##it can also be used to show only a subset of all variables
#'
#' ##Latex example
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "latex")
#' print(star.out)
#' ##Only keep hp
#' star.out <- star_rhs_order(star.out, c("hp"))
#' print(star.out)
#'
#' ##text example
#' star.out <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "text")
#' print(star.out)
#' ##Only keep hp
#' star.out <- star_rhs_order(star.out, c("hp"))
#' print(star.out)
#'
#' @export
star_rhs_order <- function(star, pattern, reg = TRUE) {
    ##Assume that each coefficient comes with a second line for the SE or other output
    ##are there empty lines between the varaibles (logical)
    ##either latex or text
    latex <- is.latex(star)
    text <- is.text(star)

    star.lines <- get_star_line_numbers(star, reg = reg)
    start.lines <- star.lines$start.lines
    main.lines <- star.lines$main.lines
    end.lines <- star.lines$end.lines

    ##Check for empty lines in the main output
    empty.lines <- (!grepl("[a-zA-Z0-9]", star[main.lines])) %>% any
    ## The  ^\\s$+   regex starts a string looking for one or more empty lines, then finishes
    ## the string
    ## empty.lines <- ifelse(grepl("^\\s+$", star) | grepl("& &", star), TRUE, FALSE) %>%
    ##     any

    ##Check if there's extra regression output
    ##e.g. a standard error
    if (latex) {
        ##This regex will check for an empty space at the start of the string
        ##followed by &   Then, it will look for numbers
        regression.extra <- grepl("(^\\s+&)(.*[0-9])", star[main.lines]) %>%
            any
    } else if (text) {
        ##This regex will check for an empty space at the beginning of the string
        ##and the for any other alphanumeric character. Strings with all white
        ##space will return false
        regression.extra <- grepl("(^\\s+)(.*[0-9a-zA-Z])", star[main.lines]) %>%
            any
    }

    ##Get the lines with the coefficients that we want keep in order
    coef.lines <- vector("numeric", length = length(pattern))

    for (i in seq_along(pattern)) {
        temp <- grep(pattern[i], star)
        if (length(temp) == 0) {
            stop(paste0(pattern[i], " is not found in star"))
        } else if (length(temp) > 1) {
            stop(paste0(pattern[i], " matches more than 1 variable"))
        }
        coef.lines[i] <- temp
    }
    ##Also add in the standard errors and possible empty lines
    if (empty.lines && regression.extra) {
        ##both empty lines and regression.extra info
        coef.lines <- c(rbind(coef.lines, coef.lines+1, coef.lines+2))
    } else if (!empty.lines && regression.extra) {
        ##regression.info but no empty lines
        coef.lines <- c(rbind(coef.lines, coef.lines+1))
    } else if (!empty.lines && !regression.extra) {
        ##neither regression info or empty lines
        coef.lines <- coef.lines
    } else {
        stop("error in empty.lines or regression flag variables")
    }

    star.out <- star[c(start.lines, coef.lines, end.lines)]

    return(star.out)

}
