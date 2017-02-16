## c:/Dropbox/Rpackages/starpolish/R/star_cleaning_functions.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

##Extra functions for cleaning stargazer ouput

#' Insert column numbrers in a table
#'
#' Currently this only works for latex. This function is helpful when
#' creating a stargazer table from matrix output as stargazer does not
#' automatically use column numbers for matrices
#'
#' @param star the \code{stargazer} output
#' @param insert.after insert the column numbers after
#'     \code{inster.after}
#' @param skip.col.1 skip the first column? If \code{TRUE} no column
#'     number will be placed after the first column
#' @return an updated version of the stargazer table with
#' @examples
#' library(stargazer)
#' data(mtcars)
#' star.out <- stargazer(as.matrix(head(mtcars)))
#' print(star.out)
#' star.out <- star_add_column_numbers(star.out, insert.after = 10)
#' print(star.out)
#' @export
star_add_column_numbers <- function(star, insert.after, skip.col.1 = TRUE) {

    if (!is.latex(star))
        stop("star_add_column_numbers() currently only supported with latex")

    ##The number of columns
    num.cols <- star_ncol(star)


    if (skip.col.1) {
        ##if skip.col.1 is TRUE, skip the first column

        ##get the column numbers
        col.nums <- paste0("(", 1:(num.cols - 1), ")", collapse = " & ")
        ##add the first & for the first row and the end of the latex column
        col.nums <- paste0(" & ", col.nums, " \\\\ ")
    } else {
        ##use a column number for the first column

        ##get the column numbers
        col.nums <- paste0("(", 1:(num.cols - 1), ")", collapse = " & ")
        ##add the first & for the first row and the end of the latex column
        col.nums <- paste0(" & ", col.nums, " \\\\ ")
    }

    return(star_insert_row(star, col.nums, insert.after = insert.after))

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


#' Insert a row in a stargazer table
#'
#' @param star the \code{stargazer} output
#' @param string the string of text to insert into the table
#' @param insert.after insert \code{string} after
#'     \code{inster.after}. \code{insert.after} can be a vector, so
#'     you can insert multiple strings at one time. If
#'     \code{insert.after} has length 1, then all elements of
#'     \code{string} are insterted after \code{insert.after}.
#' @return the updated \code{stargazer} output
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
#' star_insert_row(star.out, "Controls? & No & No & No \\\\", insert.after = 27)
#' star_insert_row(star.out,
#'                c("Controls? & No & No & No \\\\",
#'                  "Linear Model? & Yes & Yes & Yes \\\\"),
#'                insert.after = c(27, 32))
#' @export
star_insert_row <- function(star, string, insert.after) {

    if (length(string) < length(insert.after)) {
        stop("Error: in star_insert_row() insert.after has more elements than string")
    }

    if (length(insert.after) == 1) {
        ##If the length of insert.after is 1, use the append function
        return(append(star, string, after = insert.after))
    }
    ##c(star[1:insert.after], string, star[(insert.after + 1):length(star)]))

    if (length(string) > length(insert.after)) {
        ##string has more elements than Insert after
        ##fill insert.after with its last element so its length
        ##matches string
        length.diff <- length(string) - length(insert.after)
        insert.after <- c(insert.after,
                          rep(insert.after[length(insert.after)], length.diff))
    }

    ##From http://stackoverflow.com/a/1495204/1317443
    id <- c(seq_along(star), insert.after + 0.5)
    star <- c(star, string)
    star <- star[order(id)]
    return(star)
}


#' get the number of stargazer columns
#'
#' @param star the \code{stargazer} output
#' @return the number of columns in the \code{stargazer} output
#' @examples
#' library(stargazer)
#' data(mtcars)
#' star.out <- stargazer(mtcars)
#' star_ncol(star.out)
#' @export
star_ncol <- function(star) {

    return(max(stringr::str_count(star, "&")) + 1)
}

#' A function to for scientific unit columns from the latex
#' \code{siunitx} package
#'
#' The latex \code{siunitx} package has to be loaded in the preamble.
#' See this answer on tex stackexchange:
#' \url{http://tex.stackexchange.com/a/2747} This function is useful
#' with \code{paste0} when updating column to work \code{siunitx} and
#' aligning decimals.
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

