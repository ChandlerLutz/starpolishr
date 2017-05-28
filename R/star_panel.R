## c:/Dropbox/Rpackages/starpolish/R/star_panel.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-19


#' To get a panel of stargazer output
#'
#' Currently only the same lhs variables and
#' the same summary statistics across all panels are allowed
#'
#' @param ... \code{stargazer} output
#' @param starlist a list of \code{stargazer} output
#' @param panel.names the names for the panels in the same order as
#'     the stargazer output
#' @param reg (logical) set to TRUE if the panels are regression
#'     output; defualt is TRUE
#' @param same.lhs.vars (logical) set to TRUE if all panels have the
#'     same left-hand-side variables. Currently, only TRUE is
#'     supported
#' @param same.summary.stats (logical) set to TRUE if all panels have
#'     the same summary stats. Currently, only TRUE is supported
#' @param panel.label.fontface The fontface for the panel
#'     labels. Options are "plain", "italic", "bold",
#'     "bold.italic". Default is "plain".
#' @return the updated \code{stargazer} output
#' @examples
#' ## -- Regressoin example -- ##
#' library(stargazer)
#' data(mtcars)
#' ##First set up models without weight
#' mod.mtcars.1 <- lm(mpg ~ hp, mtcars)
#' mod.mtcars.2 <- lm(mpg ~ hp + cyl, mtcars)
#' star.out.1 <- stargazer(mod.mtcars.1, mod.mtcars.2, keep.stat = "n")
#' ##Second set of models with weight as a regressor
#' mod.mtcars.3 <- lm(mpg ~ hp + wt, mtcars)
#' mod.mtcars.4 <- lm(mpg ~ hp + cyl + wt, mtcars)
#' star.out.2 <- stargazer(mod.mtcars.1, mod.mtcars.2, keep.stat = "n")
#'
#' ##stargazer panel
#' star.panel.out <- star_panel(star.out.1, star.out.2,
#'                              panel.names = c("Without Weight", "With Weight")
#'                             )
#' print(star.panel.out)
#' ##write to a tex file and compile to check output
#' \dontrun{
#' tex_write(star.panel.out, file = "my_tex_file.tex", headers = TRUE)
#' }
#' @export
star_panel <- function(..., starlist = NULL, panel.names,
                       reg = TRUE, same.lhs.vars = TRUE,
                       same.summary.stats = TRUE,
                       panel.label.fontface = "plain") {

    ##Get all of the star output
    starlist <- c(list(...), starlist)
    latex <- is.latex(starlist[[1]])
    text <- is.text(starlist[[1]])

    if (!latex) stop("Currently only latex supported")

    if (length(panel.names) != length(starlist)) {
        stop("the length of panel.names needs to be the same as the length of starlist")
    }

    ##Get the panel names using letters
    for (i in seq_along(panel.names)) {
        ##Update the panel name
        panel.names[i] <- paste0("Panel ", LETTERS[i], ": ", panel.names[i])
    }

    ##Now get the star lines
    star.lines <- lapply(starlist, get_star_lines, reg = reg)

    ##The starting and ending lines
    start.lines <- star.lines[[1]]$start.lines
    end.lines <- star.lines[[1]]$end.lines

    ##Get just the main lines
    star.lines <- lapply(star.lines, function(x) x$main.lines)

    ##Add the panel names at the top of the main lines
    for (i in seq_along(panel.names)) {

        ##The temporary panel name
        ##note: add @{} to left alignment as stargazer adds
        ##extra line space. see http://tex.stackexchange.com/a/276468/16412
        temp.panel.name <- paste0("\\\\[-2.0ex] \\multicolumn{",
                                  star_ncol(star.lines[[i]]),
                                  ##make sure to add an empty column after the
                                  ##panel name
                                  "}{@{} l}{")
        ##Add in the panel name with the fontface requested by the user
        if (panel.label.fontface == "plain") {
            temp.panel.name <- paste0(temp.panel.name, panel.names[i], "}")
        } else if (panel.label.fontface == "bold") {
            temp.panel.name <- paste0(temp.panel.name, "\\textbf{", panel.names[i], "}}")
        } else if (panel.label.fontface == "italic") {
            temp.panel.name <- paste0(temp.panel.name, "\\textit{", panel.names[i], "}}")
        } else if (panel.label.fontface == "bold.italic") {
            temp.panel.name <- paste0(temp.panel.name,
                                      "\\textit{\\textbf{", panel.names[i], "}}}")
        } else {
            stop('temp.panel.name must be "plain", "bold", "italic", or "bold.italic')
        }

        ##Finish the line and add a little space
        temp.panel.name <- paste0(temp.panel.name, "\n \\\\",
                                  "\n \\\\[-1.5ex]")

        ##If not the last panel, add the hline and a little
        ##space after. Otherwise, add nothing
        if (i != length(panel.names)) {
            ##
            hline.end <- "\\\\[-1.83ex] \n \\hline \\\\[-1.83ex]"
        } else {
            hline.end <- NULL
        }

        star.lines[[i]] <- c(temp.panel.name,
                             star.lines[[i]],
                             hline.end
                             )

        }

    out <- c(start.lines, unlist(star.lines), end.lines)

    return(out)

}
