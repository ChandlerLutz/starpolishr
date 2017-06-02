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
#' @param same.lhs.vars (logical) set to \code{TRUE} if all panels
#'     have the same left-hand-side variables. Currently, only
#'     \code{TRUE} is supported
#' @param same.summary.stats (logical) set to \code{TRUE} if all
#'     panels have the same summary stats. Default is \code{TRUE}. If
#'     set to \code{FALSE}, different panels can have different
#'     summary stats. \code{same.summary.stats = FALSE} is only
#'     supported for regression and so \code{reg} must be set to
#'     \code{TRUE}
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
#' star.out.2 <- stargazer(mod.mtcars.1, mod.mtcars.2, keep.stat = c("n", "rsq"))
#'
#' ##stargazer panel -- same summary statistics across panels.
#' star.panel.out <- star_panel(star.out.1, star.out.2,
#'                              panel.names = c("Without Weight", "With Weight")
#'                             )
#' print(star.panel.out)
#' ##write to a tex file and compile to check output
#' \dontrun{
#' tex_write(star.panel.out, file = "my_tex_file.tex", headers = TRUE)
#' }
#'
#' ##stargazer panel -- different summary statistics across panels.
#' star.panel.out2 <- star_panel(star.out.1, star.out.2,
#'                               same.summary.stats = FALSE,
#'                               panel.names = c("Without Weight", "With Weight")
#'                              )
#' print(star.panel.out2)
#' ##write to a tex file and compile to check output
#' \dontrun{
#' tex_write(star.panel.out2, file = "my_tex_file2.tex", headers = TRUE)
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


    ##Initialize the summary statistics for each panel to NULL
    panel.summary.stats <- vector(mode = "list",
                                  length = length(starlist))

    ##Only regressions are allowed to have different
    ##summary stats across panels
    if (same.summary.stats == FALSE && reg == TRUE) {
        ##Get the end lines without any summary stats. The
        ##end lines will be from the second hline to the end,
        ##after the summary stats
        end.lines <- star.lines[[1]]$end.lines
        end.lines.second.hline <- grep("hline", end.lines)[2]
        end.lines <- end.lines[end.lines.second.hline:length(end.lines)]

        ##Loop through the starlist and add the summary stats
        ##Use a cline before each set of summary stats
        for (i in seq_along(panel.summary.stats)) {
            ##the end lines
            out <- star.lines[[i]]$end.lines
            ##The summary stats will be between the first and second hlines
            out.hlines <- grep("hline", out)

            ##The summary statistics
            out <- out[(out.hlines[1] + 1):(out.hlines[2] - 1)]

            ##Add in a cline before the summary stats
            out <- c(paste0("\\cline{2-", star_ncol(out), "} \\\\[-2.0ex]"),
                     out)

            ##For the bottom panel, add a little space after
            ##the summary stats
            if (i == length(panel.summary.stats))
                out <- c(out, "\\\\[-2.0ex]")

            ##Assign the output
            panel.summary.stats[[i]] <- out

            ##remove out
            rm(out, out.hlines)
        }

    } else {
        ##If the summary statistics are the same across all panels,
        ##use the summary statistics from the first panel
        end.lines <- star.lines[[1]]$end.lines

    }


    ##Get just the main lines
    star.lines <- lapply(star.lines, function(x) x$main.lines)

    ##Add the panel names at the top of the main lines
    for (i in seq_along(starlist)) {

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

        ##If there are going to be different summary statistics
        ##for each panel, add them in

        star.lines[[i]] <- c(temp.panel.name,
                             star.lines[[i]],
                             panel.summary.stats[[i]], ##Add in the panel summ stats
                             hline.end
                             )

        }

    out <- c(start.lines, unlist(star.lines), end.lines)

    return(out)

}
