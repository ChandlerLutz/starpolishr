#' To change the RHS variable names of an felm model
#'
#' To change the right-hand-side (rhs) variable names of an felm
#' model. This is often useful when you want display instrumental
#' variable regressions on the same row as ols regressions (see
#' examples).
#' 
#' @param felm.model An \code{felm} model from the \code{lfe} package
#' @param old A character vector of the original rhs names
#' @param new A character vector of the new rhs names
#' @return the \code{felm} model with the rhs names
#' @examples
#' library(stargazer); library(lfe)
#' data(mtcars)
#'
#' # -- Trivial example -- #
#' mod <- felm(mpg ~ hp + wt, data = mtcars)
#' stargazer(mod, type = "text")
#' # Rename the rhs variables and re-print the stargazer table 
#' mod <- star_change_felm_rhs_names(mod, old = c("hp", "wt"), new = c("x1", "x2"))
#' stargazer(mod, type = "text")
#' 
#' # -- Combining OLS and IV on a single line using stargazer -- #
#' mod.ols <- felm(mpg ~ hp, data = mtcars)
#' mod.iv <- felm(mpg ~ 1 | 0 | (hp ~ cyl), data = mtcars)
#'
#' # Original stargazer output
#' # Notice how the two `hp` coefficients are on different lines 
#' stargazer(mod.ols, mod.iv, type = "text", keep.stat = "n")
#'
#' # In the IV model, replace the RHS var name to just "hp"
#' mod.iv <- star_change_felm_rhs_names(mod.iv, old = "`hp(fit)`", new = "hp")
#' # Reprint the stargazer output, notice how the ols and iv rhs variables
#' # are now in the same row
#' stargazer(mod.ols, mod.iv, type = "text", keep.stat = "n")
#' @export
star_change_felm_rhs_names <- function(felm.model, old, new)  {

  if (class(felm.model) != "felm")
    stop("felm.model must be an felm model")

  if (is.null(old) || is.null(old)) 
    stop("Both `old` and `new` must be set")

  if (!is.character(old) | !is.character(new)) 
    stop("Both `old` and `new` must be character vectors")

  if (length(old) != length(new))
    stop("`old` and `new` must have the same length")
  
  felm.x.var.names <- rownames(felm.model$beta)
  new.felm.x.var.names <- replace(felm.x.var.names,
                                  felm.x.var.names %in% old, new)
    rownames(felm.model$beta) <- new.felm.x.var.names
    rownames(felm.model$coefficients) <- new.felm.x.var.names

  return(felm.model)

}
