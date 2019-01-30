## c:/Dropbox/Rpackages/starpolish/tests/testthat/test_star_panel.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

library(stargazer)
data(mtcars)
##First set up models without weight
mod.mtcars.1 <- lm(mpg ~ hp, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + cyl, mtcars)
star.out.1 <- stargazer(mod.mtcars.1, mod.mtcars.2, keep.stat = "n")
##Second set of models with weight as a regressor
mod.mtcars.3 <- lm(mpg ~ hp + wt, mtcars)
mod.mtcars.4 <- lm(mpg ~ hp + cyl + wt, mtcars)
star.out.2 <- stargazer(mod.mtcars.1, mod.mtcars.2, keep.stat = "n")

context("test star_panel() function")

##stargazer panel
star.panel.out <- star_panel(star.out.1, star.out.2,
                             panel.names = c("Without Weight", "With Weight")
                             )

test_that("star_panel() function works", {
    expect_is(star.panel.out, "character")
})


## Test that star_panel() works with a list
star.panel.out <- star_panel(starlist = list(star.out.1, star.out.2),
                             panel.names = c("Without Weight", "With Weight")
                             )

test_that("star_panel() function works iwth a list", {
    expect_is(star.panel.out, "character")
})
