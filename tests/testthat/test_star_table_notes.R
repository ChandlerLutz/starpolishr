## c:/Dropbox/Rpackages/starpolish/tests/testthat/test_star_table_notes.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-21

##Clear the workspace
rm(list = ls())

##For star_table notes

context("test star_notes_caption() function")

library(stargazer)
data(mtcars)
##First set up models without weight
mod.mtcars.1 <- lm(mpg ~ hp, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + cyl, mtcars)
star.out <- stargazer(mod.mtcars.1, mod.mtcars.2)
star.out <- star_notes_tex(star.out, note.type = "caption", "custom note")

test_that("star_notes_tex() works", {
    expect_is(star_notes_tex(star.out, note.type = "caption", "custom note"), "character")
})

