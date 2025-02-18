## c:/Dropbox/Rpackages/starpolish/tests/testthat/test_lhs_functions.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-19


context("star_lhs_order function")

##Clear the workspace
rm(list = ls())

library(stargazer); library(magrittr)

data(mtcars)
mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
mod.mtcars.3 <- lm(hp ~ wt + cyl, mtcars)
##latex example
star.out.latex  <- stargazer(mod.mtcars.1, mod.mtcars.2, mod.mtcars.3,
                                            type = "latex")
##text example
star.out.text  <- stargazer(mod.mtcars.1, mod.mtcars.2, mod.mtcars.3,
                                           type = "text")



test_that("star_lhs_function() works for regression", {
    ## -- Latex -- ##
    expect_is(star_lhs_names(star.out.latex,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles per gallon", "horsepower")
                             ),
              "character")
    expect_is(star_lhs_names(star.out.latex,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles per", "horse-"),
                             line2 = c("gallon", "power")
                             ),
              "character")
    expect_is(star_lhs_names(star.out.latex,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles", "horse-"),
                             line2 = c("per", "power"),
                             line3 = c("gallon", "")
                             ),
              "character")
    expect_equal(star_lhs_names(star.out.latex,
                                pattern = c("mpg", "hp"),
                                line1 = c("miles per gallon", "horsepower")
                             ) %>% length,
              star.out.latex %>% length)
    expect_equal(star_lhs_names(star.out.latex,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles per", "horse-"),
                             line2 = c("gallon", "power")
                             ) %>% length,
              length(star.out.latex) + 1)
    expect_equal(star_lhs_names(star.out.latex,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles", "horse-"),
                             line2 = c("per", "power"),
                             line3 = c("gallon", "")
                             ) %>% length,
                 length(star.out.latex) + 2)
    ## -- Text -- ##
    expect_is(star_lhs_names(star.out.text,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles per gallon", "horsepower")
                             ),
              "character")
    expect_is(star_lhs_names(star.out.text,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles per", "horse-"),
                             line2 = c("gallon", "power")
                             ),
              "character")
    expect_is(star_lhs_names(star.out.text,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles", "horse-"),
                             line2 = c("per", "power"),
                             line3 = c("gallon", "")
                             ),
              "character")
    expect_equal(star_lhs_names(star.out.text,
                                pattern = c("mpg", "hp"),
                                line1 = c("miles per gallon", "horsepower")
                             ) %>% length,
              star.out.text %>% length)
    expect_equal(star_lhs_names(star.out.text,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles per", "horse-"),
                             line2 = c("gallon", "power")
                             ) %>% length,
              length(star.out.text) + 1)
    expect_equal(star_lhs_names(star.out.text,
                             pattern = c("mpg", "hp"),
                             line1 = c("miles", "horse-"),
                             line2 = c("per", "power"),
                             line3 = c("gallon", "")
                             ) %>% length,
                 length(star.out.text) + 2)
})

