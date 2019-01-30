## c:/Dropbox/Rpackages/starpolish/tests/testthat/test_rhs_functions.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-19


context("star_rhs_order function")

##Clear the workspace
rm(list = ls())

library(stargazer); library(magrittr)

library(stargazer)
data(mtcars)
mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
star.out.latex <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "latex")
star.out.text <- stargazer(mod.mtcars.1, mod.mtcars.2, type = "text")

star.out.summ.latex <- stargazer(mtcars[,c("mpg", "hp", "wt")], type = "latex")
star.out.summ.text <- stargazer(mtcars[,c("mpg", "hp", "wt")], type = "text")


## -- star_rhs_order () -- ##




test_that("star_rhs_order() works for regression", {
    expect_is(star_rhs_order(star.out.latex, c("cyl", "hp", "wt", "Constant")), "character")
    expect_equal(star_rhs_order(star.out.latex, c("cyl", "hp", "wt", "Constant")) %>% length,
                 star.out.latex %>% length)
    expect_is(star_rhs_order(star.out.text, c("cyl", "hp", "wt", "Constant")), "character")
    expect_equal(star_rhs_order(star.out.text, c("cyl", "hp", "wt", "Constant")) %>% length,
                 star.out.text %>% length)
})

test_that("star_rhs_order() works for summary stats", {
    expect_is(star_rhs_order(star.out.summ.latex, c("hp", "mpg", "wt"), reg = FALSE),
              "character")
    expect_equal(star_rhs_order(star.out.summ.latex, c("hp", "mpg", "wt"), reg = FALSE)
                 %>% length,
                 star.out.summ.latex %>% length)
    expect_is(star_rhs_order(star.out.summ.text, c("hp", "mpg", "wt"), reg = FALSE),
              "character")
    expect_equal(star_rhs_order(star.out.summ.text, c("hp", "mpg", "wt"), reg = FALSE) %>%
                 length,
                star.out.summ.text %>% length)
})

## -- star_rhs_names() -- ##

context("star_rhs_names")

test_that("star_rhs_names() works for regressions", {
    expect_is(star_rhs_names(star.out.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ),
              "character")
    expect_is(star_rhs_names(star.out.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ),
              "character")
    expect_is(star_rhs_names(star.out.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ),
              "character")
    expect_is(star_rhs_names(star.out.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ),
              "character")
    expect_equal(star_rhs_names(star.out.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ) %>% length,
                 star.out.latex %>% length
                 )
    expect_equal(star_rhs_names(star.out.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ) %>% length,
                 star.out.latex %>% length
                 )
    expect_equal(star_rhs_names(star.out.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ) %>% length,
                 star.out.text %>% length
                 )
    expect_equal(star_rhs_names(star.out.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ) %>% length,
                 star.out.text %>% length
                 )
})



test_that("star_rhs_names() works for summary stats", {
    expect_is(star_rhs_names(star.out.summ.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ),
              "character")
    expect_is(star_rhs_names(star.out.summ.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ),
              "character")
    expect_equal(star_rhs_names(star.out.summ.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ) %>% length,
                 star.out.summ.latex %>% length
                 )
    expect_equal(star_rhs_names(star.out.summ.latex,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ) %>% length,
                 star.out.summ.latex %>% length
                 )
    ## -- text -- ##
    expect_is(star_rhs_names(star.out.summ.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ),
              "character")
    expect_is(star_rhs_names(star.out.summ.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ),
              "character")
    expect_equal(star_rhs_names(star.out.summ.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horsepower", "weight")
                             ) %>% length,
                 star.out.summ.text %>% length
                 )
    expect_equal(star_rhs_names(star.out.summ.text,
                             pattern = c("hp", "wt"),
                             line1 = c("horse-", "weight"),
                             line2 = c("power", "")
                             ) %>% length,
                 star.out.summ.text %>% length
                 )
})









