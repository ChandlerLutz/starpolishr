
context("star_ncol function")

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

##the original, expected number of latex columns 
star.ncol.expected <- 4

test_that("star_ncol returns the correct number of columns",
          expect_equal(star_ncol(star.out.latex), star.ncol.expected)
          )

##Add an ampersand (&) side to one of the labels
star.out.latex <- star.out.latex %>%
  sub("Constant", "Intercept \\\\& Constant", x = .)

test_that(
  "star_ncol returns correct number of cols when there is an escaped & in the table",
  expect_equal(star_ncol(star.out.latex), star.ncol.expected)
)
            

          

