library(stargazer);

library(stargazer); library(magrittr)

data(mtcars)
mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
mod.mtcars.3 <- lm(hp ~ wt + cyl, mtcars)

expect_equal(star_get_lhs_names(stargazer(mod.mtcars.1)), "mpg")
expect_equal(star_get_lhs_names(stargazer(mod.mtcars.1, mod.mtcars.2)),
             "\\multicolumn{2}{c}{mpg}")
expect_equal(star_get_lhs_names(stargazer(mod.mtcars.1, mod.mtcars.2, mod.mtcars.3)), 
             c("\\multicolumn{2}{c}{mpg}", "hp"))
expect_equal(star_get_lhs_names(stargazer(mod.mtcars.1, mod.mtcars.3, mod.mtcars.2)),
             c("mpg", "hp", "mpg"))


