
<!-- README.md is generated from README.Rmd. Please edit that file -->
starpolishr
===========

This package allows for easy post polishing of latex [stargazer](https://cran.r-project.org/web/packages/stargazer/index.html) output. See [this vignette](https://github.com/ChandlerLutz/starpolishr/blob/master/vignettes/pdf/starpolishr-intro.pdf) for a quick introduction. For examples, see the `Tables` section of my papers [here](https://chandlerlutz.github.io/pdf/california-foreclosure-prevention-laws.pdf) and [here](https://chandlerlutz.github.io/pdf/local-labor-markets-canada-us.pdf). All of these tables were generated exclusively within R using `stargazer` and `starpolishr`.

Installation
------------

You can install starpolishr from github with:

``` r
# install.packages("devtools")
devtools::install_github("ChandlerLutz/starpolishr")
```

Quick Tips
----------

-   The first argument in all `starpolishr` functions is a `stargazer` table, allowing for easy compatibility with the [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html) `%>%`.
-   All of the functions in the `starpolishr` package begin with `star_` for easy `tab` completion in emacs or rstudio.

Key Functions
-------------

Here is a list of key `starpolishr` functions in order of how often I use them. See their help files for more details.

-   `star_tex_notes` -- Adds custom notes using either the latex [caption](https://www.ctan.org/pkg/caption) or the latex [threeparttable](https://www.ctan.org/pkg/threeparttable).
-   `star_tex_write` -- writes latex tables to a file and optionally adds header files and common packages
-   `star_lhs_names` and `star_rhs_names` -- updates variable names using regular expressions. The advantage of these functions is that they allow for variable names to span more than one line.
-   `star_panel` -- panels multiple related `stargazer` tables into a single latex table.
-   `star_insert_row` -- insert a row after a given row in a `stargazer` table
-   `star_sidewaystable` -- converts a latex table to a latex `sidewaystable`
-   `star_add_column_numbers` -- if you pass a matrix to `stargazer`, it won't add column numbers. This function will add column numbers to a latex table.

Authors
-------

[Chandler Lutz](https://chandlerlutz.github.io/)

License
-------

GPL (&gt;= 2)
