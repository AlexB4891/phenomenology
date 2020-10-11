
# Libraries ---------------------------------------------------------------

library(purrr)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)
library(forcats)
library(readr)
library(stringr)
library(rlang)
library(lubridate)
library(janitor)

# test_class --------------------------------------------------------------

test_that("test_class", {


  price_q0  <- c(100,130)

  slope <- c(2,-6)

  rsc <- create_market(price_q0 = c(100,130),
                       slope= c(2,-6))

  expect_is(rsc,class = "market_curves")

  expect_named(rsc[[1]],expected = c("name",
                                "funs",
                                "equation",
                                "coeficients",
                                "intercept"))

  expect_named(rsc[[2]],expected = c("name",
                                     "funs",
                                     "equation",
                                     "coeficients",
                                     "intercept"))

  expect_vector(map_chr(rsc,"name"),
                ptype = character(),
                size = length(price_q0))

  funns <- map(rsc,"funs")

  values <- c(102,124)

    walk2(funns,
          values,~{

            f <- .x

            expect_is(f,class = "function")

            expect_equal(f(1),.y)

      })

    vector <- map_dbl(rsc,"intercept")

    expect_setequal(vector, expected = c(100,130))


})



# test_market -------------------------------------------------------------

test_that("test_market",{

  rsc <- create_market(price_q0 = c(100,130),
                       slope= c(2,-6))


  # Consider extreme cases: supply and demands perfectly inelestic

  # rsc2 <- create_market(price_q0 = c(100,130),
                       # slope= c(1,-Inf))

  # rsc3 <- create_market(price_q0 = c(100,130),
                        # slope= c(0,-8))

  mkt <- linear_curve(rsc,
                      "Market of food")



 expect_is(mkt$market,class = "ggplot")


})

