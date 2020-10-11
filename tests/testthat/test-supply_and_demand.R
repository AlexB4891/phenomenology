
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


    rsc2 <- create_market(price_q0 = 100,
                         slope= 2,
                         perfect_e = 75)

    expect_length(rsc2,n = 2)

    values2 <- c(102,75)

    walk2(funns,
          values,~{

            f <- .x

            expect_is(f,class = "function")

            expect_equal(f(1),.y)

          })




})



# test_market -------------------------------------------------------------

test_that("test_market",{

  # Initial test for consistency
  rsc <- create_market(price_q0 = c(100,130),
                       slope= c(2,-6))


  mkt <- linear_curve(rsc,
                      "Market of food")



 expect_is(mkt$market,class = "ggplot")

 # Consider extreme cases: supply and demands perfectly inelestic

 # Perfectly elastic demand:
 rsc2 <- create_market(price_q0 = 50,
                       slope= 1,
                       perfect_e = -76)

 # Perfectly elastic supply:
 rsc3 <- create_market(price_q0 = 50,
                       slope= -1,
                       perfect_e = 76)

 # Perfectly inelastic demand:
 rsc4 <- create_market(price_q0 = 50,
                       slope= 1,
                       perfect_i = -32)

 # Perfectly inelastic supply:
 rsc5 <- create_market(price_q0 = 50,
                       slope= -1,
                       perfect_i = 32)

 mkt2 <-
   map(list(
     rsc2,
     rsc3,
     rsc4,
     rsc5
   ),
   linear_curve,
 "Market of food"
 )


 walk(mkt2,~{
   expect_is(.x$market,class = "ggplot")
   expect_error(print(.x$market),NA)
 })


})

