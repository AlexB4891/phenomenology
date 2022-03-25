
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
  rsc <- create_market(price_q0 = c(1.50,4),
                       slope= c(0.2,-0.2))


  mkt <- linear_curve(rsc,
                      "Cantidad de botellas de biela")




  expect_is(mkt$market,class = "ggplot")

  # Consider extreme cases: supply and demands perfectly inelestic

  # Perfectly elastic demand:
  rsc2 <- create_market(price_q0 = 50,
                        slope= 1,
                        perfect_e = -76)

  cuve2 <- linear_curve(market = rsc2,market_name = "Market of food")

  expect_is(cuve2$market,class = "ggplot")

  expect_error(print(cuve2$market),NA)

  # Perfectly elastic supply:
  rsc3 <- create_market(price_q0 = c(50,60),
                        slope= c(-1,-1),
                        perfect_e = 26)

  cuve3 <- linear_curve(market = rsc3,market_name = "Market of food")

  expect_is(cuve3$market,class = "ggplot")

  expect_error(print(cuve3$market),NA)

  # Perfectly inelastic demand:
  rsc4 <- create_market(price_q0 = 50,
                        slope= 1,
                        perfect_i = -32)

  curve4 <- linear_curve(market = rsc4,market_name = "Market of food")

  expect_is(curve4$market,class = "ggplot")

  expect_error(print(curve4$market),NA)

  # Perfectly inelastic supply:
  rsc5 <- create_market(price_q0 = 50,
                        slope= -1,
                        perfect_i = 32)

  curve5 <- linear_curve(market = rsc5,market_name = "Market of food")

  expect_is(curve5$market,class = "ggplot")

  expect_error(print(curve5$market),NA)


})

