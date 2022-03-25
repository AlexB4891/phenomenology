
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


  mkt <- supply_and_demand(rsc,
                      "Market of food")



 expect_is(mkt$market,class = "ggplot")

 ejemplos <- tibble::tibble(
   price_q0 = c(50,50,50,50),
   slope= c(1,-1,1,-1),
   perfect_e = c(-76,41,NA,NA),
   perfect_i = c(NA,NA,-32,32)
 ) %>%
   pmap(create_market)


 plots <- map2(.x = ejemplos,
               .y = list(
                 "Supply + Perfectly elastic demand",
                 "Demand + Perfectly elastic supply",
                 "Supply + Perfectly inelastic demand",
                 "Demand + Perfectly inelastic supply"
               ),~supply_and_demand(market = .x,market_name = .y))




 walk(plots,~{
   expect_is(.x$market,class = "ggplot")
   expect_error(print(.x$market),NA)
 })


})

