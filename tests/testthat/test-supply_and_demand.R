test_that("test_clases", {


  price_q0  <- c(100,130)

  slope <- c(2-6)

  rsc <- create_market(price_q0 = c(100,130),
                       slope= c(2-6))

  expect_is(rsc,class = "market_curves")

  expect_named(rsc,expected = c("name",
                                "funs",
                                "equation",
                                "coeficients",
                                "intercept"))

  expect_vector(map_chr(rsc,"name"), ptype = character(), size = length(price_q0))

  funns <- map(rsc,"funs")

  values <- c(102,124)

    walk2(funs,
          values,~{

            f <- .x

            expect_is(f,class = "function")

            expect_equal(f(1),.y)

      })

  expect_mapequal(map_dbl(rsc,"intercept"), expected = price_q)


})




