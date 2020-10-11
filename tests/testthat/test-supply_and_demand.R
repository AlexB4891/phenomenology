test_that("test_grafico", {

  sd_curve <- phenomenology::linear_curve(
    price_q0 = c(100,200),
    slope = c(2,5))

  expect_type(object = sd_curve,
              type = c("gg","ggplot")
              )
})
