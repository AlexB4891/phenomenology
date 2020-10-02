


test_that("multiplication works", {

  # viepi <- read_delim(file = "data/viepi_prec.csv")

  result <- f_cubo_muestras(viepi_prec,identificador_unico = "variable_ident")

  out_nam <- c("variable_ident",
               "P_C_muestra",
               "P_S_diagnos1",
               "M_N_numero",
               "M_C_ademuestra",
               # "M_C_lab_tomo",
               "M_C_resulcovid",
               "M_C_tmuestra",
               "M_F_entmuestra",
               "M_F_promuestra",
               "M_F_recmuestra",
               "M_F_tomamuestra"
               # ,
               # "M_I_lab_proc"
               )

  conteo <- map2(.x = result ,
                 .y = c("0","1"),
                 ~.x %>%
                   group_by(P_C_muestra) %>%
                   tally(name = "N") %>%
                   filter(P_C_muestra == .y) %>%
                   pull("N"))

  expect_named(result[[1]],
               out_nam)

  expect_named(result[[2]],
               out_nam)

  expect_equal(conteo[[1]] == nrow(result[[1]]),TRUE)

  expect_equal(conteo[[2]] == nrow(result[[2]]),TRUE)
})

