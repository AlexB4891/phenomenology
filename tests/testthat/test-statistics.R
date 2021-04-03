
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


# Auxiliary functions -----------------------------------------------------

dice_mean <- function(sample_n){

  set.seed(1984)

  mean_0 <- rerun(.n = sample_n,sample(1:6,1)) %>%
    reduce(c) %>%
    mean


  z <- (sqrt(sample_n)*(mean_0 - 3.5))/1.71

  z
}


# test_satistitcal_functions -----------------------------------------------------

test_that("test_satistitcal_functions", {

set.seed(1984)

  expect_equal(dice_mean_mod(10),dice_mean(10))


})






