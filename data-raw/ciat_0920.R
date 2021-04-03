
# Report of Fiscal Income -------------------------------------------------
# Data was downloadad using Office Web App:
# https://ciatorg.sharepoint.com/:x:/s/cds/EZFFGWFtnhREhrIvkCY6a64BGzN95ylWcHNc0QrrIgQVgA?rtime=MYU_-gnv2Eg
# I use downloaded the data using the Save as command with the name "~/ciat_25_03_20201.xlsx" rigth in my
# Docuemnts folder called by "~"


library(tidyverse)
library(readxl)
library(usethis)

file <- "~/ciat_25_03_20201.xlsx"

sheets <- readxl::excel_sheets( file )

total_summaries_list <- sheets %>%
  stringr::str_subset("^Resumen") %>%
  purrr::map(~readxl::read_excel(path = file,sheet = .x))

resumen_total <- total_summaries_list[[1]] %>%
  dplyr::filter_all(.vars_predicate = any_vars(!is.na(.)))

# Use of an invoke_map:

dim(resumen_total) # Row column


dimensions <- invoke_map_dbl(.f = list(ncol,nrow),
                             x = resumen_total) # Same argument

# Long alternative:
most_empty <- purrr::invoke_map(
  .f = list(
    function(x){
      x %>%
        as.list %>%
        purrr::transpose(.) %>%
        purrr::map(is.na)
    },
    function(x){
      x %>%
        purrr::map(is.na)
    }
  ),
  x = resumen_total
) %>%
  purrr::map_depth(.depth = 2,sum) %>%
  purrr::map(unlist)


# Using apply to:

purrr::map(.x = c(1,2),
           ~apply(X = resumen_total,
                  MARGIN = .x,
                  FUN = function(x)sum(is.na(x))))


#' Title
#'
#' @param tabla
#' @param begin
#' @param end
#'
#' @return
#' @export
#'
#' @examples


function(tabla,begin,end){
  tabla
}




  seq_along()

  most_empty <- purrr::map2(.x= most_empty,
            .y = dimensions,
            ~{

              which(between(x = .x,left = 0.75*.y,right = .y))

            })

posiciones <- most_empty %>%
  purrr::map(seq_along) %>%
  purrr::map_depth(2,~c(.x,.x+1)) %>%
  purrr::cross()




extract <- function(tabla,
                    vacios,
                    limites){

  vacios <- vacios %>%
    map2(dim(tabla),~c(1,.x,.y) %>% unique)


  # browser()

  limites %>%
    purrr::map2_chr(.y =vacios,
             ~c(.y[.x[1]],
                .y[.x[2]]) %>%
               stringr::str_c(collapse = ":")

            ) %>%
    stringr::str_c(collapse = ",") %>%
    stringr::str_c("tabla[",.,"]")
  # %>%
  #   rlang::parse_expr() %>%
  #   eval

}


posiciones %>%
  map(extract,
      tabla = resumen_total,
      vacios = most_empty)

posibles <- purrr::map(posiciones,)


purrr::set_names(
  x = purrr::map(~ readxl::read_excel( file, sheet = .x))

    )

data_ciat <- readxl::read_excel(path = "~/ciat_25_03_20201.xlsx")



usethis::use_data(ciat_0920, overwrite = TRUE)
