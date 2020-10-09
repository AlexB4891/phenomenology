#' Plot a demand, supply or both curves for a market
#'
#' Consider the next linear expressiion for a supply (demand is the same especification but with a negative b) curve:
#' $$P = a + b*q$$
#' Then:
#'
#' @param price_q0 It's the parameter `a` and for both, demanded and/or supplied quantities are equal to 0
#' @param slope Ir's the parameter `b` and it's the rate at wich prices changes due to a change of the quantity demanded or supplied
#'
#' @return
#' @export
#'
#' @examples
linear_curve <- function(price_q0 = numeric(),
                         slope = numeric()){

  if(length(price_q0) > length(slope)) {

    stop("Not enough slopes")

  }else if(length(slope) > length(price_q0)){

    stop("Not enough prices")

  }else if(length(slope) == length(price_q0)){

    resources <- purrr::map2(price_q0,
                             slope,
                             ~{

                               name <-  case_when(
                                 .y > 0 ~ "Supply curve",
                                 .y < 0 ~ "Demand curve"
                               )

                               result <- list(
                                 name = name,

                                 funs =  function(quatity){.x + .y*quatity},

                                 equation = str_c(": $P = ",
                                                  .x," + ",
                                                  .y,"Q_{",str_sub(name,1,1),"}$"),

                                 coeficients= c(1, -.y),

                                 intercep = .x)

                               return(result)

                             })

    indicador <- resources %>%
      map_chr("name") %>%
      map(str_detect,"Demand") %>%
      reduce(sum)

    if(indicador == 0 |indicador == length(resources)){

      names <- resources %>%
        map_chr("name")

      purrr::imap(
        names,
        ~{
          alias <- str_sub(.x,1,1) %>% str_c(.,.y)

          table <- tibble(
            quantity = seq(from = 0,to = max(price_q0),by = 10)
          )


        }
       )
    }
  }

}


lala <- function(table,
                 var_fun){

  name <- exp(var_fun$name)

  funs <- exp(var_fun$funs)

  table %>%
    mutate({{name}} := funs(quatity))

}

# lala("D1")
