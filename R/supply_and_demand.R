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

      index <- 1:length(resources)

      resources <- imap(resources,
                        ~append(x = .x,
                          values = list(index = .y))
                    )

      # browser()

      new_f <- function(name,
               funs,
               equation,
               coeficients,
               intercep,
               index){

        alias <- str_sub(name,1,1) %>% str_c(.,index)


        lim_step <- list(
          sup_lim_y = max(price_q0) * 1.25,

          sup_lim_x = (intercep/((-1)*coeficients[2])) * 1.25
        ) %>%
          map(~{

            val <- abs(.x)

            step <- val %/% 10

            step <- abs(step)

            step <- case_when(abs(step) < 1 ~ 0.1,
                              TRUE ~ 10^(nchar(step)))

            list(
              lim = val,
              step = step
            )
          })

        # browser()

        table <- tibble(
          quantity = seq(from = 0,
                         to = lim_step$sup_lim_x$lim,
                         by = lim_step$sup_lim_x$step)
        )

        table %>%
          mutate(Price = funs(quantity)) %>%
          rename_at("quantity",~alias)



      }

     curves_df <-  purrr::pmap(
        resources %>% transpose,
        new_f
       ) %>%
        reduce(full_join)

     curves_df %>%
       gather(variable,value,-Price) %>%
       ggplot() +
       geom_line(aes(x = value,y = Price,color = variable)) +
       theme_light() +
       labs(x = "Quantity",
            color = "Curve")
    }
  }

}


