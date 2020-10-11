#' Plot a demand, supply or both curves for a market
#'
#' Consider the next linear expressiion for a supply (demand is the same especification but with a negative b) curve:
#' $$P = a + b*q$$
#' Then:
#'
#' @param price_q0 It's the parameter `a` and for both, demanded and/or supplied quantities are equal to 0
#' @param slope Ir's the parameter `b` and it's the rate at wich prices changes due to a change of the quantity demanded or supplied
#'
#' @importFrom magrittr %>%
#' @return
#'
#' Return a list with some elements_
#' * `market_plot` gives a ggplot2 graphic showin the market defined by the parameters
#' * `cuts` if there are at least two curves defined, one for the demand curve and
#' one for the supply returns a list with the prices and quantities in the each curve intersection
#'
#' @export
#' @examples

linear_curve <- function(market,
                         market_name = NULL
                         ){




  stopifnot(class(market) == "market_curves")


  indicador <- market %>%
    purrr::map_chr("name") %>%
    purrr::map(str_detect,"Demand") %>%
    purrr::reduce(sum)


  if(indicador == 0 |indicador == length(market)){

    index <- 1:length(market)

    equilibrium <- NULL

  }else{


    index <- c("Demand","Supply") %>%
      purrr::map(~market %>%
            purrr::map_chr("name") %>%
            stringr::str_subset(pattern = .x)) %>%
      purrr::map(~1:length(.x)) %>%
      purrr::reduce(c)

    d_curves <- market %>% keep(.p = ~ .x$name == "Demand curve")

    s_curves <- market %>% keep(.p = ~ .x$name == "Supply curve")


    equilibrium <- c("coeficients","intercept") %>%
      map(~{

        attrib <- .x

        insumos <-

          list(D = d_curves,
               S = s_curves) %>%
          purrr::map(~{

            purrr::map(.x,attrib)

          })

        purrr::cross2(insumos$D,insumos$S) %>%
          purrr::map(reduce,rbind)

      }) %>%
      purrr::transpose(.) %>%
      purrr::map(~solve(.x[[1]],.x[[2]]))

    equilibrium <- equilibrium %>%
      map(~t(.x) %>% as_tibble %>%
            rename_all(~c("optim_p","optim_q"))
      ) %>%
      reduce(bind_rows) %>%
      filter_all(all_vars(.>0)) %>%
      rowid_to_column %>%
      mutate(eq_nam = str_c("E",rowid," (q*=",round(optim_q,2)," ,p*=",round(optim_p,2),")"))
  }




  market <- purrr::map2(market,
                        index,
                        ~append(x = .x,
                                values = list(index = .y))
  )

  price_q0 <- purrr::map(market,"coeficients") %>% reduce(max)

  curves_df <-  purrr::pmap(
    market %>%
      purrr::transpose(.),
    function(name,
             funs,
             equation,
             coeficients,
             intercept,
             index){

      alias <- stringr::str_sub(name,1,1) %>% stringr::str_c(.,index)


      lim_step <- list(
        sup_lim_y = price_q0 * 1.25,

        sup_lim_x = (intercept/((-1)*coeficients[2])) * 1.25
      ) %>%
        map(~{

          val <- abs(.x)

          step <- val %/% 10

          step <- abs(step)

          step <- dplyr::case_when(abs(step) < 1 ~ 0.1,
                            TRUE ~ 10^(nchar(step)))

          list(
            lim = val,
            step = step
          )
        })

      # browser()

      table <- tibble::tibble(
        quantity = seq(from = 0,
                       to = lim_step$sup_lim_x$lim,
                       by = lim_step$sup_lim_x$step)
      )

      table %>%
        dplyr::mutate(Price = funs(quantity)) %>%
        dplyr::rename_at("quantity",~str_c(alias,equation))



    }
  ) %>%
    purrr::reduce(dplyr::full_join)

  curves_df <- curves_df %>%
    tidyr::gather(variable,value,-Price) %>%
    mutate(variable = str_replace(variable,"\\+ \\-","- "),
           variable = factor(variable))

  if(indicador == 0 |indicador == length(market)){
    plot <-
      curves_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable)) +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Quantity",
                    color = "Curve",
                    title = market_name) +
      scale_color_discrete(label = map(levels(curves_df$variable),latex2exp::TeX)) +
      theme_light()

  }else{
    plot <-
      curves_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable)) +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Quantity",
                    color = "Curve",
                    title = market_name) +
      geom_segment(data = equilibrium,
                   aes(y = optim_p,yend =optim_p, x = 0,xend = optim_q),linetype = "dashed") +
      geom_segment(data = equilibrium,
                   aes(y = 0,yend =optim_p, x = optim_q,xend = optim_q),linetype = "dashed") +
      geom_point(data = equilibrium,
                 aes(x = optim_q,y=optim_p)) +
      geom_text(data = equilibrium,
                aes(x = optim_q,y=optim_p,label =eq_nam,vjust = -1)) +
      geom_hline(aes(yintercept = 0)) +
      geom_vline(aes(xintercept = 0)) +
      scale_color_discrete(label = map(levels(curves_df$variable),latex2exp::TeX)) +
      theme_light()

  }

   market <-
    list(curves = market,
         market = plot ,
         cross_points = equilibrium)

   return(market)

}


#' Create a series of elements to analize market's dinamic
#'
#' Given a set of prices and quantities create all the market needed to
#' analize a market using linear curves like:
#'
#' \deqn{$$P = a + bx$$}
#'
#' @param price_q0 It's the parameter `a` and for both, demanded and/or supplied quantities are equal to 0
#' @param slope Ir's the parameter `b` and it's the rate at wich prices changes due to a change of the quantity demanded or supplied#'
#' @return
#'
#' A list with all the market needed to construct our market
#'
#' @export
#'
#' @examples
#'
#' # Make a demand curve:
#'
#' demand <- create_market(price_q0 = 200,slope = -4)
#'
#' # Make a supply curve:
#'
#' supply <- create_market(price_q0 = 100,slope = 2)
#'
#' # Find the equilibrium:
#'
#' equiilibrium <- create_market(price_q0 = c(200,100),slope = c(-4,2))
#'
#' # Many demands and one supply:
#'
#' equiilibrium <- create_market(price_q0 = c(200,100,150),slope = c(-4,2,3))
#'
#' # Works the same way for many supply curves
create_market <- function(price_q0,
                          slope){
  if(length(price_q0) > length(slope)) {

    stop("Not enough slopes")

  }else if(length(slope) > length(price_q0)){

    stop("Not enough prices")

  }else if(length(slope) == length(price_q0)){

    resources <- purrr::map2(price_q0,
                             slope,
                             ~{

                               name <-  dplyr::case_when(
                                 .y > 0 ~ "Supply curve",
                                 .y < 0 ~ "Demand curve"
                               )

                               result <- list(

                                 name = name,

                                 funs =  function(quatity){.x + .y*quatity},

                                 equation = stringr::str_c("$: P = ",
                                                           .x," + ",
                                                           .y,"Q_{",stringr::str_sub(name,1,1),"}$"),

                                 coeficients= c(1, -.y),

                                 intercept = .x)

                               return(result)

                             })

    structure(resources,class = "market_curves")
  }
}

