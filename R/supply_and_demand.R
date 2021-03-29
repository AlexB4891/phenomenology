#' Plot a demand, supply or both curves for a market
#'
#'
#' @param market result from `create_market` function
#' @param market_name A title for the analized market
#'
#' @importFrom magrittr %>%
#' @return
#'
#' Return a list with some elements_
#' * `curve` PLot of the analized market
#' * `market` all the elements behind the market
#' * `equilibrium` if at least one curve is a supply curve and at leat one is a demand curve then the equilibriums are calculated
#' @export
#' @examples
#'
#' # market <- create_market(price_q0 = c(100,200),slope = c(-1,1))
#' # supply_and_demand(market = market,market_name = "Black market")
supply_and_demand <- function(market,
                         market_name = NULL
){


  stopifnot(class(market) == "market_curves")

  # Determine if all curves are of the same kind

  indicator <- market %>%
    purrr::map_chr("name") %>%
    purrr::map(stringr::str_detect,"Demand") %>%
    purrr::reduce(sum)


  if(indicator == 0 |indicator == length(market)){

    # No equilibrium to be found

    index <- 1:length(market)

    equilibrium <- NULL

  }else{

    # Calculate the equilibrium for that market

    index <- c("Demand","Supply") %>%
      purrr::map(~market %>%
                   purrr::map_chr("name") %>%
                   stringr::str_subset(pattern = .x)) %>%
      purrr::map(~1:length(.x)) %>%
      purrr::reduce(c)

    d_curves <- market %>%
      purrr::keep(.p = ~ .x$name == "Demand curve")

    s_curves <- market %>%
      purrr::keep(.p = ~ .x$name == "Supply curve")


    equilibrium <- c("coeficients","intercept") %>%
      purrr::map(~{

        attrib <- .x

        insumos <-

          list(D = d_curves,
               S = s_curves) %>%
          purrr::map(~{

            purrr::map(.x,attrib)

          })

        purrr::cross2(insumos$D,insumos$S) %>%
          purrr::map(purrr::reduce,rbind)

      }) %>%
      purrr::transpose(.) %>%
      purrr::map(~solve(.x[[1]],.x[[2]]))

    equilibrium <- equilibrium %>%
      purrr::map(~ t(.x) %>%
                   tibble::as_tibble(.) %>%
                   dplyr::rename_all(~c("optim_p","optim_q"))
      ) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::filter_all(dplyr::all_vars(.>0)) %>%
      tibble::rowid_to_column(.) %>%
      dplyr::mutate(eq_nam =
                      stringr::str_c("E",rowid,
                                     " (q*=",round(optim_q,2),
                                     " ,p*=",round(optim_p,2),")"))
  }




  market <- purrr::map2(market,
                        index,
                        ~append(x = .x,
                                values = list(index = .y))
  )

  # browser()

  price_q0 <- purrr::map(market,"intercept") %>%
    purrr::reduce(max)

  curves_df <-  purrr::pmap(
    market %>%
      purrr::transpose(.),
    function(name,
             funs,
             funs_q,
             equation,
             coeficients,
             intercept,
             index){

      alias <- stringr::str_sub(name,1,1) %>%
        stringr::str_c(.,index)

      lim_step <- list(
        sup_lim_y = funs(funs_q(0)),

        sup_lim_x = funs_q(0)
      ) %>%
        purrr::map(~{

          val <- abs(.x)

          step <- 0.01

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

      if(coeficients[1] == 1){
        table <- table %>%
          dplyr::mutate(Price = funs(quantity)) %>%
          dplyr::rename_at("quantity",~stringr::str_c(alias,equation))
      }else if(coeficients[1] == 0){
        table <- table %>%
          dplyr::mutate(
            Price = quantity,
            quantity = funs(Price)) %>%
          dplyr::rename_at("quantity",~stringr::str_c(alias,equation))
      }

      return(table)

    }
  ) %>%
    purrr::reduce(dplyr::full_join)



  curves_df <- curves_df %>%
    tidyr::gather(variable,value,-Price) %>%
    dplyr::mutate(variable = stringr::str_replace(variable,"\\+ \\-","- "),
                  variable = factor(variable))

  curves_df <- curves_df %>%
    split(.$variable) %>%
    map2(market,
         ~{
           .x %>%
             mutate(value = if_else(is.na(value),.y$funs_q(Price),value))
         }) %>%
    reduce(rbind) %>%
    filter(value>=0)

  value_max <- curves_df %>%
    split(.$variable) %>%
    map_dbl(~.x %>% pull(value) %>% max)
  # %>%
  #   min

  demand_v <- str_detect(names(value_max),"D[:digit:].*")

  if(length(demand_v)>0){
    value_max <- value_max[demand_v]
  }else{
    value_max <- min(value_max)
  }
  # browser()




  if(indicator == 0 |indicator == length(market)){
    market_plot <-
      curves_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable)) +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Quantity",
                    color = "Curve",
                    title = market_name) +
      ggplot2::scale_color_discrete(labels = purrr::map(levels(curves_df$variable),
                                                        latex2exp::TeX))

  }else{

    # browser()

    if(nrow(equilibrium) != 0){

#
#       inelastic_curves <- market %>%
#         purrr::map("coeficients") %>%
#         purrr::map(~.x[1]) %>%
#         purrr::reduce(c)


      # if(any(inelastic_curves == 0)){
        market_plot <-
          curves_df %>%
          ggplot2::ggplot() +
          ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable)) +
          ggplot2::labs(x = "Quantity",
                        color = "Curve",
                        title = market_name)

      # }else{C


      market_plot <- market_plot +
        ggplot2::geom_segment(data = equilibrium,
                              ggplot2::aes(y = optim_p,yend =optim_p, x = 0,xend = optim_q),
                              linetype = "dashed") +
        ggplot2::geom_segment(data = equilibrium,
                              ggplot2::aes(y = 0,yend =optim_p, x = optim_q,xend = optim_q),
                              linetype = "dashed") +
        ggplot2::geom_point(data = equilibrium,
                            ggplot2::aes(x = optim_q,y=optim_p)) +
        ggplot2::geom_text(data = equilibrium,
                           ggplot2::aes(x = optim_q,y=optim_p,label =eq_nam,vjust = -1)) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0)) +
        ggplot2::xlim(0,value_max)+
        ggplot2::scale_color_discrete(labels = purrr::map(levels(curves_df$variable),
                                                          latex2exp::TeX)) +
        ggplot2::theme_light()

    }else{
      market_plot <- NULL
      message("Curves intersect at negative values")

    }
  }

  market <-
    list(curves = market,
         market = market_plot ,
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
#' @param slope It's the parameter `b` and it's the rate at wich prices changes due to a change of the quantity demanded or supplied#'
#' @param perfect_e Extreme cases for perfectly elastic demand (-) or supply(+)
#' @param perfect_i Extreme cases for perfectly inelastic demand (-) or supply(+)
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
                          slope,
                          perfect_e = NA_real_,
                          perfect_i = NA_real_){


  # browser()
  # e_ind <- switch (perfect_e,
  #   NA = NULL
  # )
  #
  # i_ind <- switch (perfect_i,
  #                  NA = NULL
  # )

  # For finite values of the slope:

  if(length(price_q0) > length(slope)) {

    stop("Not enough slopes")

  }else if(length(slope) > length(price_q0)){

    stop("Not enough prices")

  }else if(length(slope) == length(price_q0)){

    resources <- purrr::map2(price_q0,
                             slope,
                             ~{

                               name <-  dplyr::case_when(
                                 .y >= 0 ~ "Supply curve",
                                 .y < 0 ~ "Demand curve"
                               )

                               result <- list(

                                 name = name,

                                 funs =  function(quatity){.x + .y*quatity},

                                 funs_q =  function(price){  (1/.y)*price - (.x/.y)},

                                 equation = stringr::str_c("$: P = ",
                                                           .x," + ",
                                                           .y,"Q_{",stringr::str_sub(name,1,1),"}$"),

                                 coeficients= c(1, -.y),

                                 intercept = .x)

                               return(result)

                             })

  }

  # Perfectly elastic demand or supply curves:

  el_ind <- sum(!is.null(perfect_e), !is.na(perfect_e))

  if(el_ind == 2){

    perfect_elastic <- purrr::map(perfect_e,
                                  ~{

                                    name <-  dplyr::case_when(
                                      .x >= 0 ~ "Supply curve",
                                      .x < 0 ~ "Demand curve"
                                    )

                                    result <- list(

                                      name = name,

                                      funs =  function(quatity){
                                        abs(.x) },

                                      equation = stringr::str_c("$: P = ",
                                                                abs(.x),"$"),

                                      coeficients= c(1, 0),

                                      intercept = abs(.x)
                                      )

                                    return(result)

                                  })
  }else{
    perfect_elastic <- NULL
  }

  # Perfectly inelastic demand or supply curves:

  inel_ind <- sum(!is.null(perfect_i), !is.na(perfect_i))

  if(inel_ind == 2){

    perfect_inelastic <- purrr::map(perfect_i,
                                    ~{

                                      name <-  dplyr::case_when(
                                        .x >= 0 ~ "Supply curve",
                                        .x < 0 ~ "Demand curve"
                                      )

                                      result <- list(

                                        name = name,

                                        funs =  function(price){
                                          abs(.x)
                                           },

                                        equation = stringr::str_c("$: Q = ",
                                                                  abs(.x),"$"),

                                        coeficients= c(0, 1),

                                        intercept = abs(.x))

                                      return(result)

                                    })
  }else{
    perfect_inelastic <- NULL
  }

  resources <- list(
    resources,
    perfect_elastic,
    perfect_inelastic
  ) %>%
    purrr::keep(~!is.null(.x)) %>%
    purrr::flatten(.)


  resources <- structure(resources,class = "market_curves")
  return(resources)
}




