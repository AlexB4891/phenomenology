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
#' # linear_curve(market = market,market_name = "Black market")
linear_curve <- function(market,
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

    d_curves <- market %>% purrr::keep(.p = ~ .x$name == "Demand curve")

    s_curves <- market %>% purrr::keep(.p = ~ .x$name == "Supply curve")


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

  price_q0 <- purrr::map(market,"coeficients") %>% purrr::reduce(max)

  curves_df <-  purrr::pmap(
    market %>%
      purrr::transpose(.),
    function(name,
             funs,
             equation,
             coeficients,
             intercept,
             index){

      alias <- stringr::str_sub(name,1,1) %>%
        stringr::str_c(.,index)

      lim_step <- list(
        sup_lim_y = dplyr::case_when(coeficients[2] != 0 ~ price_q0 * 1.25,
                              TRUE ~ intercept*1.25),

        sup_lim_x = dplyr::case_when(coeficients[2] != 0 ~ (intercept/((-1)*coeficients[2])) * 1.25,
                              TRUE ~ funs(price_q0) * 1.25)
      ) %>%
        purrr::map(~{

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

      if(coeficients[1] == 1){
        table <- table %>%
          dplyr::mutate(Price = funs(quantity)) %>%
          dplyr::rename_at("quantity",~stringr::str_c(alias,equation))
      }else if(coeficients[1] == 0){
        table <- table %>%
          dplyr::mutate(
            Price = 1,
            quantity = funs(Price)) %>%
          dplyr::rename_at("quantity",~stringr::str_c(alias,equation))
      }

      return(table)

    }
  ) %>%
    purrr::reduce(dplyr::full_join)

  # browser()

  curves_df <- curves_df %>%
    tidyr::gather(variable,value,-Price) %>%
    dplyr::mutate(variable = stringr::str_replace(variable,"\\+ \\-","- "),
                  variable = factor(variable))

  curves_df %>%
    split(.$variable) %>%
    purrr::map(~{
      .x %>%
        dplyr::mutate(
          Price = dplyr::if_else(Price = 1,
                                 NA_real_,
                                 Price)) %>%
        dplyr::mutate_at(.vars = c("Price",
                                   "value"),
                         .funs = list(~dplyr::case_when(
                           is.na(.) & (lag(.))
                         )))
    })


  # browser()

  if(indicator == 0 |indicator == length(market)){
    plot <-
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

    if(nrow(equilibrium) != 0){


      inelastic_curves <- market %>%
        purrr::map("coeficients") %>%
        purrr::map(~.x[1]) %>%
        purrr::reduce(c)


      if(any(inelastic_curves == 0)){
        plot <-
          curves_df %>%
          ggplot2::ggplot() +
          ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable)) +
          ggplot2::labs(x = "Quantity",
                        color = "Curve",
                        title = market_name)

      }





      plot +
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
        ggplot2::scale_color_discrete(labels = purrr::map(levels(curves_df$variable),
                                                          latex2exp::TeX)) +
        ggplot2::theme_light()

    }else{

      message("Curves intersect at negative values")

    }
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
                          slope,
                          perfect_e = NULL,
                          perfect_i = NULL){

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

                                 equation = stringr::str_c("$: P = ",
                                                           .x," + ",
                                                           .y,"Q_{",stringr::str_sub(name,1,1),"}$"),

                                 coeficients= c(1, -.y),

                                 intercept = .x)

                               return(result)

                             })

  }

  # Perfectly elastic demand or supply curves:

  if(!is.null(perfect_e)){

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

  if(!is.null(perfect_i)){

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


#' Fill NA in a sequence vector with constant step
#'
#' @param vector sequence vector with constant step with NA
#'
#' @return Filled vector
#' @export
#'
#' @examples
fill_sequence_vector <- function(vector){

  dplyr::lag(vector,1) - dplyr::lag(vector,2)

}

