#' Plot a demand, supply or both curves for a market
#'
#'
#' @param market result from `create_market` function
#' @param market_name A title for the analyzed market
#'
#' @importFrom magrittr %>%
#' @return
#'
#' Return a list with some elements_
#' * `curve` PLot of the analyzed market
#' * `market` all the elements behind the market
#' * `equilibrium` if at least one curve is a supply curve and at least one is a demand curve then
#' the equilibriums are calculated
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
      purrr::map(~purrr::safely(solve)(.x[[1]],.x[[2]])) %>%
      purrr::map("result") %>%
      purrr::keep(~!is.null(.x))

    equilibrium <- equilibrium %>%
      purrr::map(~ t(.x) %>%
                   tibble::as_tibble(.) %>%
                   dplyr::rename_all(~c("optim_p","optim_q"))
      ) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::filter_all(dplyr::all_vars(.>0)) %>%
      tibble::rowid_to_column(.) %>%
      dplyr::mutate(eq_nam =
                      stringr::str_c("E",rowid),
                    eq_val = stringr::str_c(
                      "E",rowid,
                      " (q*=",round(optim_q,2),
                      " ,p*=",round(optim_p,2),")"
                    ))
  }

# browser()

  if(nrow(equilibrium)< 1){

    opt <- options(show.error.messages=FALSE)

    on.exit(options(opt))

    message("Equilibrium is out of limits")

    stop()
  }


  market <- purrr::map2(market,
                        index,
                        ~append(x = .x,
                                values = list(index = .y))
  )

  price_q0 <- purrr::map(market,"coeficients") %>% purrr::reduce(max)

  aliases <-  purrr::pmap(
    market %>%
      purrr::transpose(.),
    function(name,
             funs,
             equation,
             coeficients,
             intercept,
             index){

      alias <- stringr::str_sub(name,1,1) %>%
        stringr::str_c(.,index,equation)


      return(alias)

    }
  )

  limits <-
    purrr::map(
      .x =  list(
        c(0,1),     # Y axis
        c(1,0)      # X axis
      ),
      function(vector){

        c("coeficients",
          "intercept") %>%
          purrr::map(
            ~market %>%
              purrr::map(.x)

          ) %>%
          purrr::transpose() %>%
          purrr::map(
            ~{
              values <- .x

              purrr::map2(
                values,
                list(
                  vector,
                  0
                ),
                ~rbind(.x,.y)
              ) %>%
                purrr::reduce(purrr::safely(solve))
            }
          ) %>%
          purrr::map("result") %>%
          purrr::keep(~!is.null(.x)) %>%
          purrr::map(as.vector) %>%
          unlist %>%
          max


      }
    ) %>%
    purrr::set_names(x = .,nm = c("price",
                                  "quatity"))



    # purrr::map_dfc(
    #   ~seq(from = 0,to = .x,length.out = 100)
    # )


  curves_df <- aliases %>%
    purrr::map(~{

      nam <- .x

      curve <- stringr::str_replace_all(nam,"^[DS][:digit:]{1,2}","")


      element <- purrr::keep(.x = market,
                             .p = ~.x$equation == curve)


      if(element[[1]]$name %>%
        stringr::str_detect("Demand")){
        tibble::tibble(
          price = seq(from = limits$price,to = 0,length.out = 100),
          quatity = seq(from = limits$price,to = 0,length.out = 100)
        )
      }

      browser()

      if(length(element)>0){

        fun <- element[[1]]$funs

        var <- formals(fun) %>% names

        limits %>%
          dplyr::mutate_at(.vars = var,
                           .funs = fun) %>%
          dplyr::filter(price <= max(limits$price),
                        quatity <= max(limits$quatity)) %>%
          dplyr::rename_at(c("price","quatity"),
                           ~c("Price","value")) %>%
          dplyr::mutate(variable = nam)

      }else{

        NULL

      }

    }) %>%
    purrr::reduce(dplyr::bind_rows)

  browser()

  curves_df <- curves_df %>%
    dplyr::mutate(variable = stringr::str_replace(variable,"\\+ \\-","- "),
                  variable = factor(variable))


  # browser()

  if(indicator == 0 |indicator == length(market)){
    plot <-
      curves_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = value,
                                      y = Price,
                                      color = variable),
                         size = 0.8) +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Quantity",
                    color = "Curve",
                    title = market_name) +
      ggplot2::scale_color_discrete(labels = purrr::map(levels(curves_df$variable),
                                                        latex2exp::TeX)) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

  }else{

    x.offset.country <- 2

    plot <-
      curves_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable),size = 0.8) +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Quantity",
                    color = "Curve",
                    title = stringr::str_to_upper(market_name),
                    subtitle = stringr::str_c(equilibrium$eq_val,collapse = "\n\n")) +
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
      ggplot2::scale_color_discrete(labels = purrr::map(c(levels(curves_df$variable),equilibrium$eq_val),
                                                        latex2exp::TeX) ) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      ggplot2::theme_light()

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
#' @param slope Ir's the parameter `b` and it's the rate at witch prices changes due to a change of the quantity demanded or supplied
#' @param perfect_e Perfectly __elastic__ demand (negative value) or supply (positive value)
#' @param perfect_i Perfectly __inelastic__ demand (negative value) or supply (positive value)
#' @return
#'
#' Element of class __"market_curves"__. The result is a list with the following elements, for each of
#' the curves:
#' __name:__ Wether it is a demand or supply curve
#' __funs:__ Functional form of the curve
#' __equation:__ Latex formula to print in
#' __coeficients:__ The value of the slopes and intercepts as matrix
#' __intercept:__ The value of the intercept (plotting porpuses)
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

    # browser()

    perfect_elastic <- purrr::map(perfect_e,
                                  ~{

                                    name <-  dplyr::case_when(
                                      .x >= 0 ~ "Supply curve",
                                      .x < 0 ~ "Demand curve"
                                    )

                                    result <- list(

                                      name = name,

                                      funs =  function(quatity){
                                        dplyr::case_when(
                                          .x >= 0 ~ .x,
                                          .x < 0 ~ -.x
                                        ) },

                                      equation = stringr::str_c("$: P = ",
                                                                abs(.x),"$"),

                                      coeficients= c(1, 0),

                                      intercept = dplyr::case_when(
                                        .x >= 0 ~ .x,
                                        .x < 0 ~ -.x
                                      ))

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
                                          dplyr::case_when(
                                            .x >= 0 ~ .x,
                                            .x < 0 ~ -.x
                                          ) },

                                        equation = stringr::str_c("$: Q = ",
                                                                  abs(.x),"$"),

                                        coeficients= c(0, 1),

                                        intercept = dplyr::case_when(
                                          .x >= 0 ~ .x,
                                          .x < 0 ~ -.x
                                        ))

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

