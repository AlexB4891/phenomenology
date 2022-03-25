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

    d_curves <- market %>%
      purrr::keep(.p = ~ .x$name == "Demand curve")

    s_curves <- market %>%
      purrr::keep(.p = ~ .x$name == "Supply curve")

    market <- append(
      d_curves,
      s_curves
    )

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

  if(!is.null(equilibrium) & is.data.frame(equilibrium)){

    if(nrow(equilibrium) <1){
      opt <- options(show.error.messages=FALSE)

      on.exit(options(opt))

      message("Equilibrium is out of limits")

      stop()
    }
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


  # browser()

  length_market <- aliases %>%
    stringr::str_sub(start = 1,end = 1) %>%
    unique %>%
    length

  if(length_market == 2){

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
                                    "quatity")) %>%
      purrr::map_dfc(
        ~seq(from = 0,to = .x,length.out = 100)
      )

  }else if(length_market == 1){
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
      unlist

    if(sum(limits == 0) == 1){
      limits <- rep(sum(limits),2)

      limits <-
        purrr::set_names(x = limits*4,nm = c("price",
                                             "quatity")) %>%
        purrr::map_dfc(
          ~seq(from = 0,to = .x,length.out = 100)
        )
    }else{
      limits <-
        purrr::set_names(x = limits,nm = c("price",
                                           "quatity")) %>%
        purrr::map_dfc(
          ~seq(from = 0,to = .x,length.out = 100)
        )
    }



  }
  # browser()

  curves_df <- aliases %>%
    purrr::map(~{

      nam <- .x

      curve <- stringr::str_replace_all(nam,"^[DS][:digit:]{1,2}","")


      element <- purrr::keep(.x = market,
                             .p = ~.x$equation == curve)

      if(length(element)>0){

        fun <- element[[1]]$funs

        var <- formals(fun) %>% names

        vars <- c("price","quatity")

        objecive <- vars[vars != var]


        limits %>%
          dplyr::mutate(!!rlang::sym(objecive) :=
                          fun(!!rlang::sym(var))) %>%
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

  # browser()

  curves_df <- curves_df %>%
    dplyr::mutate(variable = stringr::str_replace(variable,"\\+ \\-","- "),
                  variable = factor(variable)) %>%
    dplyr::filter(value > 0,
                  Price > 0)


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
                                                        latex2exp::TeX))
    # +
    #   ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
    #   ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

  }else{

    x.offset.country <- 2

    plot <-
      curves_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = value,y = Price,color = variable),size = 0.8) +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Quantity",
                    color = "Curve",
                    title = market_name,
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
      # ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      # ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
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


#' Analize a market after a shock:
#'
#' @param market A simple market with supply and demand produced by `create_market`
#' function. The element is a list of class __market_curves__
#' @param curve String with the curve(s) name that get the shock.
#' Use "demand" or "supply"
#' @param shock_name A name from the shock. Default value: "Tax"
#' @param p_delta `p_delta` and `q_delta` works together with the `percent` argument.
#' If `percent` is `TRUE` then all prices are multiplied by the value declared. Otherwise
#' if `percent` is `FALSE` the curve moves in or out by a constant value.
#' @param q_delta p_delta `p_delta` and `q_delta` works together with the `percent` argument.
#' If `percent` is `TRUE` then all prices are multiplied by the value declared. Otherwise
#' if `percent` is `FALSE` the curve moves in or out by a constant value.
#' @param slope_change To wich value the slope of the curve should change.
#' @param percent  If the shock is a percentage (15% tax over supply) use `TRUE`,
#' else, a constant is summed to the curve.
#'
#' @return List with:
#'
#' * Original market plot
#' * Income and substitution effect plot
#' * Consumer and producer surplus before shock
#' * Consumer and producer surplus after shock
#' * Table with
#'     * Elasticities at equilibrium (before and after)
#'     * Surplus (before and after)
#'     * Income and substitution effect
#'
#' @export
#'
#' @examples
shock_over_market <- function( market,
                               curve,
                               shock_name = "Tax",
                               p_delta = 0.15,
                               q_delta = 0,
                               slope_change = 0,
                               percent = TRUE
){

  prior <- linear_curve(market,"Example")

  #   market <- create_market(price_q0 = c(0,50),
  #                           slope = c(10,-10))

  valid_market <- market %>%
    purrr::map("name") %>%
    unlist %>%
    table %>%
    tibble::enframe() %>%
    dplyr::filter(value>1)

  if(nrow(valid_market) > 0){
    msm <- valid_market %>%
      dplyr::pull(name) %>%
      stringr::str_c("Too much ",.,"s!")

    stop(msm)
  }

  if(length(curve) != length(shock_name)){
    stop("Not enough shock names!")
  }

  if(length(curve) != length(p_delta)){
    stop("Not enough price deltas!")
  }

  if(length(curve) != length(q_delta)){
    stop("Not enough quantity deltas!")
  }

  if(length(curve) != length(percent)){
    stop("Not enough measures of shock!")
  }

  if(length(curve) != length(slope_change)){
    stop("Not enough cahnges in slopes!")
  }

  shocks <- list(curve = curve,
                 shock_name = shock_name,
                 p_delta = p_delta,
                 q_delta = q_delta,
                 slope_change = slope_change,
                 percent = percent
  ) %>%
    purrr::pmap(
      function(curve,
               shock_name,
               p_delta,
               q_delta,
               slope_change,
               percent){

        objective_curve <- stringr::str_to_sentence(curve)

        objective_curve <- market %>%
          purrr::keep(.p = ~ stringr::str_detect(.x[["name"]],
                                                 objective_curve))



        slopes <- objective_curve %>%
          purrr::map("coeficients")


        slope_shock <- slopes[[1]][2]*(-1)


        intercept <- objective_curve %>%
          purrr::map("intercept")


        x_intercept <- solve(
          rbind(slopes[[1]],
                c(1,0)),
          c(intercept[[1]],0)
        )

        x_intercept <- x_intercept[2]


        if(percent){
          x_intercept <- x_intercept * (1 + q_delta)
        }else{
          x_intercept <- x_intercept  + q_delta
        }


        slope_shock <- slope_shock*(1 + slope_change)


        intercept_shock <- abs(x_intercept*slope_shock)

        if(percent){
          intercept_shock <- intercept_shock * (1 + p_delta)
        }else{
          intercept_shock <- intercept_shock + p_delta
        }

        # browser()

        create_market(price_q0 = intercept_shock,
                      slope = slope_shock)

      })   %>%
    purrr::flatten() %>%
    purrr::map(~append(.x,list(type = "shock")))


  market <- append(market,shocks)

  market <- structure(market,class = "market_curves")

  resources <- linear_curve(market = market,
                            market_name = "Example")


  browser()

  movement <- resources$cross_points %>%
    dplyr::select(rowid,matches("optim")) %>%
    tidyr::gather(key = "value",value = "points",-rowid) %>%
    dplyr::mutate(value = stringr::str_c(value,rowid),
                  rowid = 1) %>%
    tidyr::spread(key = value,value = points)


  # movement %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_segment(
  #     ggplot2::aes(x = optim_q1,
  #                  xend = optim_q2,
  #                  y = optim_p1,
  #                  yend = optim_p2)
  #   )


  # png(filename = "~/market_example.png",
  #     res = 250,
  #     width = 9,
  #     height = 6,
  #     units = "in")

  fixed_q <- c("coeficients","intercept") %>%
    purrr::map(~{
      resources$curves[[3]] %>% ### Define shock curve
        purrr::pluck(.x)
    }) %>%
    purrr::map2(
      .y = list(
        c(1,0),
        c(movement$optim_p1)
      ),
      ~rbind(.x,.y)
    ) %>%
    purrr::reduce(solve) %>%
    as.vector

  effects <- tibble::tibble(
    optim_q1 = fixed_q[2],
    optim_q2 = movement$optim_q2,
    optim_q3 = movement$optim_q1
  )


  shock <- resources$market +

    ggplot2::geom_segment(
      data = movement,
      ggplot2::aes(x = optim_q1,
                   xend = optim_q2,
                   y = optim_p1,
                   yend = optim_p2),
      arrow = ggplot2::arrow(
        length = ggplot2::unit(0.5,"cm")
      ),
      size = 1
    ) +
    ggplot2::geom_text(
      data = movement,
      ggplot2::aes(
        x = - optim_q1/5,
        y = (optim_p1 + optim_p2 )/2,
        label = shock_name
      ),
      size = 4,
      color = "#7a7472"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = 0,
        ymin = min(Price),
        ymax = max(Price)
      ),
      fill = 'transparent'
    ) +
    ggplot2::geom_segment(
      data = movement,
      ggplot2::aes(x = -optim_q1/10,
                   xend = -optim_q1/10,
                   y = optim_p1,
                   yend = optim_p2),
      arrow = ggplot2::arrow(ends = "both",
                             length = ggplot2::unit(0.2,"cm")
      ),
      size = 0.5
    ) +
    ggpubr::geom_bracket(
      xmin = effects$optim_q3,
      xmax = effects$optim_q1,
      label = stringr::str_c("Income effect","\n"," \u2190"),
      y.position = (movement$optim_p1)*0.8
    ) +
    ggpubr::geom_bracket(
      xmin = effects$optim_q2,
      xmax = effects$optim_q1,
      label = stringr::str_c("Substitution effect","\n"," \u2192"),
      y.position = (movement$optim_p1)*0.4
    ) +
    ggplot2::theme_minimal()



  ggpubr::ggarrange(
    prior$market +
      ggplot2::theme_minimal(),
    shock,ncol = 2,
    nrow = 1,align = "h"
  )

}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
annotate_surplus <- function(...){





  market <- create_market(price_q0 = c(0,50,200),
                          slope = c(10,10,-10))


  resources <- linear_curve(market = market,
                            market_name = "Example")


  movement <- resources$cross_points %>%
    dplyr::select(rowid,matches("optim")) %>%
    tidyr::gather(key = "value",value = "points",-rowid) %>%
    dplyr::mutate(value = stringr::str_c(value,rowid),
                  rowid = 1) %>%
    tidyr::spread(key = value,value = points)



  fixed_q <- c("coeficients","intercept") %>%
    purrr::map(~{
      resources$curves[[2]] %>%
        purrr::pluck(.x)
    }) %>%
    purrr::map2(
      .y = list(
        c(1,0),
        c(movement$optim_p1)
      ),
      ~rbind(.x,.y)
    ) %>%
    purrr::reduce(solve) %>%
    as.vector

  effects <- tibble::tibble(
    optim_q1 = fixed_q[2],
    optim_q2 = movement$optim_q2,
    optim_q3 = movement$optim_q1
  )

  previos <-  resources$market +
    ggplot2::geom_ribbon(
      data = resources$market$data %>%
        dplyr::filter(dplyr::between(value,0,effects$optim_q3),
                      dplyr::between(Price,
                                     movement$optim_p1,
                                     resources$curves[[3]]$intercept),
                      stringr::str_detect(variable,"D")),
      ggplot2::aes(
        x = value,
        y = Price,
        xmin = 0,
        xmax = effects$optim_q3,
        ymin = movement$optim_p1,
        ymax = Price
      ),
      alpha = 0.2,
      fill = "Blue"
    ) +
    ggplot2::geom_ribbon(
      data = resources$market$data %>%
        dplyr::filter(dplyr::between(value,0,effects$optim_q3),
                      dplyr::between(Price,
                                     0,
                                     movement$optim_p1),
                      stringr::str_detect(variable,"S.*P = 0")),
      ggplot2::aes(
        x = value,
        y = Price,
        xmin = 0,
        xmax = effects$optim_q3,
        ymin = Price,
        ymax = movement$optim_p1
      ),
      alpha = 0.2,
      fill = "red"
    )



  after <- resources$market +
    ggplot2::geom_ribbon(
      data = resources$market$data %>%
        dplyr::filter(dplyr::between(value,0,effects$optim_q2),
                      dplyr::between(Price,
                                     movement$optim_p2,
                                     resources$curves[[3]]$intercept),
                      stringr::str_detect(variable,"D")),
      ggplot2::aes(
        x = value,
        y = Price,
        xmin = 0,
        xmax = effects$optim_q2,
        ymin = movement$optim_p2,
        ymax = Price
      ),
      alpha = 0.2,
      fill = "Blue"
    ) +
    ggplot2::geom_ribbon(
      data = resources$market$data %>%
        dplyr::filter(dplyr::between(value,0,effects$optim_q2),
                      dplyr::between(Price,
                                     0,
                                     movement$optim_p2),
                      stringr::str_detect(variable,"S.*P = 50")),
      ggplot2::aes(
        x = value,
        y = Price,
        xmin = 0,
        xmax = effects$optim_q3,
        ymin = Price,
        ymax = movement$optim_p2
      ),
      alpha = 0.2,
      fill = "red"
    ) +
    ggplot2::geom_ribbon(
      data = resources$market$data %>%
        dplyr::filter(dplyr::between(value,effects$optim_q1,effects$optim_q3),
                      dplyr::between(Price,
                                     movement$optim_p1,
                                     movement$optim_p2),
                      stringr::str_detect(variable,"S.*P = 50|D")),
      ggplot2::aes(
        x = value,
        y = Price,
        xmin = effects$optim_q1,
        xmax = effects$optim_q3,
        ymin = movement$optim_p1,
        ymax = Price
      ),
      alpha = 0.2,
      fill = "green"
    )




  png(filename = "~/market_example.png",
      res = 250,
      width = 19,
      height = 13,
      units = "in")


  ggpubr::ggarrange(
    resources$market,shock,previos,after,ncol = 2,
    nrow = 2
  )


  dev.off()
}

