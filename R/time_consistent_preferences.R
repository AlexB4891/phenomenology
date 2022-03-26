
#' Exponential discounting of future utility
#'
#' @param consumption value of the utility function at time t
#' @param beta Discount rate relative to today
#' @param delta Discount rate fo eah period in the future
#' @param periods Number of periods in the future that we want to discount exponentially
#' @export
#' @return Exponentially discounted value of utility function
#'
#' @examples
exponential_discount <- function(consumption,
                                 beta,
                                 delta,
                                 periods){

  period_value <- consumption*(delta^periods)*beta

  return(period_value)

}

#' Model of time consistent preferences utility function
#'
#' @param c_t Value of the consumption today
#' @param c_t_1 Value of the future consumption
#' @param beta Discount rate relative to today
#' @param delta Discount rate fo eah period in the future
#' @param periods Number of periods in the future that we want to discount exponentially
#' @param start Equals to 0 if the consumption begin today, any other value means that the consumption
#' starts in the future
#' @export
#' @return Present value of utility after discounting the future utility in today terms
#'
#' @examples
lifetime_utility_function <- function(c_t,
                                      c_t_1,
                                      start,
                                      periods,
                                      beta,
                                      delta){


  if(start == 0){
    start0 <- start + 1
  }else{
    start0 <- start
  }

  future_consumption <- seq(from = start0,
                            to = start + periods) %>%
    purrr::map(~exponential_discount(consumption = c_t_1,
                                     periods = .x,
                                     beta = beta,
                                     delta = delta)
    ) %>%
    purrr::reduce(sum)

  if(start == 0){
    present_utility <- c_t + future_consumption
  }else{
    present_utility <- future_consumption
  }

  return(present_utility)

}
