#' Calculate the expected values from a bet
#'
#' A fair bet is the one which leads to a expected value equal to cero.
#' Someone who is unwilling to make a fair bet is risk averse. A person
#' who is indifferent about making a fair bet is risk neutral. A person who
#' is risk preferring will make a fair bet. (Perloff J. M., 2020)
#'
#' @param earnings value you get if you win the bet
#' @param losings value you loose if you do not win the bet
#' @param probabilities set of probabilities for the options of the bet
#' (the order should be as follows c(earnings,losings) )
#' @param endowment Initial endowment
#' @export
#' @return Expected value of a bet
#'
#' @examples
expected_value <- function(earnings = c(),
                           losings = c(),
                           probabilities = c(),
                           endowment = 0){

  if(sum(probabilities) != 1){
    stop("Probabilities set are not equal to 1")
  }else{

    if(endowment == 0){
      values0 <- c(earnings, losings)
    }else{
      values0 <- c(earnings, losings) %>%
        purrr::map_dbl(~ endowment +.x )
    }

    ev <- purrr::map2_dbl(values0,
                    probabilities,
                    ~.x*.y) %>%
      purrr::reduce(sum)

  }

  if(ev == 0){
    message("This is a fair bet!")
  }else if(ev > 0){
    message("This is a bet that favors you!")
  }else if(ev < 0){
    message("This is a bet that does not favors you!")
  }

  return(ev)

}

#' Calculate the expected utility from a bet
#'
#' @param earnings value you get if you win the bet
#' @param losings value you loose if you do not win the bet (always negative numbers)
#' @param probabilities set of probabilities for the options of the bet
#' (the order should be as follows c(earnings,losings) )
#' @param endowment Initial endowment
#' @param u_function Form of the utility function
#' @export
#' @return Expected utility from a bet
#'
#' @examples
expected_utility <- function(earnings = c(),
                             losings = c(),
                             probabilities = c(),
                             u_function = function(x){x},
                             endowment = 0){

  if(sum(probabilities) != 1){
    stop("Probabilities set are not equal to 1")
  }else{

    if(endowment == 0){
      values0 <- c(earnings, losings)
    }else{
      values0 <- c(earnings, losings) %>%
        purrr::map_dbl(~ endowment +.x )
    }

    values0 <- purrr::map_dbl(values0,
                              ~u_function(.x))



    eu <- purrr::map2_dbl(values0,
                          probabilities,
                          ~.x*.y) %>%
      purrr::reduce(sum)

  }


  return(eu)

}

# tibble(
#   p = seq(from = 0, to = 1, by = 0.01)
# ) %>%
#   mutate(q = 1 - p) %>%
#   mutate(expected = p*sqrt(10) + q*sqrt(70),
#          wealth = p*10 + q*70,
#          utility = sqrt(wealth)) %>%
#   ggplot() +
#   geom_line(aes(x = wealth,y = expected)) +
#   geom_line(aes(x = wealth,y = utility))

