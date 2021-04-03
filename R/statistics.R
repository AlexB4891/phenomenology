
#' Draw a fair dice __sample_n_t__ times
#'   Theoretical mean: 3.5
#'   Theoretical mean: 1.71
#'
#' @param sample_n Draws
#'
#' @importFrom magrittr %>%
#'
#' @return Return the standarized value of the mean draws.
#' @export
#'
#' @examples
dice_mean_mod <- function(sample_n){

  set.seed(1984)

  mean_0 <- sample(1:6,
                   size = sample_n,
                   replace = T) %>%
    mean

  z <- (sqrt(sample_n)*(mean_0 - 3.5))/1.71

  z
}



