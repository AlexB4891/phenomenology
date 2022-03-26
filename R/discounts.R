# library(tidyverse)
#
#
# tibble(year = seq(1,10,0.00001)) %>%
#   mutate(payment = if_else(year == 0,-100,
#                            (100*sqrt(year))/(1.05^year)
#                            ),
#          payment_continuos = if_else(year == 0,-100,
#                            (100*sqrt(year))/(exp(0.05*year))
#          ),
#          value = (100*sqrt(year))) %>%
#   filter(payment_continuos == max(payment_continuos))
# # %>% View
