# set.seed(250)
# set.seed(2022-08-06)
library(ggplot2)
library(magrittr)
library(tidyverse)

farm_model <- function(n_cows = 100, replacement_age = NA, 
                       end_time = 5 * 365) {
  # n_cows <- 100
  if (is.na(replacement_age)) {
    replacement_age_distr <- function(n_cows) 
      rnorm(n_cows, mean = 1642, sd = 50) %>% 
      round()
  } else {
    #TODO: check that it is numeric(1)
    replacement_age_distr <- function(n_cows) {
      rep.int(replacement_age, n_cows)
    }
  }
  # Create the farm:
  farm <- tibble(id = seq_len(n_cows),
                 age = round(runif(n_cows, 730, 1642)),
                 replacement_age = replacement_age_distr(n_cows)
  )
  
  # We want to simulate 5 years:
  # end_time <- 5 * 365
  
  # Collect the mean age of the cows in herd over time:
  age_collect <- numeric(end_time)
  times <- seq_len(end_time)
  # total_infected <- 0
  # farm$infected[1] <- TRUE
  
  for (t in times)
  {
    # If cows reach the age of 1642,
    # they are replaced with a new cow that is 2 years old in DIM = 1:
    is_replaced <- which(farm$age >= farm$replacement_age);
    if (length(is_replaced) > 0) {
      farm$age[is_replaced] <- 730
      farm$replacement_age[is_replaced] <- replacement_age_distr(length(is_replaced))
    }
    
    # Add one day to the age of all the animals, for each simulated:
    farm$age <- farm$age + 1
    
    # Save the daily mean age of all cows:
    age_collect[t] <- mean(farm$age)
  }
  list(age_collect = age_collect, 
       times = times,
       replacement_age = replacement_age)
}
farm_model(end_time = 10000, replacement_age = NA) %>% 
  
  as_tibble() %>% 
  ggplot() + 
  aes(times, age_collect) + 
  geom_line()+
  
  scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean()
#'

# plot(age_collect, type = "l", lwd = 2)
# points(age_collect[age_collect==1642])
# age_collect %>%
#   enframe(value = "age") %>%
#   ggplot() +
#   aes(name, age) +
#   # geom_step() +
#   geom_line() +
#   geom_point(data = . %>% filter(age == 1642 - 1)) +
#   ggpubr::theme_pubclean()

#' The mean age is never close to the threshold, as we are not recording each
#' individual cow.

plot.new()
age_collect <- farm_model(n_cows = 100)$age_collect
plot(age_collect, type = "l", lwd = 2, col = 1)
quantile(age_collect)
age_collect <- farm_model(n_cows = 500)$age_collect
lines(age_collect, type = "l", lwd = 2, col = 2)
quantile(age_collect)
age_collect <- farm_model(n_cows = 5e3)$age_collect
lines(age_collect, type = "l", lwd = 2, col = 3)
quantile(age_collect)

#' The variation decreases with increasing number of cows.
#' 
# farm_model(end_time = 10000, replacement_age = 1600) %>% 
farm_model(end_time = 10000, replacement_age = NA) %>% 
  
  as_tibble() %>% 
  ggplot() + 
  aes(times, age_collect) + 
  geom_line()+
  
  scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean()
#'
#'
#'  
tibble(rep_id = seq_len(100)) %>% 
  rowwise() %>% 
  mutate(results = farm_model() %>% as_tibble()) %>% 
  ungroup() ->
  results_df

results_df %>% 
  slice(1) %>% 
  unnest_wider(results) %>% 
  unnest() %>% 
  ggplot() +
  aes(times, age_collect, group = rep_id) +
  geom_line() +
  # geom_smooth(aes(group = NA)) + 
  NULL
