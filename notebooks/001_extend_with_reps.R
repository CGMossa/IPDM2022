# set.seed(250)
# set.seed(2022-08-06)
library(ggplot2)
library(magrittr)
library(tidyverse)

farm_model <- function(n_cows = 100, replacement_age = 1642) {
  # n_cows <- 100
  
  # Create the farm:
  farm <- data.frame(id = seq_len(n_cows),
                     age = round(runif(n_cows, 730, 1642)))
  
  # We want to simulate 5 years:
  end_time <- 5 * 365
  
  # Collect the mean age of the cows in herd over time:
  age_collect <- numeric(end_time)
  times <- seq_len(end_time)
  
  for (t in times)
  {
    # If cows reach the age of 1642,
    # they are replaced with a new cow that is 2 years old in DIM = 1:
    farm$age[farm$age >= replacement_age] <- 730
    
    # Add one day to the age of all the animals, for each simulated:
    farm$age <- farm$age + 1
    
    # Save the daily mean age of all cows:
    age_collect[t] <- mean(farm$age)
  }
  list(age_collect = age_collect, 
       times = times)
}

reps_farm_model <- function(repetitions = 100, 
                            n_cows = 100, replacement_age = 1642) {
  # doesn't work.
  # replicate(repetitions, farm_model(...), simplify = FALSE)
  replicate(repetitions, farm_model(n_cows, replacement_age), simplify = FALSE)
}

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

tibble(
  # replacement_age = c(800, 1000, 1200)
  replacement_age = seq.default(720 + 1, 1642, length.out = 10)
) %>% 
  # don't set the above to something greater than the replaced cow age, 
  # as in then we'd replace younger cows with much older ones
  mutate(stopifnot(all(replacement_age > 720))) %>% 
  rowwise() %>% 
  mutate(results = farm_model(replacement_age = replacement_age) %>% list()) %>%
  ungroup() %>%
  unnest_wider(results) %>% 
  unnest() %>% 
  glimpse() %>% 
  ggplot() + 
  aes(times, age_collect, group = replacement_age) + 
  geom_line(aes(color = replacement_age))+
  
  scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean()


tibble(
  # replacement_age = c(800, 1000, 1200)
  replacement_age = seq.default(720 + 1, 1642, length.out = 10)
) %>% 
  # don't set the above to something greater than the replaced cow age, 
  # as in then we'd replace younger cows with much older ones
  mutate(stopifnot(all(replacement_age > 720))) %>% 
  rowwise() %>% 
  mutate(results = reps_farm_model(repetitions = 25, 
                                   replacement_age = replacement_age) %>% 
           
           enframe(name = "repetition", value = "results") %>% 
           list()) %>%
  ungroup() %>%
  unnest_wider(results) %>% 
  unnest() %>% 
  unnest_wider(results) %>% 
  unnest() %>% 
  glimpse() %>% 
  ggplot() + 
  aes(times, age_collect, group = interaction(repetition, replacement_age)) + 
  # geom_line(aes(color = repetition))+
  geom_line(aes(color = replacement_age)) +
  # geom_smooth(
  #   aes(group = replacement_age, color = replacement_age), 
  #   se = FALSE) +
  scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean()
#'
#'
#'  
