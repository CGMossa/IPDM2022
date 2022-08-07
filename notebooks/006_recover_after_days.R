# set.seed(250)
# set.seed(2022-08-06)
library(ggplot2)
library(magrittr)
library(tidyverse)

farm_model <- function(n_cows = 100, 
                       ProbInfection = 0.01,
                       ProbRecovery = NA,
                       days_infected = NA,
                       replacement_age = NA, 
                       end_time = 5 * 365) {
  stopifnot(
    "only set the rate of recovery of days of infection" = 
      is.na(ProbRecovery) + is.na(days_infected) == 1,
    "days until recovery has to be `NA` or positive integer" = 
      is.na(days_infected) | days_infected > 0
  )
  
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
                 replacement_age = replacement_age_distr(n_cows),
                 infected = 0,
                 time_until_recovery = 0L
  )
  farm$infected[1] <- 1
  if (is.na(ProbRecovery)) {
    farm$time_until_recovery[1] <- days_infected
  }
  
  # Collect the mean age of the cows in herd over time:
  age_collect <- numeric(end_time)
  times <- seq_len(end_time)
  inf_collect <- matrix(NA, nrow = end_time, ncol = 3) %>% 
    set_colnames(c("susceptible", "infected", "recovered"))
  
  for (t in times)
  {
    # `past_farm` <- farm
    # If cows reach the age of 1642,
    # they are replaced with a new cow that is 2 years old in DIM = 1:
    is_replaced <- which(farm$age >= farm$replacement_age);
    if (length(is_replaced) > 0) {
      farm$age[is_replaced] <- 730
      farm$replacement_age[is_replaced] <- replacement_age_distr(length(is_replaced))
    }
    
    # Add one day to the age of all the animals, for each simulated:
    farm$age <- farm$age + 1
    
    
    # recovery
    pot_recovered <- which(farm$infected == 1)
    if (length(pot_recovered) > 0) {
      if (is.na(ProbRecovery)) {
        NewRecovered <- which(farm$infected == 1 & farm$time_until_recovery == 0)
        
        still_infected <- setdiff(pot_recovered, NewRecovered)
        farm$time_until_recovery[still_infected] <- 
          farm$time_until_recovery[still_infected] - 1
      } else {
        NewRecovered <- pot_recovered[
          rbinom(length(pot_recovered), 1, prob = ProbRecovery) == 1
        ]
      }
    }
    stopifnot(all(farm$time_until_recovery >= 0))
    
    if (length(NewRecovered) > 0) {
      farm$infected[NewRecovered] <- 2
    }
    
    # disease
    susceptible <- which(farm$infected == 0)
    total_infected <- sum(farm$infected == 1)
    infect_rate <- total_infected * ProbInfection / n_cows
    infect_probability <- 1 - exp(-infect_rate)
    NewInf <- susceptible[
      rbinom(length(susceptible), 1, prob = infect_probability) == 1]
    if (length(NewInf) > 0) {
      farm$infected[NewInf] <- 1
      if (is.na(ProbRecovery)) {  
        farm$time_until_recovery[NewInf] <- days_infected
      }
    }
    
    # Save the number of susceptible and infected:
    sus <- sum(farm$infected == 0)
    inf <- sum(farm$infected == 1)
    rec <- sum(farm$infected == 2)
    inf_collect[t, 1] <- sus
    inf_collect[t, 2] <- inf
    inf_collect[t, 3] <- rec
    
    # Save the daily mean age of all cows:
    age_collect[t] <- mean(farm$age)
    
    # terminate early if infection is over..
    # what about recovered after infection is over?
    if (inf == 0) {
      break
    }
  }
  last_t <- t
  list(age_collect = age_collect[c(seq_len(last_t), last_t)],
       times = times[c(seq_len(last_t), end_time)],
       replacement_age = replacement_age[c(seq_len(last_t), last_t)],
       disease_collect = 
         inf_collect[c(seq_len(last_t), last_t),, drop = FALSE] %>% 
         as_tibble())
}
farm_model(
  end_time = 1 * 365,
  ProbInfection = 0.10,
  # ProbRecovery = 1 /  14,
  days_infected = 14,
  replacement_age = NA) %>% 
  # assign(x = "replacement_age", value = NULL) %>% 
  as_tibble() %>% 
  unpack(disease_collect) %>% 
  
  #' stop at 100% infected (plus 10 more time steps)
  # slice(seq_len(which(.$susceptible == 0)[1] + 10)) %>% 
  
  ggplot() +
  aes(times) + 
  geom_line(aes(y = susceptible, color = "susceptible")) +
  geom_line(aes(y = infected, color = "infected")) +
  geom_line(aes(y = recovered, color = "recovered")) +
  # scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean()



repetitions <- 250
tibble(
  rep_id = seq_len(repetitions)
) %>% 
  rowwise() %>%
  mutate(results = 
           farm_model(
             end_time = 1 * 365,
             ProbInfection = 0.10,
             # ProbRecovery = 1 /  14,
             days_infected = 14,
             replacement_age = NA) %>% 
           list()) %>% 
  ungroup() ->
  output_df

output_df %>% 
  hoist(results, "disease_collect", "times") %>% 
  select(-results) %>% 
  unnest() %>% 
  
  glimpse() %>% 
  
  pivot_longer(c(susceptible, infected, recovered), names_to = "compartment",
               values_to = "count") %>% 
  
  identity() %>% 
  ggplot() + 
  aes(times, count) + 
  geom_step(
    aes(color = compartment, group = str_c(compartment, rep_id))) + 
  
  # geom_smooth(method = "glm", aes(group = compartment, color = compartment),
  #             method.args = list(family = poisson)) +
  
  # scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean()


