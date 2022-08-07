library(tidyverse)
library(R6)

#' Parameter [source](https://www.sciencedirect.com/science/article/pii/S1201971212013112)
Farm <- R6Class(
  classname = "Farm",
  public = list(
    initialize = function(n_pigs, 
                          # no assymptomatic
                          rate_sus_to_expo = 0.400,
                          rate_expo_to_infect = (1 / 2.62),
                          rate_infect_to_recovered = 1 / 3.38
                          # 
                          # rate_sus_to_expo = 0.400,
                          # rate_expo_to_infect = (1 - 0.398) * (1 / 2.62),
                          # rate_infect_to_recovered = 1 / 3.38,
                          # rate_expo_to_assympto = 0.398 * (1 / 2.62),
                          # rate_assympto_to_recovered = 1 / 3.38
                          ) {
      private$n_pigs <- n_pigs
      #' all are susceptible in the beginning
      private$infected_status <- numeric(n_pigs)
      private$rate_sus_to_expo <- rate_sus_to_expo
      private$rate_expo_to_infect <- rate_expo_to_infect
      private$rate_infect_to_recovered <- rate_infect_to_recovered
    },
    
    add_infected_pig = function() {
      stopifnot("no pigs are available for infection" = private$n_pigs >= 1)
      private$infected_status[1] = 1
    },
    update_daily = function() {
      # browser()
      # how many susceptible become infected
      total_infected <- sum(private$infected_status == 1 |
                              private$infected_status == 2)
      
      sus_to_exposed_rate <- 1 - exp(-private$rate_sus_to_expo * 
                                      total_infected / private$n_pigs)
      which_susceptible <- which(private$infected_status == 0)
      sus_to_expo <- rbinom(length(which_susceptible), size = 1, prob = sus_to_exposed_rate) == 1
      
      which_exposed <- which(private$infected_status == 1)
      expo_to_infect <- rbinom(length(which_exposed), 1, 1 - exp(-private$rate_expo_to_infect)) == 1
      
      which_infected <- which(private$infected_status == 2)
      infected_to_recovered <- rbinom(length(which_infected), size = 1, 1 - exp(-private$rate_infect_to_recovered)) == 1
      
      
      # update state
      private$infected_status[which_susceptible[sus_to_expo]] <- 1
      private$infected_status[which_exposed[expo_to_infect]] <- 2
      private$infected_status[which_infected[infected_to_recovered]] <- 3
      
    },
    disease_status = function() {
      disease_status = c(
        susceptible = sum(private$infected_status == 0),
        exposed = sum(private$infected_status == 1),
        infectious = sum(private$infected_status == 2),
        recovered = sum(private$infected_status == 3)
      )
      stopifnot("compartments no longer make sense" = 
                  sum(disease_status) == private$n_pigs)
      disease_status
    }
  ),
  private = list(
    n_pigs = numeric(1),
    rate_sus_to_expo = numeric(1),
    rate_expo_to_infect = numeric(1),
    rate_infect_to_recovered = numeric(1),
    infected_status = numeric(0)
  )
)
