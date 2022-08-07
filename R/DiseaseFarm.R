library(tidyverse)
library(R6)


Farm <- R6Class(
  classname = "Farm",
  public = list(
    initialize = function(n_pigs, transmission_rate) {
      private$n_pigs <- n_pigs
      private$transmission_rate <- transmission_rate
      #' all are susceptible in the beginning
      private$infected_status <- numeric(n_pigs)
    },
    
    add_infected_pig = function() {
      stopifnot("no pigs are available for infection" = private$n_pigs >= 1)
      private$infected_status[1] = 1
    },
    update_daily = function() {
      # how many susceptible become infected
      # browser()
      total_infected <- self$disease_status()["infected"]
      sus_to_infect_rate <- 1 - exp(-private$transmission_rate * 
                                      total_infected / private$n_pigs)
      which_susceptible <- which(private$infected_status == 0)
      sus_to_infect <- rbinom(length(which_susceptible), size = 1, prob = sus_to_infect_rate) == 1
      
      # TODO: how many infected becomes removed
      
      
      # update state
      private$infected_status[sus_to_infect] <- 1
      
    },
    disease_status = function() {
      disease_status = c(
        susceptible = sum(private$infected_status == 0),
        infected = sum(private$infected_status == 1),
        recovered = sum(private$infected_status == 2)
      )
      stopifnot("compartments no longer make sense" = 
                  sum(disease_status) == private$n_pigs)
      disease_status
    }
  ),
  private = list(
    n_pigs = numeric(1),
    transmission_rate = numeric(1),
    #' 0 means susceptible, 1 means infected, 2 means removed
    infected_status = numeric(0)
    # susceptible = numeric(0),
    # infected = numeric(0),
    # removed = numeric(0),
  )
)
