#' Parameter [source](https://www.sciencedirect.com/science/article/pii/S1201971212013112)
DiseaseFarm <- R6::R6Class(
  classname = "DiseaseFarm",
  public = list(
    initialize = function(n_pigs,
                          rate_sus_to_expo = 0.400,
                          rate_expo_to_infect = (1 / 2.62),
                          rate_infect_to_recovered = 1 / 3.38,
                          rate_remove_clinical = 0.70,
                          vaccination = FALSE,
                          intervention = FALSE
    ) {
      private$time_counter <- 0
      private$n_pigs <- n_pigs
      private$intervention <- intervention
      #' all are susceptible in the beginning
      private$infected_status <- numeric(n_pigs)
      private$d_infected_status <- numeric(n_pigs)
      private$d_infected_status[] <- NA
      if (vaccination) {
        private$rate_sus_to_expo <- rate_sus_to_expo / 2.
      } else {
        private$rate_sus_to_expo <- rate_sus_to_expo
      }
      
      private$rate_expo_to_infect <- 1 - exp(-rate_expo_to_infect)
      private$rate_infect_to_recovered <-  1 - exp(-rate_infect_to_recovered)
      private$rate_remove_clinical <- 1 - exp(-rate_remove_clinical)
    },
    
    add_infected_pig = function(total_infected = 1) {
      stopifnot("no pigs are available for infection" = private$n_pigs >= total_infected)
      private$infected_status[
        sample.int(private$n_pigs, size = total_infected, replace = FALSE)] = 1
    },
    update_daily = function() {
      # ensures that disease status is overwritten by intervention
      self$update_disease_status();
      # intervene on a daily basis
      if (private$intervention) {
        self$update_intervention();
      }
      private$apply_daily()
      private$time_counter <- private$time_counter + 1
    },
    #' Internally updates the delta-updates. Remember to call `apply` after this.
    update_disease_status = function() {
      # how many susceptible become infected
      total_infectious <- sum(private$infected_status == 1 |
                              private$infected_status == 2)
      sus_to_exposed_rate <- 1 - exp(-private$rate_sus_to_expo * 
                                       total_infectious / private$n_pigs)
      which_susceptible <- which(private$infected_status == 0)
      sus_to_expo <- rbinom(length(which_susceptible), size = 1, prob = sus_to_exposed_rate) == 1
      
      which_exposed <- which(private$infected_status == 1)
      expo_to_infect <- rbinom(length(which_exposed), 1, private$rate_expo_to_infect) == 1
      
      which_infected <- which(private$infected_status == 2)
      infected_to_recovered <- rbinom(length(which_infected), size = 1, private$rate_infect_to_recovered) == 1
      
      
      # update state
      private$d_infected_status[which_susceptible[sus_to_expo]] <- 1
      private$d_infected_status[which_exposed[expo_to_infect]] <- 2
      private$d_infected_status[which_infected[infected_to_recovered]] <- 3
      
    },
    
    #' Performs removal of infected animals (not exposed!)
    #' Internally updates the delta-updates. 
    #' Remember to call `apply` after this.
    update_intervention = function() {
      which_infectious <- which(private$infected_status == 2)
      infectious_to_removal <-
        rbinom(length(which_infectious), size = 1, prob = private$rate_remove_clinical) == 1
      
      private$d_infected_status[which_infectious[infectious_to_removal]] <- 3
      
    },
    #' must execute after the separate `update_*` routines
    apply_daily = function() {
      changed_status_mask <- !is.na(private$d_infected_status)
      private$infected_status[changed_status_mask] <-
        private$d_infected_status[changed_status_mask]
      #' only revert the slots that was just used.. 
      private$d_infected_status[changed_status_mask] <- NA
    },
    #' Reset the internal state variables for re-use in replications / repetitions.
    reset = function() {
      private$time_counter <- 0
      #' all are susceptible in the beginning
      private$infected_status[] <- 0
      
    },
    #' Returns current counts of the disease compartments
    disease_status = function() {
      disease_status <- list(
        susceptible = sum(private$infected_status == 0),
        exposed = sum(private$infected_status == 1),
        infectious = sum(private$infected_status == 2),
        recovered = sum(private$infected_status == 3)
      )
      # VALIDATION:
      # stopifnot("compartments no longer make sense" = 
      #             sum(unlist(disease_status)) == private$n_pigs)
      disease_status
    }
  ),
  private = list(
    n_pigs = numeric(0),
    rate_sus_to_expo = numeric(0),
    rate_expo_to_infect = numeric(0),
    rate_infect_to_recovered = numeric(0),
    rate_remove_clinical = numeric(0),
    infected_status = numeric(0),
    #' delta changes on the disease states
    d_infected_status = numeric(0),
    time_counter = integer(0),
    intervention = logical(0)
  )
)
