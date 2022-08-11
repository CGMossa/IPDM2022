rfarm <- DiseaseFarm$new(n_pigs = 100)

total_iterations <- 100
total_ticks <- 2e3
bench::mark(
  no_reset = {
    set.seed(222)
    for (iter_ in total_iterations) {
      farm <- DiseaseFarm$new(n_pigs = 100)
      for (tick_ in seq_len(total_ticks)) {
        farm$update_daily()
      }
    }
    farm$disease_status()
  },
  reset = {
    set.seed(222)
    for (iter_ in total_iterations) {
      rfarm$reset()
      for (tick_ in seq_len(total_ticks)) {
        rfarm$update_daily()
      }
    }
    rfarm$disease_status()
  },
  # min_time = Inf,
  filter_gc = FALSE
) %>% 
  print(width = Inf) %>% 
  plot()
