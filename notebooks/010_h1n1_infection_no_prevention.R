
devtools::load_all(".")
#' Investigating the affect of increased herd sizes in pig productions in Italy
#' from 55 pigs to 365 pigs.
#' 
#' 

end_time <- 1 * 365
farm_small <- DiseaseFarm$new(n_pigs = 55, vaccination = TRUE)
farm_big <- DiseaseFarm$new(n_pigs = 356, vaccination = TRUE)
disease_status_collect <- list(
  farm_small = vector(mode = "list", end_time),
  farm_big = vector(mode = "list", end_time)
)


farm_small$add_infected_pig(10)
farm_big$add_infected_pig()
for (t in seq_len(end_time)) {
  farm_small$update_daily()
  farm_big$update_daily()
  disease_status_collect$farm_small[[t]] <- farm_small$disease_status() %>% 
    c(time = t, .)
  disease_status_collect$farm_big[[t]] <- farm_big$disease_status() %>% 
    c(time = t, .)
}

farm_small$disease_status()
farm_big$disease_status()

disease_status_collect %>%
  enframe(name = "farm_type") %>% 
  unnest(value) %>% 
  unnest_wider(value) %>% 
  
  pivot_longer(c(susceptible, exposed, infectious, recovered),
               names_to = "compartment",
               values_to = "count") %>%
  glimpse() %>%
  identity() %>%
  ggplot() +
  aes(time, count) +
  geom_step(# aes(color = compartment, group = str_c(compartment, rep_id))) +
    aes(color = compartment, group = interaction(compartment, farm_type))) +
  
  # geom_smooth(aes(group = compartment, color = compartment)) +
  facet_wrap(~farm_type, ncol = 1, scales = "free_y") + 
  # scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean() ->
  disease_trace_plot
disease_trace_plot

# plotly::ggplotly(disease_trace_plot)
