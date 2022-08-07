library(tidyverse)
devtools::load_all(".")
#' Investigating the affect of increased herd sizes in pig productions in Italy
#' from 55 pigs to 365 pigs.
#' 
repetitions <- 250
expand_grid(
  end_time = 1 * 365,
  vaccination = c(FALSE, TRUE),
  intervention = c(FALSE, TRUE),
  n_pigs = c(55, 365)
) %>% 
  arrange(n_pigs) %>%
  print() %>% 
  filter(vaccination != TRUE | intervention != TRUE) %>% 
  rowid_to_column("scenario") %>% 
  identity() -> 
  scenario_desc_df

scenario_desc_df %>% 
  # View() %>% 
  expand_grid(rep_id = seq_len(repetitions)) ->
  scenario_config_df


scenario_config_df %>% 
  rowwise() %>% 
  
  mutate(results = {
    result <- list(time_of_outbreak = NA, total_infected_animals = NA)
    
    farm <- Farm$new(n_pigs = n_pigs, vaccination = vaccination)
    farm$add_infected_pig(1)
    for (t in seq_len(end_time)) {
      farm$update_daily()
      status <- farm$disease_status()
      # browser()
      total_infected <- status$exposed + status$infectious
      if (total_infected == 0) {
        result$time_of_outbreak <- t
        result$total_infected_animals <- n_pigs - status$susceptible
        break
      }
    }
    result %>% list()
  }) %>% 
  ungroup() %>% 
  unnest_wider(results) -> 
  output_df

output_df %>%
  group_by(scenario) %>% 
  summarise(across(c(time_of_outbreak, total_infected_animals), 
                   list(mean = mean, sd = sd))) %>% 
  left_join(scenario_desc_df) ->
  output_summary
output_summary %>% 
  
  pander::pander_return() %>% 
  clipr::write_clip()



scenario_desc_df %>% 
  expand_grid(
    rep_id = seq_len(10)
  ) %>% 
  # slice(1:4) %>% 
  rowwise() %>% 
  
  mutate(results = {
    # result <- list(time_of_outbreak = NA, total_infected_animals = NA)
    result <- list()
    farm <- Farm$new(n_pigs = n_pigs, vaccination = vaccination)
      # browser()
    farm$add_infected_pig(1)
    for (t in seq_len(end_time)) {
      farm$update_daily()
      status <- farm$disease_status()
      result[[t]] <- list(time = t, status = status)
      total_infected <- status$exposed + status$infectious
      if (total_infected == 0) {
        break
      }
    }
    result %>% 
      list()
  }) %>% 
  ungroup() %>% 
  # unnest_wider(results) %>% 
  identity() ->
  output_df
output_df %>% 
  unnest(results) %>% 
  unnest_wider(results) %>% 
  unnest_wider(status) %>%
  glimpse() %>% 
  
  pivot_longer(c(susceptible, exposed, infectious, recovered),
               names_to = "compartment",
               values_to = "count") %>%
  glimpse() %>%
  identity() %>%
  ggplot() +
  aes(time, count) +
  # ggrepel::geom_text_repel(
  #   data = . %>%
  #     ungroup() %>%
  #     group_by(scenario) %>%
  #     slice(1) %>% 
  #     ungroup(),
  #   aes(x = 120, y = 50, label = "a")
  # ) +
  geom_step(
    aes(color = compartment, 
        group = interaction(compartment, rep_id))) +
  
  # geom_smooth(aes(group = compartment, color = compartment)) +
  facet_wrap(vaccination + intervention ~ n_pigs, ncol = 2,
             labeller = . %>% 
               label_both(multi_line = FALSE),
             scales = "free_y") + 
  # scale_color_viridis_c(option = "A") +
  ggpubr::theme_pubclean() ->
  disease_trace_plot
disease_trace_plot

# plotly::ggplotly(disease_trace_plot)
