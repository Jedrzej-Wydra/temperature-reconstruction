short_reconstruction %>%
  dplyr::select(location, length, bayes_error_value, best_bayes_value) %>%
  bind_cols(
    short_reconstruction_fda %>%
      dplyr::select(bayes_error_value_fda, best_bayes_value_fda)
  ) %>%
  filter(length != 1, location != 'basement') %>%
  pivot_longer(bayes_error_value:best_bayes_value_fda) %>%
  ggplot(aes(length, value, color = name)) +
  geom_line() +
  facet_wrap(~location) +
  theme_bw()
  

short_reconstruction %>%
  dplyr::select(location, length, bayes_error_value, best_bayes_value) %>%
  bind_cols(
    short_reconstruction_fda %>%
      dplyr::select(bayes_error_value_fda, best_bayes_value_fda)
  ) %>%
  #filter(length != 1, location != 'basement') %>%
  filter(length != 1) %>%
  pivot_longer(bayes_error_value:best_bayes_value_fda) %>%
  group_by(length, name) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(length, value, color = name)) +
  geom_line() +
  theme_bw()
