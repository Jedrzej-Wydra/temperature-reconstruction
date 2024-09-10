distribution %>%
  group_by(location) %>%
  summarise(const_mean = mean(const),
            const_sd = sd(const),
            sin_mean = mean(sin),
            sin_sd = sd(sin),
            cos_mean = mean(cos),
            cos_sd = sd(cos)) %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean))

# standard deviation form 0.5 to 2

distribution %>%
  group_by(location) %>%
  summarise(const_mean = mean(const)) %>%
  left_join(
    data %>%
      group_by(location) %>%
      summarise(weather_station = mean(X)),
    by = "location"
  ) %>%
  mutate(diff = const_mean - weather_station) %>%
  summarise(min = min(diff),
            max = max(diff))

known_zero_temp <- 
  distribution %>%
  group_by(location) %>%
  summarise(const_mean = mean(const)) %>%
  left_join(
    data %>%
      group_by(location) %>%
      summarise(weather_station = mean(X)),
    by = "location"
  ) %>%
  mutate(diff = const_mean - weather_station)

# zero temp is different from mean weather temperature about 0 to 4 degrees

distribution %>%
  group_by(location) %>%
  mutate(aplitude = sqrt(sin^2 + cos^2)) %>%
  mutate(normal = rnorm(n(), mean(aplitude), sd(aplitude))) %>%
  ggplot(aes(aplitude)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw()

(characteristics_of_amplitude_distribution <- 
  distribution %>%
  group_by(location) %>%
  mutate(aplitude = sqrt(sin^2 + cos^2)) %>%
  summarise(mean = mean(aplitude),
            sd = sd(aplitude)))
  
# standard deviation form 0 to 3

distribution2 %>%
  group_by(location) %>%
  mutate(aplitude = sqrt(sin^2 + cos^2)) %>%
  dplyr::select(location, X = aplitude) %>%
  bind_cols(
    distribution %>%
      group_by(location) %>%
      mutate(aplitude = sqrt(sin^2 + cos^2)) %>%
      ungroup() %>%
      dplyr::select(Y = aplitude)
  ) %>%
  transmute(location,
            diff_amp = Y - X) %>%
  group_by(location) %>%
  mutate(normal = rnorm(n(), mean(diff_amp), sd(diff_amp))) %>%
  ggplot(aes(diff_amp)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw()

distribution2 %>%
  group_by(location) %>%
  mutate(aplitude = sqrt(sin^2 + cos^2)) %>%
  dplyr::select(location, X = aplitude) %>%
  bind_cols(
    distribution %>%
      group_by(location) %>%
      mutate(aplitude = sqrt(sin^2 + cos^2)) %>%
      ungroup() %>%
      dplyr::select(Y = aplitude)
  ) %>%
  transmute(location,
            diff_amp = Y - X) %>%
  group_by(location) %>%
  summarise(mean = mean(diff_amp),
            sd = sd(diff_amp))
  
