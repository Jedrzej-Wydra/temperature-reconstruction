distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n(),
             amplitude = sqrt(sin^2 + cos^2)) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(rep)) +
  geom_line(aes(y = const_measurement), col = 'red') +
  geom_line(aes(y = const_weather), col = 'blue') +
  facet_wrap(~location) +
  theme_bw()

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n(),
             amplitude = sqrt(sin^2 + cos^2)) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(rep)) +
  geom_line(aes(y = sin_measurement), col = 'red') +
  geom_line(aes(y = sin_weather), col = 'blue') +
  facet_wrap(~location) +
  theme_bw()

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n(),
             amplitude = sqrt(sin^2 + cos^2)) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(rep)) +
  geom_line(aes(y = cos_measurement), col = 'red') +
  geom_line(aes(y = cos_weather), col = 'blue') +
  facet_wrap(~location) +
  theme_bw()

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n(),
             amplitude = sqrt(sin^2 + cos^2)) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(rep)) +
  geom_line(aes(y = amplitude_measurement), col = 'red') +
  geom_line(aes(y = amplitude_weather), col = 'blue') +
  facet_wrap(~location) +
  theme_bw()


distribution2 %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  ungroup() %>%
  dplyr::select(-location) %>% 
  distinct() %>%
  mutate(func_const = cumprod(replace_na(const / lag(const), 1))) %>%
  dplyr::select(rep, func_const)


distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n(),
             amplitude = sqrt(sin^2 + cos^2)) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct() %>%
      mutate(c1 = const, c2 = lag(const),
        func_const = cumprod(replace_na(const / lag(const), 1))) %>%
      dplyr::select(rep, func_const),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  dplyr::select(rep, location, const, func_const) %>% 
  mutate(recon = ifelse(rep == 1, const, NA)) %>%
  fill(recon) %>%
  transmute(location, const, recon_const = func_const*recon, func_const) %>%
  group_by(location) %>%
  #summarise(rmse = sqrt(mean((const - recon_const)^2)))
  summarise(mape = 100*(mean(abs(const - recon_const)/const)))

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n(),
             amplitude = sqrt(sin^2 + cos^2)) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct() %>%
      mutate(func_amplitude = cumprod(replace_na(amplitude / lag(amplitude), 1))) %>%
      dplyr::select(rep, func_amplitude),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  dplyr::select(rep, location, amplitude, func_amplitude) %>%
  mutate(recon = ifelse(rep == 1, amplitude, NA)) %>%
  fill(recon) %>%
  transmute(location, amplitude, recon_amplitude = func_amplitude*recon) %>%
  group_by(location) %>%
  #summarise(rmse = sqrt(mean((amplitude - recon_amplitude)^2)))
  summarise(mape = 100*(mean(abs(amplitude - recon_amplitude)/amplitude)))

distribution2 %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  ungroup() %>%
  dplyr::select(-location) %>% 
  distinct() %>%
  pull(const) -> const


distribution2 %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  ungroup() %>%
  dplyr::select(-location) %>% 
  distinct() %>%
  dplyr::select(sin) %>%
  pull(sin) -> sin


distribution2 %>%
  group_by(location) %>%
  mutate(rep = 1:n(),
         amplitude = sqrt(sin^2 + cos^2)) %>% 
  ungroup() %>%
  dplyr::select(-location) %>% 
  distinct() %>%
  pull(cos) -> cos
  
rev(const)

const_func <- cumprod(replace_na(rev(const) / lag(rev(const)), 1))[13:15]
sin_func <- cumprod(replace_na(rev(sin) / lag(rev(sin)), 1))[13:15]
cos_func <- cumprod(replace_na(rev(cos) / lag(rev(cos)), 1))[13:15]

