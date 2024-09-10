# constant

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(const_measurement, const_weather)) +
  geom_point() +
  facet_wrap(~location) +
  geom_smooth(method = 'lm', se = F, formula = 'y ~ x + 0') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(const_measurement ~ const_weather, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('intercept', 'slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(const_measurement ~ const_weather + 0, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')



# sinus

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(sin_measurement, sin_weather)) +
  geom_point() +
  facet_wrap(~location) +
  geom_smooth(method = 'lm', se = F, formula = 'y ~ x + 0') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(sin_measurement ~ sin_weather, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('intercept', 'slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(sin_measurement ~ sin_weather + 0, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')


# cosinus

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  ggplot(aes(cos_measurement, cos_weather)) +
  geom_point() +
  facet_wrap(~location) +
  geom_smooth(method = 'lm', se = F, formula = 'y ~ x + 0') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(cos_measurement ~ cos_weather, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('intercept', 'slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')

distribution %>%
  group_by(location) %>%
  mutate(rep = 1:n()) %>% 
  left_join(
    distribution2 %>%
      group_by(location) %>%
      mutate(rep = 1:n()) %>% 
      ungroup() %>%
      dplyr::select(-location) %>% 
      distinct(),
    by = 'rep',
    suffix = c('_measurement', '_weather')
  ) %>%
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(cos_measurement ~ cos_weather + 0, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')


# amplitude

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
  ggplot(aes(amplitude_measurement, amplitude_weather)) +
  geom_point() +
  facet_wrap(~location) +
  geom_smooth(method = 'lm', se = F, formula = 'y ~ x + 0') +
  geom_abline(slope = 1, intercept = 0) +
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
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(amplitude_measurement ~ amplitude_weather, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('intercept', 'slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')

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
  nest() %>%
  mutate(data = map(data, function(x) coef(lm(amplitude_measurement ~ amplitude_weather + 0, x)))) %>%
  unnest(cols = c(data)) %>%
  mutate(coef = c('slope')) %>%
  pivot_wider(names_from = 'coef', values_from = 'data')

