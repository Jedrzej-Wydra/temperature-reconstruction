data %>%
  ggplot(aes(x = date, y = X)) +
  geom_line() +
  geom_line(aes(y = Y), col = 'red') +
  facet_wrap(~location) +
  theme_bw()

unique(data$location)

(loc <- unique(data$location)[1])

data %>% 
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y_trend = decompose(ts(frequency = 24, Y))$trend,
         Y_seasonal = decompose(ts(frequency = 24, Y))$seasonal,
         Y_random = decompose(ts(frequency = 24, Y))$random,
         X_trend = decompose(ts(frequency = 24, X))$trend,
         X_seasonal = decompose(ts(frequency = 24, X))$seasonal,
         X_random = decompose(ts(frequency = 24, X))$random) %>% 
  ggplot(aes(Y_trend, X_trend, color = as.factor(hour), group = as.factor(hour))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none')

data %>% 
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y_trend = decompose(ts(frequency = 24, Y))$trend,
         Y_seasonal = decompose(ts(frequency = 24, Y))$seasonal,
         Y_random = decompose(ts(frequency = 24, Y))$random,
         X_trend = decompose(ts(frequency = 24, X))$trend,
         X_seasonal = decompose(ts(frequency = 24, X))$seasonal,
         X_random = decompose(ts(frequency = 24, X))$random) %>% 
  ggplot(aes(Y_seasonal, X_seasonal, color = as.factor(hour), group = as.factor(hour))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none')

data %>% 
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y_trend = decompose(ts(frequency = 24, Y))$trend,
         Y_seasonal = decompose(ts(frequency = 24, Y))$seasonal,
         Y_random = decompose(ts(frequency = 24, Y))$random,
         X_trend = decompose(ts(frequency = 24, X))$trend,
         X_seasonal = decompose(ts(frequency = 24, X))$seasonal,
         X_random = decompose(ts(frequency = 24, X))$random) %>% 
  ggplot(aes(Y_random, X_random, color = as.factor(hour), group = as.factor(hour))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none')

data %>% 
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y_trend = decompose(ts(frequency = 24, Y))$trend,
         Y_seasonal = decompose(ts(frequency = 24, Y))$seasonal,
         Y_random = decompose(ts(frequency = 24, Y))$random,
         X_trend = decompose(ts(frequency = 24, X))$trend,
         X_seasonal = decompose(ts(frequency = 24, X))$seasonal,
         X_random = decompose(ts(frequency = 24, X))$random) %>% 
  ggplot(aes(date, X_seasonal)) +
  geom_line() +
  geom_line(aes(y = Y_seasonal), color = 'red') +
  geom_smooth(method = 'lm') +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none')

