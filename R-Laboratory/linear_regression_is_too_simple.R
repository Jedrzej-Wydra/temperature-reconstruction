
data %>%
  mutate(hour = as.factor(hour(date))) %>%
  ggplot(aes(X,Y, color = hour, group = hour)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_smooth(aes(group = 1), method = "lm", se = FALSE, col = 'red', linewidth = 2) +
  theme_bw() +
  theme(legend.position = 'none') +
  facet_wrap(~location) +
  xlab("Weather Station data") +
  ylab("Measurements")

data %>%
  mutate(hour = as.factor(hour(date))) %>%
  group_by(location, hour) %>%
  nest() %>%
  mutate(linear_model_grouped = unlist(map(data, function(x) {summary(lm(Y ~ X, data = x))$r.squared}))) %>%
  ungroup(hour) %>%
  unnest(cols = c(data)) %>%
  nest() %>%
  mutate(linear_model_ungrouped = unlist(map(data, function(x) {summary(lm(Y ~ X, data = x))$r.squared}))) %>%
  unnest(cols = c(data)) %>% 
  group_by(location, hour) %>% #View()
  summarise(grouped = mean(linear_model_grouped),
            ungrouped = mean(linear_model_ungrouped)) %>% 
  #ggplot(aes(grouped, fill = location)) +
  ggplot(aes(grouped)) +
  geom_density(fill = "#0CB702") +
  geom_vline(aes(xintercept = mean(ungrouped))) +
  #facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle("R squared")+
  xlab("R squared")

data %>%
  mutate(hour = as.factor(hour(date))) %>%
  group_by(location, hour) %>%
  nest() %>%
  mutate(linear_model_grouped = unlist(map(data, function(x) {cor(x$X, x$Y)}))) %>%
  ungroup(hour) %>%
  unnest(cols = c(data)) %>%
  nest() %>%
  mutate(linear_model_ungrouped = unlist(map(data, function(x) {cor(x$X, x$Y)}))) %>%
  unnest(cols = c(data)) %>% 
  group_by(location, hour) %>% #View()
  summarise(grouped = mean(linear_model_grouped),
            ungrouped = mean(linear_model_ungrouped)) %>% 
  #ggplot(aes(grouped, fill = location)) +
  ggplot(aes(grouped)) +
  geom_density(fill = "#0CB702") +
  geom_vline(aes(xintercept = mean(ungrouped))) +
  #facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle("Correlation") +
  xlab("Correlation")
  

data %>%
  mutate(hour = as.factor(hour(date))) %>%
  group_by(location, hour) %>%
  nest() %>%
  mutate(linear_model_grouped = unlist(map(data, function(x) {summary(lm(Y ~ X, data = x))$r.squared}))) %>%
  ungroup(hour) %>%
  unnest(cols = c(data)) %>%
  nest() %>%
  mutate(linear_model_ungrouped = unlist(map(data, function(x) {summary(lm(Y ~ X, data = x))$r.squared}))) %>%
  unnest(cols = c(data)) %>% 
  group_by(location, hour) %>% #View()
  summarise(grouped = mean(linear_model_grouped),
            ungrouped = mean(linear_model_ungrouped)) %>% 
  ggplot(aes(grouped, fill = location)) +
  #ggplot(aes(grouped)) +
  geom_density() +
  geom_vline(aes(xintercept = ungrouped)) +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle("R squared") +
  xlab("R squared")

data %>%
  mutate(hour = as.factor(hour(date))) %>%
  group_by(location, hour) %>%
  nest() %>%
  mutate(linear_model_grouped = unlist(map(data, function(x) {cor(x$X, x$Y)}))) %>%
  ungroup(hour) %>%
  unnest(cols = c(data)) %>%
  nest() %>%
  mutate(linear_model_ungrouped = unlist(map(data, function(x) {cor(x$X, x$Y)}))) %>%
  unnest(cols = c(data)) %>% 
  group_by(location, hour) %>% #View()
  summarise(grouped = mean(linear_model_grouped),
            ungrouped = mean(linear_model_ungrouped)) %>% 
  ggplot(aes(grouped, fill = location)) +
  #ggplot(aes(grouped)) +
  geom_density() +
  geom_vline(aes(xintercept = ungrouped)) +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle("Correlation") +
  xlab("Correlation")

  
