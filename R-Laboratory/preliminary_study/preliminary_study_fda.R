library(tidyverse)
library(lubridate)
library(fda)

loc_data_august
dataset_august_pila <- dataset_august[,c(1,4)]

data <- 
  loc_data_august %>% 
    left_join(dataset_august_pila, by = 'date') %>%
    rename(Y = temp, X = temp_230)

unique(data$location)

(loc <- unique(data$location)[1])

data %>%
  filter(location == loc) %>%
  mutate(hour = as.factor(hour(date))) %>%
  ggplot(aes(X,Y, color = hour, group = hour)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, col = 'red', linewidth = 2) +
  theme_bw() +
  theme(legend.position = 'none')

data %>%
  mutate(hour = hour(date)) %>%
  group_by(location, hour) %>%
  nest()

fbasis <- create.fourier.basis(rangeval = c(1, 24), 10)

fourier_basis <- function(x, bk = 10)
{
  fbasis <- create.fourier.basis(rangeval = c(1, 24), bk)
  data.frame(X = seq(1, 24, by = 0.01), 
             Y = eval.fd(seq(1, 24, by = 0.01), smooth.basis(1:24, x$value, fbasis)$fd))
}

coef_value <- "b"

data %>%
  mutate(hour = hour(date)) %>%
  group_by(location, hour) %>%
  nest() %>%
  mutate(reg = map(data, function(df) {lm(Y ~ X, df)})) %>%
  summarise(coef = map(reg, function(lm) {tibble(coef = c('b', 'a'), 
                                                 value = coef(lm))})) %>%
  unnest(cols = c(coef)) %>%
  filter(coef == coef_value) -> observed 

observed %>%
  ggplot(aes(hour, value)) +
  geom_line() +
  facet_wrap(~location) +
  theme_bw() 

data %>%
  mutate(hour = hour(date)) %>%
  group_by(location, hour) %>%
  nest() %>%
  mutate(reg = map(data, function(df) {lm(Y ~ X, df)})) %>%
  summarise(coef = map(reg, function(lm) {tibble(coef = c('b', 'a'), 
                                                 value = coef(lm))})) %>%
  unnest(cols = c(coef)) %>%
  filter(coef == coef_value) %>%
  nest() %>%
  transmute(location, 
            smooth_data = map(data, function(y) fourier_basis(y, bk = 6))) %>%
  unnest(cols = c(smooth_data)) %>%
  ggplot(aes(X, Y)) +
  geom_line(col = "red") +
  geom_line(data = observed, aes(hour, value)) +
  facet_wrap(~location) +
  theme_bw()
  
  









