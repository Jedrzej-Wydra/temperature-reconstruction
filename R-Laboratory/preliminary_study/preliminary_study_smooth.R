library(tidyverse)
library(lubridate)
library(fda)

data %>%
  filter(location == loc) %>%
  mutate(hour = as.factor(hour(date))) %>%
  ggplot(aes(X,Y, color = hour, group = hour)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, col = 'red', linewidth = 2) +
  theme_bw() +
  theme(legend.position = 'none')


fbasis <- create.fourier.basis(rangeval = c(1, 24), 2)

fourier_basis <- function(x, bk = 10)
{
  fbasis <- create.fourier.basis(rangeval = c(1, 24), bk)
  data.frame(X = seq(1, 24, by = 0.01), 
             Y = eval.fd(seq(1, 24, by = 0.01), smooth.basis(1:24, x$value, fbasis)$fd))
}

data %>%
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y1 = Y,
         Y = eval.fd(1:n(), smooth.basis(1:n(), Y, fbasis)$fd)[,1],
         X1 = X,
         X = eval.fd(1:n(), smooth.basis(1:n(), X, fbasis)$fd)[,1]) %>%
  ggplot(aes(date, Y1)) +
  geom_line() +
  geom_line(aes(y = Y), col = 'red') +
  facet_wrap(~location) +
  theme_bw()

data %>%
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y1 = Y,
         Y = eval.fd(1:n(), smooth.basis(1:n(), Y, fbasis)$fd)[,1],
         X1 = X,
         X = eval.fd(1:n(), smooth.basis(1:n(), X, fbasis)$fd)[,1]) %>%
  ggplot(aes(date, X1)) +
  geom_line() +
  geom_line(aes(y = X), col = 'red') +
  facet_wrap(~location) +
  theme_bw()

data %>%
  mutate(hour = hour(date)) %>%
  group_by(location) %>%
  mutate(Y1 = Y,
         Y = eval.fd(1:n(), smooth.basis(1:n(), Y, fbasis)$fd)[,1],
         X1 = X,
         X = eval.fd(1:n(), smooth.basis(1:n(), X, fbasis)$fd)[,1]) -> data2


coef_value <- "a"

data2 %>%
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

bspline_basis <- function(x, bk = 10)
{
  fbasis <- create.bspline.basis(rangeval = c(1, 24), bk)
  data.frame(X = seq(1, 24, by = 0.01), 
             Y = eval.fd(seq(1, 24, by = 0.01), smooth.basis(1:24, x$value, fbasis)$fd))
}

data2 %>%
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











