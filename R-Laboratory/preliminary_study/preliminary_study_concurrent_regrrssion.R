library(tidyverse)
library(lubridate)
library(fda)

data2 %>%
  dplyr::select(-Y1, -X1, -hour) %>%
  mutate(day = day(date),
         hour = hour(date)) %>%
  ggplot(aes(hour, X, color = as.factor(day), group = as.factor(day))) +
  geom_line() +
  facet_wrap(~location) +
  theme_bw() +
  theme(legend.position = 'none')

data2 %>%
  dplyr::select(-Y1, -X1, -hour) %>%
  mutate(day = day(date),
         hour = hour(date)) %>%
  ggplot(aes(hour, Y, color = as.factor(day), group = as.factor(day))) +
  geom_line() +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() +
  theme(legend.position = 'none')

fbasis <- create.fourier.basis(rangeval = c(1, 24), 4)

data %>%
  mutate(hour = hour(date),
         day = day(date)) %>%
  group_by(location, day) %>%
  mutate(n = n()) %>%
  filter(n >= 23) %>%
  dplyr::select(-n) %>%
  mutate(Y1 = Y,
         Y = eval.fd(1:n(), smooth.basis(1:n(), Y, fbasis)$fd)[,1],
         X1 = X,
         X = eval.fd(1:n(), smooth.basis(1:n(), X, fbasis)$fd)[,1]) -> data3

data3 %>%
  dplyr::select(-Y1, -X1, -hour) %>%
  mutate(day = day(date),
         hour = hour(date)) %>%
  ggplot(aes(hour, Y, color = as.factor(day), group = as.factor(day))) +
  geom_line() +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() +
  theme(legend.position = 'none')

smooth_fourier <- function(x, comp = 4, h = nrow(x))
{
  smooth.basis(1:h, x, 
               create.fourier.basis(rangeval = c(1, h), comp))$fd$coefs[,1]
}

data %>%
  mutate(hour = hour(date),
         day = day(date)) %>%
  group_by(location, day) %>% 
  nest() %>% 
  mutate(n = unlist(map(data, nrow))) %>%
  filter(n >= 23) %>%
  dplyr::select(-n) %>%
  mutate(X_coefs = map(data, function(x) {smooth_fourier(x$X, h = nrow(x))}),
         Y_coefs = map(data, function(x) {smooth_fourier(x$Y, h = nrow(x))})) %>%
  dplyr::select(-data) -> fda_data


for(i in unique(fda_data$location))
{
  fda_data %>%
    filter(location == i) %>%
    ungroup() %>%
    dplyr::select(-location, -day) %>%
    pull(X_coefs) %>%
    unlist() %>%
    matrix(., ncol = 5) -> X_mat
  
  fda_data %>%
    filter(location == i) %>%
    ungroup() %>%
    dplyr::select(-location, -day) %>%
    pull(Y_coefs) %>%
    unlist() %>%
    matrix(., ncol = 5) -> Y_mat
  
  fda_X <- fd(t(X_mat), create.fourier.basis(rangeval = c(1, 24), 4)$fd)
  fda_Y <- fd(t(Y_mat), create.fourier.basis(rangeval = c(1, 24), 4)$fd)
  
  fRegress(fda_Y, 
           list(const = rep(1,15), 
                X = fda_X), 
           list(const = create.fourier.basis(rangeval = c(1, 24), 6),
                X = create.fourier.basis(rangeval = c(1, 24), 6)))
}
