temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
distribution <- data.frame()

for(loc in unique(data$location))
{
  tempbasis <- create.fourier.basis(temprange, nbasis=bk)
  weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
  
  full_data_temperature_Y <- cbind(test_sets[[loc]]$Y, train_sets[[loc]]$Y)
  full_data_temperature_X <- cbind(test_sets[[loc]]$X, train_sets[[loc]]$X)
  
  tempfd <- smooth.basis(temptime, full_data_temperature_Y, tempbasis, method = 'qr')$fd
  weatherfd <- smooth.basis(temptime, full_data_temperature_X, weatherbasis, method = 'qr')$fd
  
  smoothed_full_data_temperature_Y <- eval.fd(1:23,tempfd)
  
  residuals <- as.numeric(full_data_temperature_Y - smoothed_full_data_temperature_Y)
  residuals_square <- as.numeric(full_data_temperature_Y - smoothed_full_data_temperature_Y)^2
  
  distribution <- rbind(distribution, 
                        data.frame(location = loc,
                                   residuals = residuals,
                                   residuals_square = residuals_square))
}

set.seed(2024)

distribution %>%
  group_by(location) %>%
  mutate(normal = rnorm(n(), mean(residuals), sd(residuals))^2) %>%
  dplyr::select(-residuals) %>%
  pivot_longer(residuals_square:normal) %>%
  ggplot(aes(value, group = name, color = name, fill = name)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() +
  ggtitle("residuals squared") +
  xlim(0, 15)

distribution %>%
  group_by(location) %>%
  mutate(normal = rnorm(n(), mean(residuals), sd(residuals))) %>%
  dplyr::select(-residuals_square) %>%
  pivot_longer(residuals:normal) %>%
  ggplot(aes(value, group = name, color = name, fill = name)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() 

distribution %>%
  group_by(location) %>%
  summarise(sd = sd(residuals)) %>%
  mutate(in_out = c(0,2,0,0,1,3,0,1,1)) %>%
  group_by(in_out) %>%
  summarise(E = mean(sd), S = sd(sd))

distribution %>%
  summarise(S = sd(residuals))

distribution %>%
  group_by(location) %>%
  summarise(sd = sd(residuals)) %>%
  filter(location != 'basement') %>%
  summarise(min = min(sd),
            max = max(sd))

set.seed(2024)

distribution %>%
  group_by(location) %>%
  summarise(sd = sd(residuals)) %>%
  filter(location != 'basement') %>%
  ggplot(aes(sd)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(data = data.frame(X = rnorm(1000, 1, 0.75)), 
               aes(x = X),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  theme_bw()
 
known_sd_of_residuals <- 
  distribution %>%
  group_by(location) %>%
  summarise(sd = sd(residuals))


