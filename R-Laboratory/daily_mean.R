temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
daily_mean <- data.frame()

for(loc in unique(data$location))
{
  tempbasis <- create.fourier.basis(temprange, nbasis=bk)
  weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
  
  full_data_temperature_Y <- cbind(test_sets[[loc]]$Y, train_sets[[loc]]$Y)
  full_data_temperature_X <- cbind(test_sets[[loc]]$X, train_sets[[loc]]$X)
  
  tempfd <- smooth.basis(temptime, full_data_temperature_Y, tempbasis, method = 'qr')$fd
  weatherfd <- smooth.basis(temptime, full_data_temperature_X, weatherbasis, method = 'qr')$fd
  
  smoothed_full_data_temperature_Y <- eval.fd(1:23,tempfd)
  
  simple_data <- colMeans(matrix(predict(lm(as.numeric(full_data_temperature_Y) ~ as.numeric(full_data_temperature_X))), ncol = 15))
  weather_data <- colMeans(full_data_temperature_X)
  actual_data <- colMeans(full_data_temperature_Y)
  smoothed_data <- colMeans(smoothed_full_data_temperature_Y)

  means <- data.frame(location = loc,
                      day = 1:length(actual_data),
                      simple_data = simple_data,
                      #weather_data = weather_data,
                      #actual_data = actual_data,
                      smoothed_data = smoothed_data)
  
  daily_mean <- rbind(daily_mean, means)
    
}

rownames(daily_mean) <- NULL
daily_mean %>%
  pivot_longer(simple_data:smoothed_data) %>%
  ggplot(aes(day, value, color = name, linetype = name)) +
  geom_line() +
  facet_wrap(~location) +
  theme_bw()
