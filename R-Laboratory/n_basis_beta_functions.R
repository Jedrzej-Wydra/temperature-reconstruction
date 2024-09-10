temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3

for(loc in unique(data$location))
{
  
  for(beta_bk in seq(from = 3, to = 21, by = 2))
  {
    tempbasis <- create.fourier.basis(temprange, nbasis=bk)
    weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
    
    train_temperature_Y <- train_sets[[loc]]$Y[,(12 - len):12]
    train_temperature_X <- train_sets[[loc]]$X[,(12 - len):12]
    
    test_temperature_Y <- test_sets[[loc]]$Y
    test_temperature_X <- test_sets[[loc]]$X
    
    tempfd <- smooth.basis(temptime, train_temperature_Y, tempbasis, method = 'qr')$fd
    weatherfd <- smooth.basis(temptime, train_temperature_X, weatherbasis, method = 'qr')$fd
    
    weatherfd_test <- smooth.basis(temptime, test_temperature_X, weatherbasis, method = 'qr')$fd
    
    const <- rep(1, dim(tempfd$coef)[2])
    
    xfdlist  <- list(const=const, weatherfd=weatherfd)
    
    beta_basis <- create.fourier.basis(temprange, nbasis=beta_bk)
    
    beta0 <- fd(basisobj=beta_basis, fdnames=tempfd$fdnames)
    beta1 <- fd(basisobj=beta_basis, fdnames=weatherfd$fdnames)
    
    betalist  <- list(const=fdPar(beta0), weatherfd=fdPar(beta1))
    
    fRegressout <- fRegress(tempfd, xfdlist, betalist)
    
    const_valid <- fd(1, basisobj = create.constant.basis(c(1,23)))
    const_valid$coefs  <- matrix(rep(1, 3), 
                                 nrow = 1, 
                                 dimnames = list('time', 
                                                 c('reps 1', 'reps 2', 'reps 3')))
    const_valid$fdnames$reps <- c('reps 1', 'reps 2', 'reps 3')
    
    preds <- eval.fd(1:23,predict.fRegress(fRegressout,
                                           newdata = list(const = const_valid,
                                                          weatherfd=weatherfd_test)))
    
    test_tempfd <- smooth.basis(temptime, test_temperature_Y, tempbasis, method = 'qr')$fd
    
    smoothed_test_temperature_Y <- eval.fd(1:23,test_tempfd)
    
    #squared error
    #calculated_error <- sqrt(mean(((preds - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    calculated_error <- 100*mean(abs(preds - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    
    simple_data <- data.frame(X = as.numeric(train_temperature_X), 
                              Y = as.numeric(train_temperature_Y))
    simple_model <- lm(Y~X, data = simple_data)
    simple_preds <- predict(simple_model, newdata = list(X = as.numeric(test_temperature_X)))
    
    #squared error
    #simple_calculated_error <- sqrt(mean(((simple_preds - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    simple_calculated_error <- 100*mean(abs(simple_preds - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    
    #squared error
    #weather_calculated_error <- sqrt(mean(((test_temperature_X - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    weather_calculated_error <- 100*mean(abs(test_temperature_X - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    
    row_of_results <- data.frame(location = loc, 
                                 beta_basis = beta_bk,
                                 fda_error_value = calculated_error,
                                 simple_error_value = simple_calculated_error,
                                 weather_error_value = weather_calculated_error)
    
    results <- rbind(results, row_of_results)
  }
  
}

#results %>%
#  ggplot(aes(length, fda_error_value)) +
#  geom_line() +
#  geom_line(aes(y = simple_error_value), col = 'green') +
#  geom_line(aes(y = weather_error_value), col = 'blue') +
#  geom_vline(xintercept = 4, col = 'red') +
#  facet_wrap(~location) +
#  theme_bw() +
#  scale_x_continuous(breaks = seq(from = 2, to = 12, by = 2))

#results %>%
#  group_by(length) %>%
#  mutate(total_fda_error_value = mean(fda_error_value),
#         total_simple_error_value = mean(simple_error_value),
#         total_weather_error_value = mean(weather_error_value)) %>%
#  ggplot(aes(length, total_fda_error_value)) +
#  geom_line() +
#  geom_line(aes(y = total_simple_error_value), col = 'green') +
#  geom_line(aes(y = total_weather_error_value), col = 'blue') +
#  geom_vline(xintercept = 4, col = 'red') +
#  theme_bw() +
#  scale_x_continuous(breaks = seq(from = 2, to = 12, by = 2))

results %>%
  pivot_longer(fda_error_value:weather_error_value) %>%
  ggplot(aes(beta_basis, value, group = name, col = name)) +
  geom_smooth(se = F) +
  geom_vline(xintercept = 4, col = 'black') +
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 3, to = 21, by = 2))

results %>%
  group_by(beta_basis) %>%
  mutate(total_fda_error_value = mean(fda_error_value),
         total_simple_error_value = mean(simple_error_value),
         total_weather_error_value = mean(weather_error_value)) %>%
  pivot_longer(total_fda_error_value:total_weather_error_value) %>%
  ggplot(aes(beta_basis, value, group = name, col = name)) +
  geom_smooth(se = F) +
  geom_vline(xintercept = 4, col = 'black') +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 3, to = 21, by = 2))

