temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
beta_functions <- data.frame()

for(loc in unique(data$location))
{
  tempbasis <- create.fourier.basis(temprange, nbasis=bk)
  weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
  
  train_temperature_Y <- train_sets[[loc]]$Y
  train_temperature_X <- train_sets[[loc]]$X
  
  test_temperature_Y <- test_sets[[loc]]$Y
  test_temperature_X <- test_sets[[loc]]$X
  
  tempfd <- smooth.basis(temptime, train_temperature_Y, tempbasis, method = 'qr')$fd
  weatherfd <- smooth.basis(temptime, train_temperature_X, weatherbasis, method = 'qr')$fd
  
  weatherfd_test <- smooth.basis(temptime, test_temperature_X, weatherbasis, method = 'qr')$fd
  
  const <- rep(1, dim(tempfd$coef)[2])
  
  xfdlist  <- list(const=const, weatherfd=weatherfd)
  
  beta0 <- with(tempfd, fd(basisobj=basis, fdnames=fdnames))
  beta1 <- with(weatherfd, fd(basisobj=basis, fdnames=fdnames))
  
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
  calculated_error <- 100*mean(abs(preds - smoothed_test_temperature_Y)/smoothed_test_temperature_Y)
  
  simple_data <- data.frame(X = as.numeric(train_temperature_X), 
                            Y = as.numeric(train_temperature_Y))
  simple_model <- lm(Y~X, data = simple_data)
  simple_preds <- predict(simple_model, newdata = list(X = as.numeric(test_temperature_X)))
  
  #squared error
  #simple_calculated_error <- sqrt(mean(((simple_preds - smoothed_test_temperature_Y)^2)))
  #percentage absolute error
  simple_calculated_error <- 100*mean(abs(simple_preds - smoothed_test_temperature_Y)/smoothed_test_temperature_Y)
  
  #squared error
  #weather_calculated_error <- sqrt(mean(((test_temperature_X - smoothed_test_temperature_Y)^2)))
  #percentage absolute error
  weather_calculated_error <- 100*mean(abs(test_temperature_X - smoothed_test_temperature_Y)/smoothed_test_temperature_Y)
  
  row_of_results <- data.frame(location = loc, 
                               fda_error_value = calculated_error,
                               simple_error_value = simple_calculated_error,
                               weather_error_value = weather_calculated_error)
  
  results <- rbind(results, row_of_results)
  
  current_beta <- data.frame(location = loc,
                             X = seq(1, 23, by = 0.001),
                             Y = eval.fd(seq(1, 23, by = 0.001),
                                         fRegressout$betaestlist$weatherfd$fd),
                             beta = "beta_weather")
  
  current_beta <- rbind(current_beta, 
                        data.frame(location = loc,
                                   X = seq(1, 23, by = 0.001),
                                   Y = eval.fd(seq(1, 23, by = 0.001),
                                               fRegressout$betaestlist$const$fd),
                                   beta = "beta_const"))
  
  beta_functions <- rbind(beta_functions, current_beta)
}

beta_functions %>%
  filter(beta == "beta_weather") %>%
  ggplot(aes(X, Y)) +
  geom_line() +
  facet_wrap(~ location) +
  theme_bw()

beta_functions %>%
  filter(beta == "beta_const") %>%
  ggplot(aes(X, Y)) +
  geom_line() +
  facet_wrap(~ location) +
  theme_bw()



