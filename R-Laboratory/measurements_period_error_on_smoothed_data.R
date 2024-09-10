library(gam)
temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
values <- data.frame()

for(loc in unique(data$location)[-c(2,3,4)])
{
  
  for(len in 1:11)
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
    #preds <- eval.fd(1:23,predict.fRegress(fRegressout))
    
    test_tempfd <- smooth.basis(temptime, test_temperature_Y, tempbasis, method = 'qr')$fd
    
    smoothed_test_temperature_Y <- eval.fd(1:23,test_tempfd)
    
    #squared error
    #calculated_error <- sqrt(mean(((preds - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    #calculated_error <- 100*mean(abs(preds - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    #calculated_error <- 100*mean(abs(preds - train_temperature_Y)/abs(train_temperature_Y))
    #MAE
    calculated_error <- mean(abs(preds - smoothed_test_temperature_Y))
    
    simple_data <- data.frame(X = as.numeric(train_temperature_X), 
                              Y = as.numeric(train_temperature_Y))
    simple_model <- lm(Y~X, data = simple_data)
    simple_preds <- predict(simple_model, newdata = list(X = as.numeric(test_temperature_X)))
    #simple_preds <- predict(simple_model)
    
    gam_model <- gam(Y~lo(X), data = simple_data)
    gam_preds <- predict(gam_model, newdata = list(X = as.numeric(test_temperature_X)))
    #gam_preds <- predict(gam_model)
    
    #squared error
    #gam_calculated_error <- sqrt(mean(((gam_preds - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    #gam_calculated_error <- 100*mean(abs(gam_preds - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    #gam_calculated_error <- 100*mean(abs(gam_preds - as.numeric(train_temperature_Y))/abs(as.numeric(train_temperature_Y)))
    #MAE
    gam_calculated_error <- mean(abs(gam_preds - smoothed_test_temperature_Y))
    
    
    #squared error
    #simple_calculated_error <- sqrt(mean(((simple_preds - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    #simple_calculated_error <- 100*mean(abs(simple_preds - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    #simple_calculated_error <- 100*mean(abs(simple_preds - as.numeric(train_temperature_Y))/abs(as.numeric(train_temperature_Y)))
    #MAE
    simple_calculated_error <- mean(abs(simple_preds - smoothed_test_temperature_Y))
    
    #squared error
    #weather_calculated_error <- sqrt(mean(((test_temperature_X - smoothed_test_temperature_Y)^2)))
    #percentage absolute error
    #weather_calculated_error <- 100*mean(abs(test_temperature_X - smoothed_test_temperature_Y)/abs(smoothed_test_temperature_Y))
    #MAE
    weather_calculated_error <- mean(abs(test_temperature_X - smoothed_test_temperature_Y))
    
    row_of_results <- data.frame(location = loc, 
                                 length = len + 1, 
                                 fda_error_value = calculated_error,
                                 simple_error_value = simple_calculated_error,
                                 gam_error_value = gam_calculated_error,
                                 weather_error_value = weather_calculated_error)
    
    results <- rbind(results, row_of_results)
    
    row_of_values <- data.frame(
      len = len + 1,
      X = 1:69,
      location = loc,
      weather = as.numeric(test_temperature_X),
      fda = as.numeric(preds),
      measurements = as.numeric(smoothed_test_temperature_Y),
      linear = as.numeric(simple_preds))
    
    values <- bind_rows(values, row_of_values)
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

# 900 x 500

results %>%
  dplyr::select(-gam_error_value) %>% 
  transmute(location, length, MTM = fda_error_value, LM = simple_error_value,
            'no correction' = weather_error_value) %>%
  pivot_longer(MTM:'no correction') %>% 
  transmute(location, length, model = name, value) %>%
  ggplot(aes(length, value, group = model, col = model)) +
  geom_smooth(se = F) +
  geom_vline(xintercept = 6, col = 'black') +
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 2, to = 12, by = 2)) +
  xlab("Length of Measurements Period [days]") +
  ylab("Mean Absolute Error [°C]") +
  scale_color_manual(values=c('#00BA38', "#F8766D", "#619CFF"))

group_func <- function(x)
{
  ifelse(x %in% c('attic','roof'), 'roof/attic',
         ifelse(x %in% c('forest', 'field'), 'forest/field',
                ifelse(x %in% c('garage', 'shack', 'underground', 'uninhabited_building'), 'other',
                       'basement')))
}

results %>%
  pivot_longer(fda_error_value:weather_error_value) %>%
  mutate(group = group_func(location)) %>%
  ggplot(aes(length, value, group = name, col = name)) +
  geom_smooth(se = F) +
  #geom_vline(xintercept = 4, col = 'black') +
  facet_wrap(~group) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 2, to = 12, by = 2))

results %>%
  group_by(length) %>%
  mutate(total_fda_error_value = mean(fda_error_value),
         total_simple_error_value = mean(simple_error_value),
         total_weather_error_value = mean(weather_error_value),
         total_gam_error_value = mean(gam_error_value)) %>%
  pivot_longer(total_fda_error_value:total_gam_error_value) %>%
  ggplot(aes(length, value, group = name, col = name)) +
  geom_smooth(se = F) +
  #geom_vline(xintercept = 4, col = 'black') +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 2, to = 12, by = 2))

values %>%
  filter(len == 12) %>%
  ggplot(aes(X, measurements)) +
  geom_line() +
  geom_line(aes(y = linear), col = '#00BA38') +
  geom_line(aes(y = fda), col = '#F8766D') +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("Measurements Period of 12 days") +
  ylab("Temperature [°C]") +
  xlab("Duration of Measurements [hours]")

values %>%
  filter(len == 12) %>%
  ggplot(aes(X, measurements)) +
  geom_line() +
  geom_line(aes(y = fda), col = '#F8766D') +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("crm, measurements period of 12 days")

values %>%
  filter(len == 12) %>%
  ggplot(aes(X, measurements)) +
  geom_line() +
  geom_line(aes(y = weather), col = '#619CFF') +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("No Correction") + 
  ylab("Temperature [°C]") +
  xlab("Duration of Measurements [hours]")


results %>%
  group_by(length) %>%
  mutate(total_fda_error_value = mean(fda_error_value),
         total_simple_error_value = mean(simple_error_value),
         total_weather_error_value = mean(weather_error_value),
         total_gam_error_value = mean(gam_error_value)) %>%
  pivot_longer(total_fda_error_value:total_weather_error_value) %>%
  ggplot(aes(length, value, group = name, col = name)) +
  geom_smooth(se = F) +
  #geom_vline(xintercept = 4, col = 'black') +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 2, to = 12, by = 2)) 

values %>% 
  filter(len == 12, X <24, location == "shack") %>%
  ggplot(aes(X, measurements)) +
  geom_point() +
  geom_line(aes(y = fda), col = '#F8766D') +
  theme_bw() +
  ggtitle("No Correction") + 
  ylab("Temperature [°C]") +
  xlab("Duration of Measurements [hours]")

