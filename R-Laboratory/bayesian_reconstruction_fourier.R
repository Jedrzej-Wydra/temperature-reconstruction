library(stringr)
temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
short_reconstruction_fourier <- data.frame()
values <- data.frame()

for(loc in setdiff(unique(data$location), "basement")[-c(2,3)])
{
  
  print(loc)
  
  #  for(j in c(1, 2, 4, 8, 16, 23))
  for(j in 1:23)
  {
    weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
    tempbasis <- create.fourier.basis(temprange, nbasis=bk)
    
    train_temperature_Y <- train_sets[[loc]]$Y
    train_temperature_X <- train_sets[[loc]]$X
    
    tempfd <- smooth.basis(temptime, train_temperature_Y, tempbasis, method = 'qr')$fd
    weatherfd <- smooth.basis(temptime, train_temperature_X, weatherbasis, method = 'qr')$fd
    
    train_temperature_Y <- eval.fd(1:23,tempfd)[1:j,12]
    train_temperature_X <- eval.fd(1:23,weatherfd)[,12]
    
    test_temperature_Y <- test_sets[[loc]]$Y
    test_temperature_X <- test_sets[[loc]]$X
    
    test_tempfd <- smooth.basis(temptime, test_temperature_Y, tempbasis, method = 'qr')$fd
    test_weatherfd <- smooth.basis(temptime, test_temperature_X, tempbasis, method = 'qr')$fd
    
    test_temperature_Y <- eval.fd(1:23,test_tempfd)
    test_temperature_X <- eval.fd(1:23,test_weatherfd)
    
    weather_data_mean <- mean(eval.fd(1:23,weatherfd))
    
    known_amp <- filter(characteristics_of_amplitude_distribution, location == loc)
    
    b1_c1 <- expand.grid(sin = seq(-15, 5, 0.01), 
                         cos = seq(-15, 5, 0.01)) %>%
      mutate(amplitude = round(sqrt(sin^2 + cos^2), 2)) %>%
      filter(amplitude > (known_amp$mean - 2*known_amp$sd),
             amplitude < (known_amp$mean + 2*known_amp$sd)) %>%
      mutate(log_prob = dnorm(amplitude, known_amp$mean, known_amp$sd, log = TRUE)) %>%
      dplyr::select(-amplitude)
    
    #grid <- cbind(const = round(weather_data_mean, 2), b1_c1, log_prob_unif = log(0.2))
    
    #add <- 0:4
    
    add <- seq(from = -1, to = 5, by = 0.25)
    grid <- cbind(const = round(weather_data_mean, 2), b1_c1, log_prob_unif = log(1/length(add)))
    
    for(a1 in (round(weather_data_mean, 2) + add))
    {
      grid <- bind_rows(grid, bind_cols(const = a1, b1_c1, log_prob_unif = log(1/length(add))))
    }
    
    set.seed(2024)
    
    grid <- grid[sample(1:dim(grid)[1], dim(grid)[1] %/% 2000),]
    
    result <- NULL
    for(k in 1:nrow(grid))
    {
      cfsy <- t(grid[k,1:3])
      resid <- train_temperature_Y - as.numeric(custom_eval(cfsy))[1:length(train_temperature_Y)]
      result <- rbind(result,
                      data.frame(nrow = k,
                                 m_residuals = round(mean(resid), 2),
                                 sd_residuals = sd(resid),
                                 mse = round(mean(resid^2), 2),
                                 log_likelihood = sum(dnorm(resid, 0, sd(resid), log = TRUE))))
    }
    
    grid %>%
      mutate(nrow = row_number()) %>%
      inner_join(
        result %>%
          filter(mse < 10, m_residuals < 1, m_residuals > -1),
        by = "nrow"
      ) %>%
      dplyr::select(-nrow, -m_residuals) %>%
      mutate(log_prob_resid = dnorm(sd_residuals, 1, 0.75, log = TRUE)) %>%
      dplyr::select(-mse, -sd_residuals) -> bayesian_coefs
    
    if(loc == "roof")
    {
      grid %>%
        mutate(nrow = row_number()) %>%
        inner_join(
          result %>%
            filter(mse < 15, m_residuals < 1, m_residuals > -1),
          by = "nrow"
        ) %>%
        dplyr::select(-nrow, -m_residuals) %>%
        mutate(log_prob_resid = dnorm(sd_residuals, 1, 0.75, log = TRUE)) %>%
        dplyr::select(-mse, -sd_residuals) -> bayesian_coefs
    }
    
    
    bayesian_coefs %>%
      transmute(const, sin, cos,
                bayes = 0 + #log_prob + 
                  log_prob_unif + 
                  log_likelihood + 
                  log_prob_resid) %>%
      mutate(name = row_number(),
             bayes = rank(bayes)) %>%
      dplyr::select(name, bayes) -> bayes
    
    test_df <- data.frame(X = 1:length(train_temperature_Y), 
                          Y = train_temperature_Y)
    
    n_cols <- dim(t(bayesian_coefs[, 1:3]))[2]
    
    #    as.data.frame(custom_eval(t(bayesian_coefs[, 1:3])))[1:length(train_temperature_Y),] %>%
    #      mutate(X = 1:length(train_temperature_Y)) %>%
    #      pivot_longer("V1":paste0("V", n_cols)) %>%
    #      mutate(name = as.numeric(str_sub(name, start = 2))) %>%
    #      left_join(bayes, by = "name") -> table
    
    #    table %>%
    #      ggplot(aes(X, value, group = name, color = bayes)) +
    #      geom_line(alpha = 0.5) +
    #      geom_line(data = filter(table, bayes == max(bayes)), color = "orange") +
    #      geom_line(data = test_df, aes(X, Y, group = 0), color = 'red') +
    #      theme_bw() +
    #      geom_line(data = data.frame(X = 1:length(train_temperature_Y),
    #                                  Y = smoothed_full_data_temperature_Y[1:length(train_temperature_Y),15]),
    #                aes(X, Y, group = 1000), color = "green") 
    
    
    as.data.frame(custom_eval(t(bayesian_coefs[, 1:3]))) %>%
      mutate(X = 1:23) %>%
      pivot_longer("V1":paste0("V", n_cols)) %>%
      mutate(name = as.numeric(str_sub(name, start = 2))) %>%
      left_join(bayes, by = "name") -> table
    
    bayesian_coefs %>%
      dplyr::select(const, sin, cos) %>%
      mutate(name = row_number()) %>%
      left_join(bayes, by = "name") -> sample_coefs
      
    
    final <- NULL
    func_tran <- rbind(const_func, sin_func, cos_func)
    #fourier
    for(rep in 1:nrow(sample_coefs))
    {
      pre_recon <- matrix(as.numeric(sample_coefs[rep,1:3]), ncol = 1)
      row.names(pre_recon) <- c('const', 'sin', 'cos')
      row_coefsy <- cbind(pre_recon, pre_recon, pre_recon) * func_tran
      coefsy <- data.frame(X = 1:23,
                           custom_eval(row_coefsy),
                           name = sample_coefs[rep,4],
                           bayes = sample_coefs[rep,5])
      names(coefsy)[2:4] <- c('day_1', 'day_2', 'day_3')
      final <- rbind(final, coefsy)
    }
    
    the_best_one <- NULL
    for(i in 1:nrow(sample_coefs))
    {
      data_best <- final %>%
        filter(name == i) %>%
        dplyr::select(-X, -name, -bayes) %>%
        as.matrix()
      
      #calculated_error <- sqrt(mean(((data_best - test_temperature_Y)^2)))
      #percentage absolute error
      calculated_error <- 100*mean(abs(data_best - test_temperature_Y)/abs(test_temperature_Y))
      
      row_of_the_best <- data.frame(ind = i,
                                    error = calculated_error)
      the_best_one <- rbind(the_best_one,
                            row_of_the_best)
      
      
    }
    
    the_best_error <- the_best_one %>% filter(error == min(error)) %>% pull(error)
    
    lm(train_temperature_Y ~ train_temperature_X[1:length(train_temperature_Y)]) %>%
      coef() -> simple_recon
    
    names(simple_recon) <- c("b", "a")
    
    reconstruction <- final %>%
      filter(bayes == max(bayes)) %>%
      dplyr::select(-X, -name, -bayes) %>%
      as.matrix()
    
    best_reconstruction <- final %>%
      filter(name == (the_best_one %>% filter(error == min(error)) %>% pull(ind))) %>%
      dplyr::select(-X, -name, -bayes) %>%
      as.matrix()
    
    #old_recon <- data.frame(X = 1:69,
    #                        Y = simple_recon[2]*as.numeric(test_temperature_X) + simple_recon[1])
    
    old_recon <- data.frame(X = 1:69,
                            Y = simple_recon[2]*as.numeric(test_sets[[loc]]$X) + simple_recon[1])
    
    #squared error
    #calculated_error <- sqrt(mean(((reconstruction - test_temperature_Y)^2)))
    #percentage absolute error
    #calculated_error <- 100*mean(abs(reconstruction - test_temperature_Y)/abs(test_temperature_Y))
    #MAE
    calculated_error <- mean(abs(reconstruction - test_temperature_Y))
    
    #squared error
    #calculated_error_simple_recon <- sqrt(mean(((old_recon$Y - as.numeric(test_temperature_Y))^2)))
    #percentage absolute error
    #calculated_error_simple_recon <- 100*mean(abs(old_recon$Y - as.numeric(test_temperature_Y))/abs(as.numeric(test_temperature_Y)))
    #MAE
    #calculated_error_simple_recon <- mean(abs(old_recon$Y - as.numeric(test_temperature_Y)))
    calculated_error_simple_recon <- mean(abs(old_recon$Y - as.numeric(test_sets[[loc]]$Y)))
    
    #squared error
    #calculated_error_weather <- sqrt(mean(((as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))^2)))
    #percentage absolute error
    #calculated_error_weather <- 100*mean(abs(as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))/abs(as.numeric(test_temperature_Y)))
    #MAE
    #calculated_error_weather <- mean(abs(as.numeric(test_temperature_X) - as.numeric(test_temperature_Y)))
    calculated_error_weather <- mean(abs(as.numeric(test_temperature_X) - as.numeric(test_sets[[loc]]$Y)))
    
    row_of_results <- data.frame(location = loc, 
                                 length = j, 
                                 fourier_error_value = calculated_error,
                                 best_bayes_value = the_best_error,
                                 simple_error_value = calculated_error_simple_recon,
                                 weather_error_value = calculated_error_weather)
    
    short_reconstruction_fourier <- rbind(short_reconstruction_fourier, 
                                  row_of_results)
    
#        final %>% 
#          ggplot(aes(X, recon, group = ind, color = bayes)) +
#          #geom_line() +
#          geom_line(data = data.frame(X = 1:69,
#                                      Y = as.numeric(test_temperature_Y)),
#                    aes(X, Y, group = 1),
#                    color = 'red') +
#          geom_line(data = data.frame(X = 1:69,
#                                      Y = as.numeric(test_temperature_X)),
#                    aes(X, Y, group = 0),
#                    color = 'green') +
#          geom_line(data = data.frame(X = 1:69,
#                                      Y = as.numeric(reconstruction)),
#                    aes(X, Y, group = "a"),
#                    color = "orange") +
#          geom_line(data = old_recon,
#                    aes(X, Y, group = "b"),
#                    color = 'violet') +
#          geom_line(data = data.frame(X = 1:69,
#                                      Y = as.numeric(best_reconstruction)),
#                    aes(X, Y, group = 'c'),
#                    color = 'black') +
#          theme_bw() +
#          theme(legend.position = 'none')
    
    row_of_values <- data.frame(
      len = j,
      X = 1:69,
      location = loc,
      weather = as.numeric(test_temperature_X),
      fourier = as.numeric(reconstruction),
      measurements = as.numeric(test_temperature_Y),
      linear = as.numeric(old_recon$Y))
    
    values <- bind_rows(values, row_of_values)
    
    
  }
  
  
}

#loc <- "basement"

#  for(j in c(1, 2, 4, 8, 16, 23))
#for(j in 1:23)
#{
#  
#  train_temperature_Y <- train_sets[[loc]]$Y[1:j,12]
#  train_temperature_X <- train_sets[[loc]]$X[,12]
#  
#  test_temperature_Y <- test_sets[[loc]]$Y
#  test_temperature_X <- test_sets[[loc]]$X
#  
#  lm(train_temperature_Y ~ train_temperature_X[1:length(train_temperature_Y)]) %>%
#    coef() -> simple_recon
#  
#  names(simple_recon) <- c("b", "a")
#  
#  reconstruction_mean <- mean(train_temperature_Y)
#  
#  old_recon <- data.frame(X = 1:69,
#                          Y = simple_recon[2]*as.numeric(test_temperature_X) + simple_recon[1])
#  
#  #squared error
#  #calculated_error <- sqrt(mean(((reconstruction_mean - as.numeric(test_temperature_Y))^2)))
#  #percentage absolute error
#  calculated_error <- 100*mean(abs(reconstruction_mean - as.numeric(test_temperature_Y))/abs(as.numeric(test_temperature_Y)))
#  
#  #squared error
#  #calculated_error_simple_recon <- sqrt(mean(((old_recon$Y - as.numeric(test_temperature_Y))^2)))
#  #percentage absolute error
#  calculated_error_simple_recon <- 100*mean(abs(old_recon$Y - as.numeric(test_temperature_Y))/abs(as.numeric(test_temperature_Y)))
#  
#  #squared error
#  #calculated_error_weather <- sqrt(mean(((as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))^2)))
#  #percentage absolute error
#  calculated_error_weather <- 100*mean(abs(as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))/abs(as.numeric(test_temperature_Y)))
#  
#  row_of_results <- data.frame(location = loc, 
#                               length = j, 
#                               fourier_error_value = calculated_error,
#                               best_bayes_value = calculated_error,
#                               simple_error_value = calculated_error_simple_recon,
#                               weather_error_value = calculated_error_weather)
#  
#  short_reconstruction_fourier <- rbind(short_reconstruction_fourier, 
#                                row_of_results)
#  
#  row_of_values <- data.frame(
#    len = j,
#    X = 1:69,
#    location = loc,
#    weather = as.numeric(test_temperature_X),
#    fourier = as.numeric(reconstruction_mean),
#    measurements = as.numeric(test_temperature_Y),
#    linear = as.numeric(old_recon$Y))
#  
#  values <- bind_rows(values, row_of_values)
#  
#  
#  
#}#


short_reconstruction_fourier %>%
  pivot_longer(fourier_error_value:weather_error_value) %>%
  filter(name != 'best_bayes_value') %>%
  ggplot(aes(length, value, color = name)) +
  geom_line() +
  facet_wrap(~location) +
  theme_bw()

group_func <- function(x)
{
  ifelse(x %in% c('attic','roof'), 'roof/attic',
         ifelse(x %in% c('forest', 'field'), 'forest/field',
                ifelse(x %in% c('garage', 'shack', 'underground', 'uninhabited_building'), 'other',
                       'basement')))
}



short_reconstruction_fourier %>%
  pivot_longer(fourier_error_value:weather_error_value) %>%
  filter(name != 'best_bayes_value') %>%
  mutate(group = group_func(location)) %>%
  #filter(location != 'basement') %>%
  #group_by(length, name, group) %>%
  #summarise(value = mean(value)) %>%
  #filter(name != 'simple_error_value') %>%
  ggplot(aes(length, value, color = name)) +
  #geom_line() +
  geom_hline(yintercept = 6) +
  geom_smooth(se = F) +
  geom_vline(xintercept = 4) +
  theme_bw() +
  #facet_wrap(~group)
  facet_wrap(~location)
  #xlim(5, 23) +
  #ylim(0, 18)

short_reconstruction_fourier %>%
  transmute(location, length, STM = fourier_error_value, LM = simple_error_value,
            'no correction' = weather_error_value) %>%
  pivot_longer(STM:'no correction') %>% 
  transmute(location, length, model = name, value) %>%
  ggplot(aes(length, value, group = model, col = model)) +
  geom_smooth(se = F) +
  geom_vline(xintercept = 4, col = 'black') +
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 1, to = 23, by = 4)) +
  xlab("Length of Measurements Period [hours]") +
  ylab("Mean Absolute Error [°C]") +
  scale_color_manual(values=c('#00BA38', "#619CFF", "#F8766D"))

# 900 x 500

values %>%
  filter(len == 4) %>%
  ggplot(aes(X, measurements)) +
  geom_line() +
  geom_line(aes(y = linear), col = '#00BA38') +  
  geom_line(aes(y = fourier), col = '#F8766D') +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("Measurements Period of 4 hours") +
  ylab("Temperature [°C]") +
  xlab("Duration of Measurements [hours]")

values %>%
  filter(len == 4) %>%
  ggplot(aes(X, measurements)) +
  geom_line() +
  geom_line(aes(y = fourier), col = '#F8766D') +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("fm, measurements period of 4 hours")

values %>%
  filter(len == 4) %>%
  ggplot(aes(X, measurements)) +
  geom_line() +
  geom_line(aes(y = weather), col = '#619CFF') +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("no correction")






