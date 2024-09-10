library(stringr)
temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
short_reconstruction <- data.frame()

for(loc in setdiff(unique(data$location), "basement"))
{
  
  print(loc)
  
#  for(j in c(1, 2, 4, 8, 16, 23))
  for(j in 1:23)
  {
    weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
    
    train_temperature_Y <- train_sets[[loc]]$Y[1:j,12]
    train_temperature_X <- train_sets[[loc]]$X[,12]
    
    weather_data_mean <- mean(train_sets[[loc]]$X)
    
    test_temperature_Y <- test_sets[[loc]]$Y
    test_temperature_X <- test_sets[[loc]]$X
    
    weatherfd <- smooth.basis(temptime, train_temperature_X, weatherbasis, method = 'qr')$fd
    weather_eval <- eval.fd(1:23,weatherfd)
    
    known_amp <- filter(characteristics_of_amplitude_distribution, location == loc)
    
    b1_c1 <- expand.grid(sin = seq(-15, 5, 0.01), 
                         cos = seq(-15, 5, 0.01)) %>%
      mutate(amplitude = round(sqrt(sin^2 + cos^2), 2)) %>%
      filter(amplitude > (known_amp$mean - 2*known_amp$sd),
             amplitude < (known_amp$mean + 2*known_amp$sd)) %>%
      mutate(log_prob = dnorm(amplitude, known_amp$mean, known_amp$sd, log = TRUE)) %>%
      dplyr::select(-amplitude)
    
    
    a1 <- round(weather_data_mean, 2) + seq(from = 0, to = 4, by = 1)
    
    grid <- cbind(const = round(weather_data_mean, 2), b1_c1, log_prob_unif = log(0.2))
    
    for(a1 in (round(weather_data_mean, 2) + 1:4))
    {
      grid <- bind_rows(grid, bind_cols(const = a1, b1_c1, log_prob_unif = log(0.2)))
    }
    
    set.seed(2024)
    
    grid <- grid[sample(1:dim(grid)[1], dim(grid)[1] %/% 1000),]
    
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
                bayes = log_prob + 
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
    
    coefsy <- NULL
    #linear regression
    for(rep in unique(table$name))
    {
      table %>%
        filter(name == rep) %>%
        mutate(Xw = train_temperature_X) %>%
        lm(value ~ Xw, data = .) %>%
        coef() -> coefs_obs
      
      coefsy <- rbind(coefsy, c(coefs_obs, min(filter(table, name == rep)$bayes)))
      
    }
    rownames(coefsy) <- NULL
    final <- NULL
    for(i in 1:dim(coefsy)[1])
    {
      final <- rbind(final,
                     data.frame(ind = i,
                                X = c(1:69),
                                bayes = coefsy[i,3],
                                recon = coefsy[i,2]*as.numeric(test_temperature_X) + coefsy[i,1]))
      
      
    }
    the_best_one <- NULL
    for(i in 1:dim(coefsy)[1])
    {
      data_best <- final %>%
        filter(ind == i)
      #calculated_error <- sqrt(mean(((data_best$recon - as.numeric(test_temperature_Y))^2)))
      #percentage absolute error
      calculated_error <- 100*mean(abs(data_best$recon - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
      
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
      filter(bayes == max(bayes))
    
    best_reconstruction <- final %>%
      filter(ind == (the_best_one %>% filter(error == min(error)) %>% pull(ind)))
    
    old_recon <- data.frame(X = 1:69,
                            Y = simple_recon[2]*as.numeric(test_temperature_X) + simple_recon[1])
    
    #squared error
    #calculated_error <- sqrt(mean(((reconstruction$recon - as.numeric(test_temperature_Y))^2)))
    #percentage absolute error
    calculated_error <- 100*mean(abs(reconstruction$recon - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
    
    #squared error
    #calculated_error_simple_recon <- sqrt(mean(((old_recon$Y - as.numeric(test_temperature_Y))^2)))
    #percentage absolute error
    calculated_error_simple_recon <- 100*mean(abs(old_recon$Y - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
    
    #squared error
    #calculated_error_weather <- sqrt(mean(((as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))^2)))
    #percentage absolute error
    calculated_error_weather <- 100*mean(abs(as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
    
    row_of_results <- data.frame(location = loc, 
                                 length = j, 
                                 bayes_error_value = calculated_error,
                                 best_bayes_value = the_best_error,
                                 simple_error_value = calculated_error_simple_recon,
                                 weather_error_value = calculated_error_weather)
    
    short_reconstruction <- rbind(short_reconstruction, 
                                  row_of_results)
    
#    final %>% 
#      ggplot(aes(X, recon, group = ind, color = bayes)) +
#      #geom_line() +
#      geom_line(data = data.frame(X = 1:69,
#                                  Y = as.numeric(test_temperature_Y)),
#                aes(X, Y, group = 1),
#                color = 'red') +
#      geom_line(data = data.frame(X = 1:69,
#                                  Y = as.numeric(test_temperature_X)),
#                aes(X, Y, group = 0),
#                color = 'green') +
#      geom_line(data = reconstruction,
#                aes(group = "a"),
#                color = "orange") +
#      geom_line(data = old_recon,
#                aes(X, Y, group = "b"),
#                color = 'violet') +
#      geom_line(data = best_reconstruction,
#                aes(group = 'c'),
#                color = 'black') +
#      theme_bw() +
#      theme(legend.position = 'none')
    
    
    
    
  }
  
  
}

loc <- "basement"

for(j in c(1, 2, 4, 8, 16, 23))
{
  
  train_temperature_Y <- train_sets[[loc]]$Y[1:j,12]
  train_temperature_X <- train_sets[[loc]]$X[,12]
  
  test_temperature_Y <- test_sets[[loc]]$Y
  test_temperature_X <- test_sets[[loc]]$X
  
  lm(train_temperature_Y ~ train_temperature_X[1:length(train_temperature_Y)]) %>%
    coef() -> simple_recon
  
  names(simple_recon) <- c("b", "a")
  
  reconstruction_mean <- mean(train_temperature_Y)
  
  old_recon <- data.frame(X = 1:69,
                          Y = simple_recon[2]*as.numeric(test_temperature_X) + simple_recon[1])
  
  #squared error
  #calculated_error <- sqrt(mean(((reconstruction_mean - as.numeric(test_temperature_Y))^2)))
  #percentage absolute error
  calculated_error <- 100*mean(abs(reconstruction_mean - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
  
  #squared error
  #calculated_error_simple_recon <- sqrt(mean(((old_recon$Y - as.numeric(test_temperature_Y))^2)))
  #percentage absolute error
  calculated_error_simple_recon <- 100*mean(abs(old_recon$Y - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
  
  #squared error
  #calculated_error_weather <- sqrt(mean(((as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))^2)))
  #percentage absolute error
  calculated_error_weather <- 100*mean(abs(as.numeric(test_temperature_X) - as.numeric(test_temperature_Y))/as.numeric(test_temperature_Y))
  
  row_of_results <- data.frame(location = loc, 
                               length = j, 
                               bayes_error_value = calculated_error,
                               best_bayes_value = calculated_error,
                               simple_error_value = calculated_error_simple_recon,
                               weather_error_value = calculated_error_weather)
  
  short_reconstruction <- rbind(short_reconstruction, 
                                row_of_results)
  
  
}

short_reconstruction %>%
  pivot_longer(bayes_error_value:weather_error_value) %>%
  ggplot(aes(length, value, color = name)) +
  geom_line() +
  facet_wrap(~location, scales = "free_y") +
  theme_bw()

short_reconstruction %>%
  pivot_longer(bayes_error_value:weather_error_value) %>%
  group_by(length, name) %>%
  summarise(value = mean(value)) %>%
  #filter(location != 'basement') %>%
  #filter(name != 'simple_error_value') %>%
  ggplot(aes(length, value, color = name)) +
  geom_line() +
  #geom_smooth(se = F) +
  theme_bw()
