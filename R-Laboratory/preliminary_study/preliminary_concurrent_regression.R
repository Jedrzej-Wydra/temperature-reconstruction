library(fda)
library(tidyverse)
library(lubridate)

results <- data.frame()

par(mfrow = c(3, 3))

bk <- 4

for(loc in unique(data$location))
#for(bk in 2:9)
{

  temp <- 
    data %>%
      filter(location == loc) %>%
      dplyr::select(-location) %>%
      transmute(rep = paste0('day_', day(date) - 1),
                value = Y,
                hour = hour(date)) %>% 
      filter(hour != 0) %>%
      #mutate(hour = ifelse(hour < 5, hour + 23, hour)) %>% 
      arrange(rep, hour) %>% 
      pivot_wider(names_from = rep, values_from = value) %>%
      dplyr::select(-hour, -day_16) %>%
      as.matrix()
  
  weather <- 
    data %>%
    filter(location == loc) %>%
    dplyr::select(-location) %>%
    transmute(rep = paste0('day_', day(date) - 1),
              value = X,
              hour = hour(date)) %>% 
    filter(hour != 0) %>%
    #mutate(hour = ifelse(hour < 5, hour + 23, hour)) %>% 
    pivot_wider(names_from = rep, values_from = value) %>%
    dplyr::select(-hour, -day_16) %>%
    as.matrix()
  
  simple_model_data <- data.frame(X = as.numeric(weather), Y = as.numeric(temp))
  simple_model <- predict(lm(Y ~ X, simple_model_data))
      
  #temptime <- 5:27
  #temprange <- c(5, 27)
  temptime <- 1:23
  temprange <- c(1, 23)
  tempbasis <- create.fourier.basis(temprange, nbasis=bk)
  weatherbasis <- create.fourier.basis(temprange, nbasis=bk)

  tempfd <- smooth.basis(temptime, temp, tempbasis, method = 'qr')$fd
  weatherfd <- smooth.basis(temptime, weather, weatherbasis, method = 'qr')$fd
  #plot(tempfd)
  #plot(weatherfd)
  
  const  <- rep(1, dim(tempfd$coef)[2])
  xfdlist  <- list(const=const, weatherfd=weatherfd)
  
  beta0 <- with(tempfd, fd(basisobj=basis, fdnames=fdnames))
  beta1 <- with(weatherfd, fd(basisobj=basis, fdnames=fdnames))
  
  betalist  <- list(const=fdPar(beta0), weatherfd=fdPar(beta1))
  
  fRegressout <- fRegress(tempfd, xfdlist, betalist)
  
  #plot(fRegressout$betaestlist$const, main = 'b')
  #plot(fRegressout$betaestlist$weatherfd, main = 'a')
  
  simple_results <- round(sum((simple_model - as.numeric(temp))^2), 2)
  weather_results <- round(sum((as.numeric(weather) - as.numeric(temp))^2), 2)
  #results <- sum((eval.fd(5:27, predict.fRegress(fRegressout)) - temp)^2)
  results <- sum((eval.fd(1:23, predict.fRegress(fRegressout)) - temp)^2)
  results <- round(results, 2)
  
  #main <- paste('fda:', results, '|', "lm:", simple_results)
  
  #main <- round(simple_results / results, 2)
  
  main <- paste(round(100 * (simple_results - results) / simple_results, 2), '%')
  xlab <- paste(round(100 * (weather_results - results) / weather_results, 2), '%')
  xlab <- paste('weather', xlab)
  
  plot(as.numeric(temp), 
       type = 'l', 
       ylab = "temp",
       xlab = xlab,
       ylim = c(10, 40),
       main = main)
  #lines(as.numeric(eval.fd(5:27, predict.fRegress(fRegressout))),
  lines(as.numeric(eval.fd(1:23, predict.fRegress(fRegressout))),
        col = 'red')
  lines(simple_model,
        col = 'blue')
  #lines(as.numeric(weather), col = 'green')
  
  #results <- rbind(results,
  #                 c(bk, sum((eval.fd(5:27, 
  #                                    predict.fRegress(fRegressout)) - temp)^2)))

  #results <- rbind(results,
  #                 c(loc, sum((eval.fd(5:27, 
  #                                    predict.fRegress(fRegressout)) - temp)^2)))
  
}

#names(results) <- c('n_basis', 'error')

#names(results) <- c('location', 'error')

#results %>% arrange(error)

#data %>%
#  mutate(hour = hour(date)) %>%
#  filter(hour != 0) %>%
#  ggplot(aes(x = date, y = Y)) +
#  geom_line() +
#  geom_line(aes(y = X), col = 'green') +
#  facet_wrap(~location) +
#  theme_bw()
