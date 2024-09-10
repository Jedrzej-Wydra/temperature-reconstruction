temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()

for(loc in unique(data$location))
{
  
  for(bk in seq(from = 4, to = 21, by = 2))
  {
    tempbasis <- create.bspline.basis(temprange, norder=bk)
    weatherbasis <- create.bspline.basis(temprange, norder=bk)
    
    tempfd <- smooth.basis(temptime, validation_train_sets[[loc]]$Y, tempbasis, method = 'qr')$fd
    weatherfd <- smooth.basis(temptime, validation_train_sets[[loc]]$X, weatherbasis, method = 'qr')$fd
    
    weatherfd_valid <- smooth.basis(temptime, validation_test_sets[[loc]]$X, weatherbasis, method = 'qr')$fd
    
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
                                                          weatherfd=weatherfd_valid)))
    
    squared_error <- sqrt(mean(((preds - validation_test_sets[[loc]]$Y)^2)))
    
    row_of_results <- data.frame(location = loc, 
                                 n_basis = bk, 
                                 error_value = squared_error)
    
    results <- rbind(results, row_of_results)
  }
  
}

results %>%
  group_by(location) %>%
  ggplot(aes(n_basis, error_value)) +
  geom_line() +
  geom_vline(xintercept = 9, color = 'red') +
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 3, to = 21, by = 2))

results %>%
  group_by(n_basis) %>%
  mutate(total_error_value = sum(error_value)) %>%
  ggplot(aes(n_basis, total_error_value)) +
  geom_line() +
  geom_vline(xintercept = 11, color = 'red') +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 3, to = 21, by = 2))
