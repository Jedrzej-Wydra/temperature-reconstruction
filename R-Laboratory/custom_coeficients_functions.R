custom_coefs <- function(eval, fdobj)
{
  nobs <- dim(coef(fdobj))[2]
  coefs <- coef(fdobj)
  
  a0 <- fdobj$basis$rangeval[1]
  a1 <- fdobj$basis$rangeval[2]
  
  p <- a1 - a0
  
  vector <- c()
  
  for(i in 1:nobs)
  {
    for(j in 1:a1)
    {
      ind <- c(0,1,1)
      cfs <- coef(fdobj)[,i]
      b1 <- rep(1, a1)
      b2 <- sin(2*pi*(a0:a1)/p)
      b3 <- cos(2*pi*(a0:a1)/p)
      basis <- matrix(c(b1,b2,b3), nrow = 3, byrow = T)
      
      vector[j] <- sum((ind*cfs*basis)[,j])

    }
    
    adj <- mean((eval[,i] - mean(eval[,i]))/(vector - mean(vector)))
    
    
    coefs[2,i] <- adj*coefs[2,i]
    coefs[3,i] <- adj*coefs[3,i]
    
    coefs[1,i] <- mean(eval[,i] - adj*vector)
    
  }
  
  return(coefs)
}

custom_eval <- function(x, a0 = 1, a1 = 23)
{
  p <- a1 - a0
  b1 <- rep(1, a1)
  b2 <- sin(2*pi*(a0:a1)/p)
  b3 <- cos(2*pi*(a0:a1)/p)
  basis <- matrix(c(b1,b2,b3), nrow = 3, byrow = T)
  nobs <- dim(x)[2]
  ret <- NULL
  for(i in 1:nobs)
  {
    ret <- cbind(ret, t(basis) %*% x[,i])
  }
  
  return(ret)
}


