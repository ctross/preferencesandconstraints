 rdirichlet <- function(n, alpha) {
  normalize <- function(.) . / sum(.)
  samps <- vapply(alpha, function(al) rgamma(n, al, 1), numeric(n))
  if (n == 1) {
    matrix(normalize(samps), nrow = 1, ncol = length(samps))
  } else {
    t(apply(samps, 1, normalize))
  }  
}

 J <- 12
  
 Theta <-rdirichlet(1, c(rep(1,J-1),4))
 Theta2 <- Theta
 Theta2[J] <- 0

 l <- vector('list',J)
 for(i in 1:(J-1)) l[[i]] <- c(0,1)
 l[[J]] <- 0

 x <- expand.grid(l) 
 x[,J] <- J-rowSums(x)

 z <- rep(NA, 2^(J-1))
 
 for(i in 1:2^(J-1))
 z[i] <- prod(Theta^x[i,])
 
 
 print(sum(z))
 print(prod(Theta[J]+Theta2)) 
 
  print(log(sum(z)))
  print(log(prod(Theta[J]+Theta2))) 
  print(log(Theta[J]) + sum(log(Theta[J] + Theta[1:(J-1)])))


















