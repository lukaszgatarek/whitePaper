# length of random walk
T <- 100
# number of simulations of random walk
nSim<-1000
randomWalkRecordings <- matrix(NA, nSim, T)
J <- 10

# beingInPositive is avariable indicating percentage of time that the random walk goes into the positive area
beingInPositive <- matrix(NA, nSim,J)
for (j in 1:J){
  prob <- 0.50 + 3*j/100 
  
  for (i in 1:nSim){
  
    eps <- rbinom(T, 1, prob)
    eps[eps == 0] <- -1
    
    randomWalkRecordings[i, ] <- cumsum(eps) 
    
    beingInPositive[i,j] <- sum(randomWalkRecordings[i,] > 0) / T
      
  }
}
plot(ecdf(beingInPositive[,1]), do.points=F, lty=1)
for (j in 2:J){
  lines(ecdf(beingInPositive[,j]), do.points=F)
}
p <- (1:100) /100 
cdf <- 2/pi * asin(sqrt(p))
lines(p, cdf, col = "red", type = "l")







#matplot(t(randomWalkRecordings), type = "l")

# plot the pdfs
hist(beingInPositive[,1], breaks = (0:20)/20 )
plot(density(beingInPositive[,1], from = 0, to= 1))

f <- 1 / ( pi * sqrt(p * (1-p)) )
lines(p, T * f, col = "red", type = "l") # according to formula 4.3 p 80 from Feller


plot(density(beingInPositive[,1]), from =0, to=1)
for (j in 2:J){
      lines(density(beingInPositive[,j]), from =0, to=1)
}


