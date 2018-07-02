library(permute)

# assume some level p
p <- 4
# assume some level q
q <- 2

x <- p + q
y <- p - q


# number of simulated paths
nSim <- 10000

# create a storage matrix for the evolution of random walk
simulatedRWs <- matrix(NA, nSim, x)
cumSumSimRWs <- matrix(NA, nSim, x)

for (i in 1: nSim){
  simulatedRWs[i, ] <- rbinom(x,1,p/x)
  simulatedRWs[i, simulatedRWs[i, ] == 0 ] <- -1
  
  cumSumSimRWs[i, ] <- cumsum(simulatedRWs[i, ])
}

uniqueRWs <- unique(cumSumSimRWs[cumSumSimRWs[,x] == (p-q), ])
numberUniqueRWs <- dim(unique(cumSumSimRWs[cumSumSimRWs[,x] == (p-q), ]))[1]

positiveOnes <- uniqueRWs[apply(uniqueRWs > 0, 1,sum) == x, ]
numberPosRWs <- dim(positiveOnes)[1]

numberPosRWs / numberUniqueRWs

matplot(t(uniqueRWs), type = "l")




## the combinatorial approach
# assume some level p
p <- 3
# assume some level q
q <- 2

x <- p + q
y <- p - q

# the combinations of p out of x places
numCombn <- dim(combn(1:x, p))[2]
combnRWs <- matrix(-1, numCombn, x)
cumSumCombnRWs <- matrix(NA, numCombn, x)

allCombs <- combn(1:x, p)

for (i in 1:numCombn){
  combnRWs[i, allCombs[ ,i]] <- 1
  cumSumCombnRWs[i, ] <- cumsum(combnRWs[i, ])
}

cumSumCombnRWs<-cbind( rep(0, dim(cumSumCombnRWs)[1]), cumSumCombnRWs) 

# first passage over y in 2n - y 
y <- 15

mostExpectedPeriod <- NULL
yOptions <- 1:100

for(y in yOptions){
  
  n <- y:2500
  
  nForPlot <- c(rep(0,y-1), n)
  
  f2ny <- (y/(2*n-y)) * choose(2*n-y,n) * 2^(-2*n+y)
  fForPlot <- c(rep(0,y-1), f2ny)
  
  plot(nForPlot,fForPlot)
  
  nMax <- which(fForPlot == max(fForPlot))[1]
  
  # so the level y is the most expected in 2 * nMax - y periods 
  mostExpectedPeriod[y] <- 2 * nMax - y
}

plot(yOptions, mostExpectedPeriod)

lines(n, f2ny)




matplot((cumSumCombnRWs[444,]), type = "l", lwd = 4, xlab= "time", ylab = "wealth", main = "Random Walk")

p<-1:10
q<-1:10


(p-q)/(p+q)


