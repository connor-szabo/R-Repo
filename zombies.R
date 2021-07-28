#initialize all values
S <- 0
Z <- 0
R <- 0
a <- 0
b <- 0
d <- 0
c <- 0
t <- 0
A <- 0
B <- 0
D <- 0
C <- 0
time <- 0

#Default values
S <- 10000
Z <- 10000
R <- 10000
a[1] <- 1*10^-5
a[2] <- 9.99*10^-5
b[1] <- 1*10^-5
b[2] <- 9.99*10^-5
d[1] <- 1*10^-5
d[2] <- 9.99*10^-5
c[1] <- 1*10^-5
c[2] <- 9.99*10^-5

t <- 30
SZR <- function(S, Z, R, a, b, d, c, t){
  time <- 1
  A <- runif(1, min = a[1], max = a[2])
  B <- runif(1, min = b[1], max = b[2])
  D <- runif(1, min = d[1], max = d[2])
  C <- runif(1, min = c[1], max = c[2])
  for(i in 1:t){
    S[time+1] <- floor(S[time] - (B * S[time] * Z[time]) - (D * S[time]))
    Z[time+1] <- floor(Z[time] + (B * S[time] * Z[time]) + (C * R[time]) - (A * S[time] * Z[time]))
    R[time+1] <- floor(R[time] + (D * S[time]) + (A * S[time] * Z[time]) - (C * R[time]))
    
    time <- time + 1
  }
  t <- c(0:t)
  #Get the x values for the graph

  plot(t, S, type = "l", col="red", ylim=c(0, max(c(S,Z,R))), ylab = "Population")
  points(t, Z, type = "l", col="green")
  points(t, R, type = "l", col ="black")
  grid(NULL, NULL, lty = 3)
}

