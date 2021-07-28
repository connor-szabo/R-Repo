## QUEUE - CODE obtained from https://www.r-bloggers.com/simulating-a-queue-in-r/

t.end   <- 10^5 # duration of sim i.e. T
t.clock <- 0    # simulation clock
Ta <- 1    # interarrival period (LAMBDA)
Ts <- 1.0000    # service period (MU)
t1 <- 0         # time for next arrival
t2 <- t.end     # time for next departure
tb <- 0         # tmp var for last busy-time start
n <- 0          # number in system
b <- 0          # total busy time
c <- 0          # total completions
set.seed(1)


### Step 2 - Simulation Loop
while (c < 1000) {
  if (t1 < t2) {      # arrival event
    t.clock <- t1   # advance simulation clock to arrival time
    n <- n + 1 # increase the size of queue (system state)
    t1 <- t.clock + rexp(1, 1/Ta)
    if(n == 1) { 
      tb <- t.clock # statistic variable 
      t2 <- t.clock + rexp(1, 1/Ts)  # exponential interarrival period
    }
  } else {            # departure event
    t.clock <- t2 # advance simulation clock
    n <- n - 1 # decrease the size of queue (system state)
    c <- c + 1 # system stat - number of served customers
    if (n > 0) { # queue is not empty
      t2 <- t.clock + rexp(1, 1/Ts)  # exponential  service period
    }
    else { # queue is empty
      t2 <- t.end
      b <- b + t.clock - tb
    }
  }   
}

### Step 3 - Analyze Simulation Performance
u <- b/t.clock       # utilization B/T
x <- c/t.clock       # mean throughput C/T number of services per unit time

