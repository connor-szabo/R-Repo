## Step 1 - Initialization
t.end   <- 90 # duration of sim i.e. T
t.clock <- 0    # sim clock

# Parameters for RV generation
lambda <- 1.0   # Poisson process arrival rate (LAMBDA)
mu <- 1.0000        # service period (MU)

# Initialize event list variables (t)
ta <- rexp(1, lambda)   # time for next arrival
t <- t.end             # time for completion of process in Server 

# Initialize System State Variables
n  <- 0         # number in queue
s <- 0         # id of process being processed at Server
bs <- 0        # Start time of busy time for Server
b <- 0         # Busy time for Crane

# Initialize other variables / counters / data structures / statistics
i <- 0          # counter / id of new process arriving in system
ha <- 0         # history (array) of arrival times (ha[i]) for process i  
wt <- 0        # Wait times in queue
c <- 0         # total services/jobs completed by server
lq <- 0         # Time-weighted average length of queue - STATISTIC (see Lecture 12 Slide 19 for definition)
maxQueue <- 0
pfive <- 0     #Number of jobs that waited over 5 min


set.seed(1)

### Step 2 - Simulation Loop
while (c < 1000) {
  
  if (ta < t) {      # arrival event is triggered
    tdelta <- ta - t.clock # record the time spent between last and current event
    t.clock <- ta   # advance sim clock to arrival time (current event time)
    
    # Update history / other variables for statistics
    lq <- lq + n*tdelta/t.end  # update time-weighted average queue length 
    i <- i + 1    # The id of arriving process
    if (i == 1){  # Arrival of first process, then do not append to history arrays
      ha <- t.clock
    }
    else{ 
      ha <- append(ha, t.clock)
    }
    
    ta <- t.clock + rexp(1, lambda) # generate time for next arrival
    
    # Update system state and future event list 
    if(n == 0 & s == 0) { # When server is free (queue should be empty)
      s <- i # assign new customer directly to server 
      t <- t.clock + rexp(1, 1.1)  # generate departure time from Berth 1 with rate
      bs <- t.clock
      wt[i] = 0
    }
    else{ # Server is busy (n should be > 0)
      n <- n + 1 # increase the size of queue (system state)
      wt[i] <- t.clock
      if(n > maxQueue){
        maxQueue <- n
      }
    }
    
    # Printing log for checking simulation logic
  } 
  
  else if (t < ta) {    # Customer is leaving Server
    tdelta <- t - t.clock # record the time spent between last and current event
    t.clock <- t # advance simulation clock
    
    # Update history and statistics variables
    lq <- lq + n*tdelta/t.clock  # update time-weighted average queue length
    c <- c + 1 #  update number of served customers

    # Update system state and future event list
    if (n > 0) { # queue is not empty
      s <- max(s) + 1 # Server takes next customer in queue (ids are order of arrival)
      wt[i] <- t.clock - wt[i]
      n <- n - 1 # decrease the size of queue
      t <- t.clock + rexp(1, 1.1)  # exponential  service period
    }
    else { # queue is empty and hence, server is now free
      t <- 10000
      s <- 0
      b <- b + t.clock-bs
    }
    
  }
}


wt = sort(wt)

for(j in 1:length(wt)){
  if(wt[j] > 5){
    pfive = pfive + 1
  }
}

cat(sprintf("Max time in Queue: %f min\n", max(wt)))
cat(sprintf("Average time in Queue: %f min\n", avg(wt)))
cat(sprintf("Proportion of jobs that waited over 5 min in queue: %f\n", pfive/c))
cat(sprintf("Max jobs in queue at one point: %i\n", maxQueue))

