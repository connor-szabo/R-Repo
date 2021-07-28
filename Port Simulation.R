## Step 1 - Initialization
t.end   <- 90 # duration of sim in days i.e. T
t.clock <- 0    # sim clock

# Parameters for RV generation
lambda <- 1.5   # Poisson process arrival rate (LAMBDA)
mu <- 1.0000        # service period (MU)

# Initialize event list variables (t1, t1 and t2)
ta <- rexp(1, lambda)   # time for next arrival
t1 <- t.end             # time for completion of ship at Harbor 1  
t2 <- t.end             # time for completion of ship at Harbor 2

# Initialize System State Variables
n  <- 0         # number in queue
s1 <- 0         # id of ship being served at Server 1
s2 <- 0         # id of ship being served at Server 2
b1s <- 0        # Start time of busy time for Crane 1
b1 <- 0         # Busy time for Crane 1
b2s <- 0        # Start time of busy time for Crane 2
b2 <- 0         # Busy time for Crane 2

# Initialize other variables / counters / data structures / statistics
i <- 0          # counter / id of new ship arriving in system
ha <- 0         # history (array) of arrival times (ha[i]) for ship i  
hw <- 0         # history (array) of waiting times (hw[i]) per ship i (NOTE: this is waiting time in system and not in queue)
c1 <- 0         # total services/jobs completed by server 1
c2 <- 0         # total services/jobs completed by server 2
lq <- 0         # Time-weighted average length of queue - STATISTIC (see Lecture 12 Slide 19 for definition)

set.seed(1)

### Step 2 - Simulation Loop
while (t.clock < t.end) {
  
  if (ta < t1 & ta < t2) {      # arrival event is triggered
    tdelta <- ta - t.clock # record the time spent between last and current event
    t.clock <- ta   # advance sim clock to arrival time (current event time)
    
    # Update history / other variables for statistics
    lq <- lq + n*tdelta/t.end  # update time-weighted average queue length 
    i <- i + 1    # The id of arriving ship
    if (i == 1){  # Arrival of first ship, then do not append to history arrays
      ha <- t.clock
      hw <- t.end - t.clock
    }
    else{ 
      ha <- append(ha, t.clock)
      hw <- append(hw, t.end - t.clock)
    }
    
    ta <- t.clock + rexp(1, lambda) # generate time for next arrival
    
    # Update system state and future event list 
    if(n == 0 & s1 == 0) { # When Berth 1 is free (queue should be empty)
      s1 <- i # assign new customer directly to Berth 1 
      t1 <- t.clock + runif(1, 0.5, 1.5)  # generate departure time from Berth 1 with rate
      b1s <- t.clock
    }
    else if (n == 0 & s1 != 0 & s2 == 0){ # When only berth 2 is free (queue should be empty)
      s2 <- i # assign new customer directly to berth 2
      t2 <- t.clock + runif(1, 0.5, 1.5)  # generate departure time from Berth 1 with rate
      b2s <- t.clock
    }
    else{ # Both cranes are busy (n should be > 0)
      n <- n + 1 # increase the size of queue (system state)
    }
    
  } 
  
  else if (t1 < ta & t1 <= t2) {    # Ship is leaving Harbor 1
    tdelta <- t1 - t.clock # record the time spent between last and current event
    t.clock <- t1 # advance simulation clock
    
    # Update history and statistics variables
    lq <- lq + n*tdelta/t.end  # update time-weighted average queue length
    c1 <- c1 + 1 #  update number of serviced ships
    hw[s1] <- t.clock - ha[s1] # record system waiting time of ship departing in history
    
    # Update system state and future event list
    if (n > 0) { # queue is not empty
      s1 <- max(s1,s2) + 1 # Harbor 1 takes next customer in queue (ids are order of arrival)
      n <- n - 1 # decrease the size of queue
      t1 <- t.clock + runif(1, 0.5, 1.5)  # exponential  service period
    }
    else { # queue is empty and hence, harbor 1 is now free
      t1 <- t.end
      s1 <- 0
      b1 <- b1 + t.clock-b1s
    }
    
  }
  
  else if (t2 < ta & t2 < t1) {    # Customer is leaving Harbor 2
    tdelta <- t2 - t.clock # record the time spent between last and current event
    t.clock <- t2 # advance simulation clock
    
    # Update history and statistics variables
    lq <- lq + n*tdelta/t.end  # update time-weighted average queue length
    c2 <- c2 + 1 #  update number of serviced ships
    hw[s2] <- t.clock - ha[s2] # record departure time of ship departing in history
    
    # Update dock state and future event list
    if (n > 0) { # queue is not empty
      s2 <- max(s1,s2) + 1 # Harbor 2 takes next ship in queue (ids are order of arrival)
      n <- n - 1 # decrease the size of queue
      t2 <- t.clock + runif(1, 0.5, 1.5)  # exponential  service period
    }
    else { # queue is empty and hence, Harbor 1 is now free
      t2 <- t.end
      s2 <- 0
      b2 <- b2 + t.clock-b2s
    }
    
  }   
  
}

cat(sprintf("MAX TIME IN HARBOR IS: %f days\n", max(hw)))
cat(sprintf("MIN TIME IN HARBOR IS: %f days\n", min(hw)))
cat(sprintf("AVERAGE TIME PER SHIP SPENT IN HARBOR IS: %f days\n", mean(hw)))
cat(sprintf("UTILIZATION OF CRANE 1 IS: %f\n", b1/t.end))
cat(sprintf("UTILIZATION OF CRANE 2 IS: %f\n", b2/t.end))
