#Number of users
n <- 50
#Time
t.clock = 0
#Time to return/move job
t.return <- 0.01
#Service time allocated to completing a job
q <- 0.5
#Response times for jobs
response <- 0
#completed jobs
jobNum <- 0
#Queue containing service time
queue <- 0
queue <- queue[-1]
#Array holding projects that are waiting to be resubmitted
waiting <- 0
#Array holding the start times for when a job enters the queue
startTime<-0

for(i in  1:n){
  waiting[i] <- rexp(1,1/30)
}

#Run the simulation
while(jobNum <1000){
  #If all jobs are waiting, increment the wait time
  if(length(queue) < 1){
    for(i in 1:length(waiting)){
      waiting[i] = waiting[i] - q
      if(is.na(waiting[i])){
        waiting <- waiting[!is.na(waiting)]
      }
      else{
        #If wait time reaches 0
        if(waiting[i] <= 0){
          #Add the start time of the new job entered into the Queue
          startTime[length(startTime)+1] <- t.clock + waiting[i] + q
          waiting <- waiting[-i]
          
          #add the job to the queue
          queue[length(queue)+1] <- rexp(1, 1/runif(1, min=1, max=3))
        }#close if switch
      }#close else
    }#close for loop
    #increment the clock  
    t.clock = t.clock + q
  }#end of if no jobs availible
  
  #If there are jobs availible
  else{
    #If time remaining on job is less or equal to round robin time
    if(queue[1] <= q){
      #Increment finished job counter
      jobNum = jobNum + 1
      #increment time
      t.clock = t.clock + queue[1] + t.return
      
      #Save the time in queue to an array
      response[jobNum] <- (t.clock - t.return) - startTime[1]
      #Remove the start time
      startTime<- startTime[-1]
      
      #Add time passed to the waiting jobs
      if(length(waiting) > 0){
        for(i in 1:length(waiting)){
          waiting[i] <- waiting[i] - queue[1] + t.return
          if(is.na(waiting[i])){
            waiting <- waiting[!is.na(waiting)]
          }
          else{
            #If the wait time reaches 0, add the job to the queue
            if(waiting[i] <= 0){
              
              #Add the start time of the new job entered into the Queue
              startTime[length(startTime)+1] <- t.clock + waiting[i]
              waiting <- waiting[-i]
              
              #add the job to the queue
              queue[length(queue)+1] <- rexp(1, 1/runif(1, min=1, max=3))
            }
          }#close else
        }
      }
      
      #Remove the job from the queue
      queue <- queue[-1]
      
      #add the job to the thinking queue
      waiting[length(waiting)+1] <- rexp(1, 1/30)
      waiting[length(waiting)] <- 1/waiting[length(waiting)]
      
    }
    #If the round robin time is less than time remaining for job
    else{
      queue[1] = queue[1]-q
      #Send the job that was just processed to the back of the queue
      queue <- c(queue[-1], queue[1])
      startTime <- c(startTime[-1], startTime[1])
      #Add passed time to clock
      t.clock = t.clock + q + t.return
      
      #If there are waiting processes
      if(length(waiting) > 0){
        #remove time from wait time remaining
        for(i in  1:length(waiting)){
          waiting[i] <- waiting[i] - q - t.return
          if(is.na(waiting[i])){
            waiting <- waiting[!is.na(waiting)]
          }
          else{
            #If the wait time reaches 0, add the job to the queue
            if(waiting[i] <= 0){
              
              #Add the start time of the new job entered into the Queue
              startTime[length(startTime)+1] <- t.clock + waiting[i]
              waiting <- waiting[-i]
              
              #add the job to the queue
              queue[length(queue)+1] <- rexp(1, 1/runif(1, min=1, max=3))
            }
          }#close else
        }
      }
    }
  }#End of if jobs are available
}#end of while loop

averageWait<- mean(response)
print(averageWait)

