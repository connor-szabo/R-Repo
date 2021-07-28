S <- 125000
E <- 125000
I <- 125000
R <- 125000

#Get Infections per 100000 people
B<- runif(1, min=0.5, max=0.6)
##change to infections per person
B<-B/100000

#The exposure rate of the virus (geometric distribution)
a<- rexp(n=1, rate=1/runif(1, min=7, max = 17))
a<- 1/a

#The rate of infection (exponential distribution)
v<-rexp(n=1, rate=1/30)
v<- 1/v

#i variable will keep track of what day it is, while the function 
#is passed the number of days as t
t<-365

SEIR<- function(S, E, I, R, a, B, v, t){
  #Adjust input to make sure there are no decimals because these numbers
  #are in terms of people
  S <- floor(S)
  E <- floor(E)
  I <- floor(I)
  R <- floor(R)
  
  #Check for negative input
  if(S < 0){
    print("Negative Input")
    return(c(0,0,0,0))
  }
  if(E < 0){
    print("Negative Input")
    return(c(0,0,0,0))
  }
  if(I < 0){
    print("Negative Input")
    return(c(0,0,0,0))
  }
  if(R < 0){
    print("Negative Input")
    return(c(0,0,0,0))
  }
  
  for(i in 1:t){
    #I use the floor function for each calculation because it is in
    #regards to people, and you can't half a part of a person
    
    
    S[i+1] = floor(S[i] - B * S[i] * I[i])
    if(S[i+1] < 0){
      S[i+1] <- 0
    }
    
    E[i+1] = floor(E[i] - a * E[i] + B * S[i] * I[i])
    if(E[i+1] < 0){
      E[i+1] <- 0
    }
    
    I[i+1] = floor(I[i] + a * E[i] - v * I[i])
    if(I[i+1] < 0){
      I[i+1] <- 0
    }
    
    R[i+1] = floor(R[i] + v * I[i])
    if(R[i+1] < 0){
      R[i+1] <- 0
    }
  }#Close for-loop
  
  
  #Create the graph
  plot(c(0:t), S, type="l", col="black", ylim=c(0, max(S,E,I,R)+15000), ylab="Population", xlab="Number of Days", main="S.E.I.R. Model")
  points(c(0:t), E, type="l", col="red")
  points(c(0:t), I, type="l", col="blue")
  points(c(0:t), R, type="l", col="green")
  legend(0, max(S,E,I,R)+15000, legend=c("S", "E", "I", "R"), col=c("black", "red", "blue", "green"), lty=1:1)

  print(S[30])
  print(S[90])
  print(S[365])
  print(E[30])
  print(E[90])
  print(E[365])
  print(I[30])
  print(I[90])
  print(I[365])
  print(R[30])
  print(R[90])
  print(R[365])
  }



