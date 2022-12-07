## Reading guide 4 
# 4b 
N0=0.7*266=186.2 with K=266
#4c 
#create a simulation model of population dynamics for SBM spotted owls
# Use this r value and the K values given in Figure 2 to develop a simulation of population dynamics for SBM spotted owls. Use your N0 value above as the first Nt value. Plot the results of population size (N) over time. 
#Hint: Use the example code in Lecture 2.3 notes.** 
#Hint: Here is what my plot looks like 
#Hint: Use formula 3.5a from Otto & Day Chapter 3. (You can run the simulation for 20 time steps)
# Formula 3.5a n(t + 1) = n(t) + rdn(t)(1-n(t)/K)
K <- 266
r <- 0.86
n=N0 <- 186.2

install.packages("tidyverse")
library(deSolve)
library(tidyverse)
## logistic growth
cont_log <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dN <- r*N*(1-(N/K))
  })
}

parameters <- c(r=0.86, K = 266)
state <- c(N=186)
times <- seq(0, 20, by = 0.01)

out <- deSolve::ode(y= state, times = times, func = cont_log, parms = parameters)
out.g <- as.data.frame(out)
ggplot2::ggpot(out.g, aes(time,N)) + geom_tine()


# 5
# change the code I had for a matrix population model
# The researchers estimated population growth rate R using data collected from the SBM population, fit to a two-stage Leslie projection matrix model. This is a model for stage-structured populations, as is the case for the spotted owl. We begin from the discrete-time model for exponential population growth:

# Step 1. Build the transition matrix A
P.12 <- 0.344
P.22 <- 0.767
F1 <- 0.304
F2 <- 0.304
Nt <- .7*K

A <- matrix(c(
  F1, F2,
  P.12, P.22)
  ,nrow=2,ncol=2,byrow=T)

A

# Step 2. Build the matrix Nt (initial abundance matrix)
Nt <- c((Nt/2),(Nt/2))

# Step 3. Matrix multiplication for Nt+1
Year1 <- A %*% Nt  # matrix multiplication!
Year1

# Step 4. Create a new variable to hold future values of N
N <- array(NA,dim=c(20,2))

# Step 5. Create a for loop to iteratively calculate N
N[1,] <- Nt
for(i in 2:20){
  N[i,] <- A %*% N[i-1,]
}

## My code 
# Step 1. Buikd the transition matrix A
P.12 <- 0.344
P.22 <- 0.767
P.32 <- 0
P.13 <- 0
P.23 <- 0.1
P.33 <- 0.1
F1 <- 0.304
F2 <- 0.304
F3 <- 0.304
Nt <- .7*266

A <- matrix(c(
  F1, F2, F3,
  P.12, P.22, P.32,
  P.13, P.23, P.33),
  nrow=3, ncol=3,byrow=TRUE)

A

# Step 2. Build the matrix Nt (initial abundance matrix)
Nt <- matrix(c(
  (Nt/2),
  (Nt/2),
  (Nt/2)),
  nrow=3, ncol=1,byrow=T)

Nt

# Step 3. Matrix multiplication for Nt+1
Year1 <- A %*% Nt
Year1

# Step 4. Create a new variable to hold future values of N
N <- array(NA,dim=c(20,3))

# Step 5. Create a for loop to iteratively calculate N 
N[1,] <- Nt 
for(i in 2:20){
  N[i,] <- A %*% N[i-1,]
}

N
