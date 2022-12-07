## Assignment 1 ##
## Author: Gina Hahnel
## Date: 01-12.2022

## Instructions. Please use the function below to create a working simulation of mouse population dynamics (N) over time
mice <- function(Nt,d,b,m){
  Nt1 <- (1+b)*(1-d)*Nt + m
  return(Nt1)
}

## Help. Recall that I created an example using a different function / model. Use that as a template to create your mouse simulation in 6 steps.
############################################################

# Step 1. Example parameters
b <- 1.7
c <- .15
Pt <- 6

# Step 2. Example equation function
example_equation_function <- function(b,c,Pt){
  Pt1 <- (b*Pt) / (1 + c*Pt)
  return(Pt1)
}

# Step 3. Make sure the function works
Pt1 <- example_equation_function(b,c,Pt)

# Step 4. Create a new variable to hold future values of P
P <- rep(NA,100)

# Step 5. Create a for loop to iteratively calculate P
P[1] <- Pt
for(i in 2:100){
  P[i] <- example_equation_function(b,c,Pt)
  Pt <- P[i]
}

# Step 6. Plot P over time
plot(P,xlab="time",ylab="P",pch=19,col="black")

#### INSERT YOUR CODE BELOW



# Step 1. save Parameters 
d <- 0.7
b <- 3
m <- 4
Nt <- 42

# Step 2. Write mice function 
mice_function <- function(d,b,m,Nt){
  Nt1 <- (1+b)*(1-d)*Nt+m
  return(Nt1)
}

# Step 3. making sure the function works 
Nt1<-mice_function(d,b,m,Nt)

#Step 4. create new variable N
N <- rep(NA,100)

# Step 5. create a for loop
N[1] <- Nt
for(i in 2:100){
  N[i] <- mice_function(d,b,m,Nt)
  Nt <- N[i]
}
print(N)

#Step 6. Plot N over time 
plot(N,xlab="time",ylab="N",pch=19,col="black")



