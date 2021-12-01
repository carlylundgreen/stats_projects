
library(reshape2)

# Expectation-Maximization Algorithm to Estimate a21 & a22 

# 1: random initialization of parameter (theta_{0})
# 2: Assume parameters from step 1 (theta_{t-1}) are fixed, compute exp value of a21 and a22
# 3: Given the expected values from part 2, estimate new parameter values for theta_{t} 
# If likelihood of the observations have not changed much, exit; otherwise, go back to Step 1.

################################################################################### 
#1. Get initial guesses for a_21 and a_22
#2. Set Pr_1 = 0
#3. Set Pr_t = a_32 Pi_{t-1} + Pr_{t-1} for t = 2 through n
#4. Set Ps_t = 1-Pi_t-Pr_t for all n
#5. Estimate a_21 and a_22 by optimizing the target function sum_{i=2}^n (Pi_t - (a_21 Ps_{t-1} + a_22 Pi_{t-1}))^2
#Repeat steps 3 through 5 until it converges. 
###################################################################################

#initial guesses for a21, a22 
# these are important because we can get negative probabilities if we choose bad initial values 
a21 <- 0.5 
a22 <- 0.4

get_probs <- function(a21,a22){
  a11 <- 1-a21
  a32 <- 1-a22
  n <- 14
  Pi <- c(3,8,28,75,221,291,255,235,190,125,70,28,12,5)/743 #observations at time t

  #initialize Pr
  Pr <- numeric(length=n)
  Pr[1] <- 0

  #define Pr_{t} as a function of Pi and Pr_{t-1}
  for(t in 2:n){
    Pr[t] <- a32*Pi[t-1] + Pr[t-1]
  }

  # Define Ps_t = 1-Pi_t - Pr_t for all n
  Ps <- numeric(length=n)
  for(t in 1:n){
    Ps[t] <- 1 - Pi[t] - Pr[t]
  }
  matrix(c(Ps,Pi,Pr),nrow=14,ncol=3,byrow=FALSE)
}
mat <- get_probs(0.5,0.58)

mat
Ps <- mat[,1]
Pi <- mat[,2]
Pr <- mat[,3]

# Estimate a_21 and a_22 by optimizing the target function sum_{i=2}^n (Pi_t - (a_21 Ps_{t-1} + a_22 Pi_{t-1}))^2  
target <- function(pars){
  a21 <- pars[1] 
  a22 <- pars[2]
  components <- numeric()
  for(t in 2:14){
  components[t-1] <- (Pi[t] - (a21*Ps[t-1] + a22*Pi[t-1])) ^2
  }
  sum(components)
}


#initial guesses for a21, a22
pars <- c(0.5,0.53) 
params <- optim(pars,target,method="BFGS")
a21 <- params$par[1]
a22 <- params$par[2]

probs <- get_probs(a21,a22) %>% as.data.frame()
colnames(probs) <- c("Susceptible","Infected","Recovered")
probs$Day <- seq(1,14,by=1)
mdat <- melt(probs, id=c("Day"))
colnames(mdat) <- c("Day","Group","value")

ggplot(data=mdat) + geom_line(aes(x=Day, y=value, color = Group),lwd=1) +
  scale_colour_manual(values=c("blue","red","green")) + labs(y="Probability of Being in Group") + 
  theme_bw()


#generate some new infected data to see what the maximum looks like 
new_Pi <- numeric(length=14)
new_Pi[1] <- 0
a21
a22
Ps <- c(probs[,1])
for(t in 2:14){
new_Pi[t] <- a21*Ps[t-1] + a22*new_Pi[t-1]
}
new_Pi*743


plot.ts(new_Pi)
