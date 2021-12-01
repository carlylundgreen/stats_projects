
#########################################################################################################
## Perform Simulation to test Methodology
#initial guesses for a21, a22 (parameters)
# these are important because we can get negative probabilities if we choose bad initial values 
a21 <- 0.5 
a22 <- 0.4
Nsim <- 1000
test <- function(a21,a22){
  
  a11 <- 1-a21
  a32 <- 1-a22
  n <- 14
  Pi <-  runif(14,0,1) #observations at time t
  Pi[1] <- 0.1
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
params <- optim(pars,target,method="BFGS")$par
params
}
test(0.5,0.6)

reps <- replicate(10000,test(0.5,0.6),simplify="matrix")
reps <- reps %>% t()

##Create table: 
colMeans(reps)

#plot a21 and a22
df <- as.data.frame(c(reps[,1],reps[,2]))
df$col2 <- c(rep("a21",nrow(df)/2),rep("a22",nrow(df)/2))
colnames(df) <- c("vals","Prob")

ggplot(data=df) + geom_histogram(mapping=aes(x=vals,color=Prob,fill=Prob),color="white",bins=40) + theme_bw() + 
  labs(x="Simulated Values of a21 and a22",y="Frequency") + 
  geom_vline(xintercept = colMeans(reps)[1],size=1) + 
  geom_vline(xintercept=colMeans(reps)[2],size=1) + 
  annotate("text", x=0.18, y=1500, label= "E(a21)") +
  annotate("text", x=0.9, y=1500, label= "E(a22)")


############################################################################################################
#### Apply to Data

#initial guesses for a21, a22 
# these are important because we can get negative probabilities if we choose bad initial values 
a21 <- 0.4 
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

#mat %>% round(3)
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

probs <- get_probs(a21,a22) %>% as.data.frame()
#initial guesses for a21, a22
pars <- c(0.4,0.5) 
params <- optim(pars,target,method="BFGS")
a21 <- params$par[1]
a22 <- params$par[2]

probs <- get_probs(a21,a22) %>% as.data.frame()
colnames(probs) <- c("Susceptible","Infected","Recovered")
probs$Day <- seq(1,14,by=1)
mdat <- melt(probs, id=c("Day"))
colnames(mdat) <- c("Day","Group","value")

ggplot(data=mdat) + geom_line(aes(x=Day, y=value, color = Group),lwd=1) +
  scale_colour_manual(values=c("blue","red","green")) + labs(y="Probability") + 
  theme_bw()

simulate <- function(row_of_probs){
  Nsim <- 10000
  #initialize matrix 
  exp_val_time_t <- matrix(nrow=1,ncol=3)
  
  #generate 10000 multinomial draws from a set of 3 probabilities for S, I, and R
  test <- rmultinom(10000,743,prob=row_of_probs) %>% t()
  
  #calculate expected value
  exp_val_time_t <- colMeans(test) 
  
  #append 95% interval bounds for *infected only*
  c(exp_val_time_t, quantile(test[,2],0.025),quantile(test[,2],0.975)) 
  }
 
#apply function over rows of probs

exp_vals <- apply(probs,1,simulate) %>% t()
exp_vals <- exp_vals %>% as.data.frame()
colnames(exp_vals) <- c("S","I","R","2.5%","97.5%")
exp_vals$S <- exp_vals$S %>% round(2)
exp_vals$I <- exp_vals$I %>% round(2)
exp_vals$R <- exp_vals$R %>% round(2)
exp_vals$`2.5%` <- exp_vals$`2.5%` %>% round()
exp_vals
#plot
exp2 <- exp_vals[,1:3]
colnames(exp2) <- c("Susceptible","Infected","Recovered")
exp2$Day <- seq(1,14,by=1)
mdat <- melt(exp2, id=c("Day"))
colnames(mdat) <- c("Day","Group","value")
mdat$value <- mdat$valu/743 #convert back to probabilities
ggplot(data=mdat) + geom_line(aes(x=Day, y=value, color = Group),lwd=1) +
  scale_colour_manual(values=c("blue","red","green")) + labs(y="Probability") + 
  theme_bw()

## Plotting a transition probability matrix 
library(MmgraphR)

##########################################
# Plotting a probability transition matrix
##########################################
a21
trmat<-matrix( c (1-a21, 0, 0, 
                  a21, a22, 0,
                  0, 1-a22, 1), nrow = 3, ncol = 3, byrow = TRUE) 
trmat
trmatplot(trmat,rowconstraint=FALSE,ylab="Group")


