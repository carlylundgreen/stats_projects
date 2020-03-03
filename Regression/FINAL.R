
##################
library(ggplot2)
library(GGally)
library(gridExtra)
library(grid)
library(car)
library(MASS)
library(bestglm)
library(lmtest)
library(pROC)
#################

beetles <- read.csv(file="C:/Users/Carly/Documents/Semester 6/Stat 330/PineBeetle2.csv",sep=",",header=TRUE)
head(beetles)

###Explore data####
par(mfrow=c(2,3))
myPlot<-function(index) {plot(beetles[,index] ~ beetles$Infested, main=names(beetles[index]),pch=16,xlab="Infested",ylab=names(beetles[index]))}
lapply(1:4,FUN=myPlot)
#create a scatterplot in order to check conditions
par(mfrow=c(2,3))
scatter.smooth(beetles$January,beetles$Infested,xlab="Average January Minimum Temp",ylab="Infested with MPBs")
scatter.smooth(beetles$August_max,beetles$Infested,xlab="Average August Maximum Temp",ylab="Infested with MPBS")
scatter.smooth(beetles$Slope,beetles$Infested,xlab="Slope of Mountainside",ylab="Infested with MPBs")
scatter.smooth(beetles$Elev,beetles$Infested,xlab="Elevation",ylab="Infested with MPS")
scatter.smooth(beetles$Precip,beetles$Infested,xlab="Precipitation",ylab="Infested with MPs")
##1 means NO, 2 means YES, the area is infested

#best GLM 
vs.res <- bestglm(beetles,IC="AIC",method="exhaustive",family=binomial)
par(mfrow=c(1,1))
plot(vs.res$Subsets$AIC,type="b",pch=19,xlab="# of Vars",ylab="AIC") 
beetle.mod <- vs.res$BestModel

##assess model and get confidence intervals (transformed)
summary(beetle.mod)
confint(beetle.mod)
exp(confint(beetle.mod))

#make a nice table
Coefficients <- exp(beetle.mod$coefficients)
coefs <- data.frame(Coefficients)
coefs2 <- round(coefs, 4)
int <- exp(confint(beetle.mod))
int <- round(int,4)
estimates <- tableGrob(coefs2)
intervals <- tableGrob(int,rows=NULL)
grid.newpage()
grid.arrange(gtable_combine(estimates,intervals),ncol=1)

###find threshold and verify it's the minimum 
pred.probs <- predict.glm(beetle.mod,type="response")
thresh <- seq(0,1,length=100)
y <- beetles$Infested
misclass <- rep(NA,length=length(thresh)) 
for(i in 1:length(thresh)) {
  my.classification <- ifelse(pred.probs>thresh[i],'Yes','No')
  misclass[i] <- mean(my.classification!=y)
}

threshold <- thresh[which.min(misclass)]
threshold
plot(thresh, misclass, pch=20,type="l",xlab="Cutoff",ylab="Misclassification")
abline(v=threshold,col="red")

##confusion matrix for ALL the data
true.class <- beetles$Infested 
pred.class <- ifelse(pred.probs>threshold, 'Yes', 'No')
table(true.class,pred.class)
addmargins(table(pred.class,true.class)) 

##create ROC curve
pred.probs <- predict.glm(beetle.mod,type="response") 
obs.y <- beetles$Infested
a.roc <- roc(obs.y,pred.probs)
plot(a.roc,legacy.axes=TRUE,main="ROC Curve")
auc(a.roc) ##area under the ROC curve
my.roc <- roc(obs.y,pred.probs)
##cross-validation study for sensitivity, specificity, etc.
## Choose number of CV studies to run in a loop & test set size
n.cv <- 500
n.test <- round(.1*nrow(beetles))

## Set my threshold for classifying
cutoff <- threshold
## Initialize matrices to hold CV results
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
auc <- rep(NA,n.cv)

## Begin for loop
for(cv in 1:n.cv){
  ## Separate into test and training sets
  test.obs <- sample(1:nrow(beetles), n.test)
  test.set <- beetles[test.obs,]
  train.set <- beetles[-test.obs,]
  ## Fit best model to training set
  train.model <- beetle.mod
  ## Use fitted model to predict test set
  pred.probs <- predict.glm(train.model,newdata=test.set,type="response") #response gives probabilities
  ## Classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,"Yes","No")
  ## Create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Infested,levels=c("No","Yes")),
                               factor(test.class,levels=c("No","Yes"))))
  ## Pull of sensitivity, specificity, PPV and NPV
  ## using bracket notation
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3]
  spec[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2]
  npv[cv] <- conf.mat[1,1]/conf.mat[3,1]
  ## Calculate AUC
  auc[cv] <- auc(roc(test.set$Infested,pred.probs))
} #End for-loop

mean(sens)
mean(spec)
mean(npv)
mean(ppv)
mean(auc)

#run through each row of the data frame given, with the slope and elevations as given for the specific area, to find the probability for each 
#of the next 10 years
#I would've put this in a for loop, but I ran out of time
year1 <- data.frame(January=-13.98,August_max=15.89,Precip=771.13,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year2 <- data.frame(January=-17.80,August_max=18.07,Precip=778.54,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year3 <- data.frame(January=-17.27,August_max=16.74,Precip=677.63,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year4 <- data.frame(January=-12.52,August_max=18.06,Precip=522.77,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year5 <- data.frame(January=-15.99,August_max=18.23,Precip=732.32,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year6 <- data.frame(January=-11.97,August_max=15.81,Precip=615.96,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year7 <- data.frame(January=-15.75,August_max=16.85,Precip=805.90,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year8 <- data.frame(January=-16.19,August_max=16.51,Precip=714.57,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year9 <- data.frame(January=-17.87,August_max=17.84,Precip=740.50,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year10 <- data.frame(January=-12.44,August_max=16.96,Precip=801.22,Slope=18.07,Elev=1901.95,NC="No",SE="Yes",SW="No")
year <- data.frame(rbind(year1,year2,year3,year4,year5,year6,year7,year8,year9,year10))

get.probs<-function(index) {predict.glm(beetle.mod,newdata=year[index,],type="response")}

#find the average probability of MPB infestation over the next 10 years for the specified location
prob.vec <- as.numeric(lapply(1:10,FUN=get.probs))
mean(prob.vec)

