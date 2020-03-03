
##################
library(ggplot2)
library(GGally)
library(gridExtra)
library(grid)
library(car)
library(MASS)
library(bestglm)
library(lmtest)
##################

farms <- read.table(file="C:/Users/Carly/Documents/Semester 6/Stat 330/farms3.txt",sep="",header=TRUE)
farms <- farms[,c(2:ncol(farms),1)]

#explore data with scatterplot and get correlations
str(farms)
pairs(farms[,c("acrePrice","improvements","tillable","crpPct","productivity")])
corrs <- cor(farms[,c("acrePrice","improvements","tillable","crpPct","productivity")])
corrs <- round(corrs,2)
table <- tableGrob(corrs)
grid.newpage()
grid.draw(table)

#fit an untransformed model and assess it
full.lm <- lm(acrePrice~., data=farms)
avPlots(full.lm)

resids.full <- full.lm$residuals
fittedvals.full <- full.lm$fitted.values
plot(fittedvals.full,resids.full,main="Residuals vs. Fitted Values for Untransformed Model",xlab="Fitted Values",ylab="Residuals")
abline(h=0)

#find best fitting model 
vs.res <- bestglm(farms,IC="AIC",method="exhaustive")
plot(vs.res$Subsets$AIC,type="b",pch=19,xlab="# of Variables", ylab="AIC",main="Number of Variables vs. AIC") 
vs.res$BestModel

#fit new model with best variables 
new.lm <- lm(log(acrePrice)~improvements + tillable + crpPct + WC + productivity*NW, data=farms)

###########look at assumptions###############
#linearity
avPlots(new.lm)

#Histogram of residuals
stdresids <- stdres(new.lm)
ggplot(farms, aes(x=stdresids))+geom_histogram()+theme_bw()+
  labs(x="Standardized Residuals",y="Frequency",title="Histogram of Standardized Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

#KS test for normality 
ks.test(stdresids,"pnorm")

#Get residuals vs. fitted values plot 
resids <- new.lm$residuals
fitted.vals <- new.lm$fitted.values

plot(fitted.vals,resids,main="Residuals vs. Fitted Values",xlab="Fitted Values",ylab="Residuals")
abline(h=0)

#BP test for equal variance
bptest(new.lm)
################################################

##Assess fit and predictive accuracy of model 
summary(new.lm)$r.squared

n.cv <- 250 
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv)
width <- rep(NA,n.cv)
n.test <- 10

for(cv in 1:n.cv){
  ##split into test and training sets 
  test.obs <- sample(1:nrow(farms), n.test)
  test.set <- farms[test.obs,]
  train.set <- farms[-test.obs,]
  
  ##fit a LM using training data only
  train.lm <- lm(log(acrePrice)~improvements + tillable + crpPct + WC + productivity*NW, data=train.set)
  
  ##Get predictions and prediction intervals 
  pred <- predict.lm(train.lm,newdata=test.set,interval="prediction")
  ##untransform predictions
  realpreds <- exp(pred)
  
  ##Calculate results 
  bias[cv] <- mean(realpreds[,'fit'] - test.set[,'acrePrice'])
  rpmse[cv] <- sqrt(mean((realpreds[,'fit'] - test.set[,'acrePrice'])^2))
  cvg[cv] <- mean(realpreds[,'lwr'] < test.set[,'acrePrice'] & realpreds[,'upr'] > test.set[,'acrePrice'])
  width[cv] <- mean(realpreds[,'upr'] - realpreds[,'lwr'])
  
}
mean(bias)
mean(cvg)
mean(rpmse)
mean(width)
max(farms$acrePrice) - min(farms$acrePrice)

############Results##################
summary(new.lm)

#Get confidence intervals for estimates 
Coefficients <- new.lm$coefficients
coefs <- data.frame(Coefficients)
coefs2 <- round(coefs, 4)
int <- confint(new.lm)
int <- round(int,4)

estimates <- tableGrob(coefs2)
intervals <- tableGrob(int,rows=NULL)
grid.newpage()
grid.arrange(gtable_combine(estimates,intervals),ncol=1)

#plot scatterplot to explore interaction 
ggplot(farms,aes(y=log(acrePrice),x=productivity,color=NW))+geom_point()+
  labs(x="Productivity",y="Log Price per Acre",title="Price vs. Productivity")+
  theme(plot.title = element_text(hjust = 0.5))

summary(new.lm)
#Use model to predict Price
dframe <- data.frame(improvements=0, tillable=94, crpPct=0,productivity=96, WC = "No",NW = "Yes")
prediction <- predict.lm(new.lm,newdata=dframe,interval="prediction",level=.95)
#un-transform prediction
real.pred <- exp(prediction)
real.pred

