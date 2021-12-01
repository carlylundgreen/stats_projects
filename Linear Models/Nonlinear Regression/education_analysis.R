
# Hw3 - Education Analysis 

dat <- read.table(file="https://mheaton.byu.edu/docs/files/Stat536/OutOfClassCaseStudies/Nonlinear/Schools/Data/SchoolResults.txt",
                  header=TRUE,sep="")
head(dat)
cor(dat)

library(mgcv)
library(gam)
#library(gam)
gamfull <- gam::gam(Score ~ s(Lunch)+s(Computer)+ s(Expenditure) + s(Income) + s(English) + s(STratio) ,data=dat)
summary(gamfull)
par(mfrow=c(2,3))
plot.Gam(gamfull , se=TRUE , col ="red ")

#Look at Lunch variable
gam2 <- gam::gam(Score ~ Lunch + s(Computer)+ s(Expenditure) + s(Income) + s(English) + s(STratio) ,data=dat)
gam3 <- gam::gam(Score ~ s(Computer)+ s(Expenditure) + s(Income) + s(English) + s(STratio) ,data=dat) #leave out 

anova(gamfull,gam2,gam3)
# first pvalue this says there is no evidence that a model with a smoothing spline is needed (expected)
# second pvalue says there is evidence that we should definitely keep 'Lunch' in the model 

#Look at Computer variable
gam4 <- gam::gam(Score ~ s(Lunch) + Computer + s(Expenditure) + s(Income) + s(English) + s(STratio) ,data=dat)
gam5 <- gam::gam(Score ~ s(Lunch) + s(Expenditure) + s(Income) + s(English) + s(STratio) ,data=dat) #leave out 

anova(gamfull,gam4,gam5)
#first pvalue says there's no evidence that we need a smoothing spline (expected)
#second pvalue says we definitely need Computer in the model in some way 

#Look at Expenditure 
gam6 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + Expenditure + s(Income) + s(English) + s(STratio) ,data=dat)
gam7 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Income) + s(English) + s(STratio) ,data=dat) #leave out 

anova(gamfull,gam6,gam7)
#we definitely don't need a smoothing spline function on expenditure 
#we don't even need expenditure in the model (I would imagine b/c it is collinear-ish with STratio and income)

#Look at Income**
gam8 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Expenditure) + Income + s(English) + s(STratio) ,data=dat)
gam9 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Expenditure) + s(English) + s(STratio) ,data=dat)

anova(gamfull,gam8,gam9)
#as expected, a smoothing spline function is definitely better than linear
#also, income is definitely important to keep in the model 

#Look at English variable
gam10 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Expenditure) +  s(Income) + English + s(STratio) ,data=dat)
gam11 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Expenditure) + s(Income) + s(STratio) ,data=dat)

anova(gamfull,gam10,gam11)
#at .05 level, we reject the null that a linear function is 'better' and conclude that we need some nonlinear function
#we also know that it's important to keep English in the model 

#Look at STratio
gam12 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Expenditure) + s(Income) + s(English) + STratio ,data=dat)
gam13 <- gam::gam(Score ~ s(Lunch)+ s(Computer) + s(Expenditure) + s(Income) + s(English), data=dat)

anova(gamfull,gam12,gam13)
#first pvalue means that there is evidence that a smoothing spline is better than linear
#high pvalue means there's not significant evidence that the model is better when STratio is included

#What are we left with then? 
new_gam2 <- mgcv::gam(Score~ Lunch + Computer + s(Income) + s(English),data=dat)

summary(new_gam2)
summary(new_gam)
new_gam2$coefficients


par(mfrow=c(2,3))
plot.Gam(new_gam,se=TRUE,col="red")

#question 1 
ggplot(mapping=aes(x=Income,y=Score),data=dat) + geom_point() + geom_smooth(method="gam")

#question 2 
library(ggplot2)
ggplot(mapping=aes(x=English,y=Score),data=dat) + geom_point() + geom_smooth(method="gam")

#Is english as a second language a barrier to student learning?
#ie, is coefficient for english significantly negative
summary(new_gam)
new_gam$coefficients
confint(new_gam)

#question 3
#In your opinion and based on the data, what can be done to increase student learning?


gam_mod <- mgcv::gam(Score~ Lunch + Computer + s(Income) + s(English),data=dat)

preds <- predict.gam(gam_mod)

(rmse <- mean(((preds - dat$Score)^2)) %>% sqrt())

rsquared <- 1 - (sum((dat$Score - preds)^2)/sum((dat$Score - mean(dat$Score))^2))  
#is there a clever way to combine income and english? 

summary(gam_mod)
summary(gam_mod)
gam_mod
std_resids <- (gam_mod$residuals - mean(gam_mod$residuals))/sd(gam_mod$residuals)
y <- gam_mod$fitted.values

#check homoskedasticity
ggplot(mapping=aes(x=y,y=std_resids)) + geom_point() + geom_abline(intercept=0,slope=0)
ggplot() + geom_histogram(mapping=aes(x=std_resids))

min(resids)

which(resids == min(resids))

dat[168,]
