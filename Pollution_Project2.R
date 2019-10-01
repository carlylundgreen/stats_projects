

pollution <- read.csv("https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/3%20-%20Project/Data/BreathingZonePM.txt", sep = " ", header = TRUE)

#PM isn't seasonal
#methodology primarily from last two data sets
#some principles from first few case studies, but not much
#make sure to account for and discuss how you accounted for problems in the data set

#strategy: find out what variables you're going use and which ones you're going throw out

#1 build a model with only that one effect
#2 yes it's significant, and this is what each of the activities are doing. Always use intervals to address uncertainty!!
#3 

#Heaton still wants an executive summary at the beginning
#No subsections beyond: Seciont 1 ...... Section 2 ......
library(ggplot2)
library(nlme)
library(multcomp)
library(dplyr)
library(car)
library(repmis)
 source("https://raw.githubusercontent.com/MJHeaton/glstools/master/predictgls.R")
 source("https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R")


pollution$ID <- as.factor(pollution$ID)

## EDA
ggplot(data = pollution) +
  geom_boxplot(mapping = aes(x = Activity, y = Aerosol), col = "maroon", fill = "white") +
  labs(x = "Child's Acitivty", y = "PM Measurement on child's vest") +
  coord_flip() + ggtitle("Plots of Activity and PM Measurement") + 
  theme(plot.title= element_text(hjust=0.5))

ggplot(data = pollution) +
  geom_point(mapping = aes(x = Stationary, y = Aerosol), col = "maroon") +
  labs(x = "PM Measurement of the stationary monitor", y = "PM Measurement on child's vest", title = "Scatterplot of Stationary and Aerosol") +
  theme(plot.title = element_text(hjust=0.5))

# definitely some equal variance problems
  # tried a few different transformations but they didn't help at all

ggplot(data = pollution) +
  geom_point(mapping = aes(x = Stationary, y = Aerosol, col=Activity)) +
  labs(x = "PM Measurement of the stationary monitor", y = "PM Measurement on child's vest")
  # pretty but difficult to read

ggplot(data = pollution) +
  geom_boxplot(mapping = aes(x = ID, y = Aerosol), col = "orchid4") +
  geom_boxplot(mapping = aes(x = ID, y = Stationary), col = "dodgerblue1", fill = "dodgerblue1") +
  labs(title = "Blue = Stationary PM, Purple = PM from Vest") +
  coord_flip() #none of them look very different

####################
##Validating a gls model

# create a linear model under the independence assumption and check the correlation matrix structure
stationary.lm <- lm(formula = log(Aerosol) ~ ., data = pollution)
resids.lm <- stdres(stationary.lm)
corr.matrix <- matrix(data = resids.lm, ncol = 118, byrow = TRUE) %>% cor() #all numbers above 0.5 from the looks of it, so there is definitely some temporal correlaion within the resuls for each child
View(corr.matrix)

# find the best model for our correlation

AR1.gls <- gls(model = log(Aerosol)~.-ID-Minute, data = pollution, correlation = corAR1(form = ~Minute|ID), method = "ML")

AIC(AR1.gls)

####################################
## Validate GLS Model Assumptions ##
####################################

#Linearity 
ggplot(data=pollution,mapping=aes(x=Stationary,y=log(Aerosol))) + geom_point() + 
  geom_smooth(method="lm",se=FALSE,col="maroon") + ggtitle("Scatterplot of Log(Aerosol) vs. Stationary PM Measurements") +
  theme(plot.title = element_text(hjust=0.5))

#Independence 
# There isn't independence, but the correlation is accounted for in our gls model
##we need to check that the residuals have actually been decorrelated
new.corr.matrix <- matrix(data=stdres.gls(AR1.gls),ncol=118,byrow = TRUE) %>% cor()
head(new.corr.matrix)
acf.plot <- acf(stdres.gls(AR1.gls),lag.max=40) ##the ACF plot also shows the decorrelation (AR1 captures most of the correlation)
ACF.dframe <- data.frame(Lag=acf.plot$lag, ACF=acf.plot$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col(fill="maroon") + 
  ggtitle("Autocorrelation Plot of Decorrelated Residuals") + 
  theme(plot.title = element_text(hjust=0.5))


library(gridExtra)
library(grid)
grid.newpage()
grid.table(round(head(new.corr.matrix)[,1:6],2))

##Normality of standardized/decorrelated residuals

ggplot()+geom_histogram(mapping=aes(x=stdres.gls(AR1.gls)),color="black",size=1,fill="maroon",binwidth=.4) + ggtitle("Histogram of Standardized Residuals") + 
  labs(x="Standardized & Decorrelated Residuals", y="Frequency") + theme(plot.title = element_text(hjust = 0.5)) 

##Plot of fitted values vs. residuals 
ggplot(data=pollution, mapping=aes(x=fitted(AR1.gls),y=stdres.gls(AR1.gls))) + geom_point(color="maroon") + geom_abline(slope=0, intercept=0,size=1) +
  ggtitle("Fitted Values vs. Residuals") + labs(x="Fitted Values",y="Residuals") + theme(plot.title = element_text(hjust = 0.5))

###########################
## Statistical Inference ##
###########################

##1 
other.model <- gls(model=log(Aerosol)~.-Stationary-Minute,data=pollution, correlation=corAR1(form = ~Minute|ID),method="ML")
anova(other.model,AR1.gls)

confint(AR1.gls)

##2 
summary(AR1.gls) ## it looks like all activities are significant at the .05 level..
intervals <- tableGrob(round(confint(AR1.gls),4))
grid.newpage()
grid.arrange(intervals)

##3 

stationary.int.gls <- gls(model = log(Aerosol)~Stationary + Activity + ID:Stationary, data=pollution,correlation = corAR1(form = ~Minute|ID), method = "ML")

activity.int.gls <- gls(model = log(Aerosol)~Stationary + Activity + ID:Activity, data=pollution,correlation = corAR1(form = ~Minute|ID), method = "ML")

anova(AR1.gls,stationary.int.gls)
anova(AR1.gls,activity.int.gls)

s1 <- (summary(AR1.gls)$sigma)^2
s2 <- (summary(stationary.int.gls)$sigma)^2
s3 <- (summary(activity.int.gls)$sigma)^2

Residual_Variance <- round(c(s1,s2,s3),4)
Model <- c("No Interaction", "Stationary:ID Interaction", "Activity:ID Interaction")
table <- tableGrob(as.data.frame(Residual_Variance, Model))
grid.newpage()
grid.arrange(table)
