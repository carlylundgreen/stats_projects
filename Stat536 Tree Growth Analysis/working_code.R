

library(mgcv)
library(gam)
library(ggplot2)
library(dplyr)
library(nlme)
library(MASS)
library(multcomp)
library(geoR)
library(reshape2)
library(car)

set.seed(536)
setwd("~/Grad_School/536")
dat <- read.csv("LodgepoleInUintas.csv",header=TRUE,sep=",")
colnames(dat) <- c("LON","LAT","Slope","Aspect","Elevationation","Basal_Area") #rename response variable

preddat <- dat[is.na(dat$Basal_Area) == TRUE,]
pinedat <- dat[is.na(dat$Basal_Area) == FALSE,]
source("https://raw.githubusercontent.com/MJHeaton/glstools/master/predictgls.R")
source("https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R")

#(exploratory stuff)
middle <- mean(pinedat$Basal_Area)
ggplot(mapping=aes(x=LON,y=LAT,color=Basal_Area),data=pinedat) + geom_point(size=2) + 
  theme_bw() +
  scale_color_gradient2(midpoint=middle, low="darkred", mid="orange",high="yellow") 

#more exploratory plots 
p0 <- ggplot(mapping=aes(x=LON,y=Basal_Area),data=pinedat) + geom_point() + theme_bw() + geom_smooth(se=FALSE)
p1 <- ggplot(mapping=aes(x=LAT,y=Basal_Area),data=pinedat) + geom_point() + theme_bw() + geom_smooth(se=FALSE)
p2 <- ggplot(mapping=aes(x=Aspect,y=Basal_Area),data=pinedat) + geom_point() + theme_bw() + geom_smooth(se=FALSE)
p3 <- ggplot(mapping=aes(x=Slope,y=Basal_Area),data=pinedat) + geom_point() + theme_bw() + geom_smooth(se=FALSE)
p4 <- ggplot(mapping=aes(x=Elevation,y=Basal_Area),data=pinedat) + geom_point() + theme_bw() + geom_smooth(se=FALSE)
grid.arrange(p2,p3,p4,ncol=1)

grid.arrange(p0,p1,ncol=2)

######evidence of spatial correlation: 
##Fit lm model and create variogram (explore initial spatial correlation)
pine_lm <- lm(Basal_Area~Aspect + Slope + Elevation,data=pinedat)

#variogram
locations <- as.matrix(pinedat[,1:2])
resids <- stdres(pine_lm)
variogram <- variog(coords=locations,data=resids)
plot(variogram,pch=19, col="navyblue",xlab="Distance",ylab="Semivariance") #as distance increases, variance tends to increase
#there definitely looks to be spatial correlation here

#scatterplot of residuals from lm()
middle <- mean(pinedat$Basal_Area)
Resids <- resids
ggplot(mapping=aes(x=LON,y=LAT,color=Resids),data=pinedat) + geom_point(size=2) + 
  theme_bw() #+
  scale_color_gradient2(midpoint=middle, low="darkred", mid="orange",high="yellow") 


#gam to plot 
gamtest <- mgcv::gam(Basal_Area ~ s(LON) + s(LAT) + s(Slope) + s(Aspect) + s(Elevation),data=pinedat)
par(mfrow=c(2,3))
plot(gamtest,se=TRUE,col="red")
par(mfrow=c(1,1))

#gams to evaluate (ie function selection)
gamfull <- gam::gam(Basal_Area ~ s(LON) + s(LAT) + s(Slope) + s(Aspect) + s(Elevation),data=pinedat)

#look at LON variable 
gam1 <- gam::gam(Basal_Area ~ LON + s(LAT) + s(Slope) + s(Aspect) + s(Elevation),data=pinedat)
gam2 <- gam::gam(Basal_Area ~ s(LAT) + s(Slope) + s(Aspect) + s(Elevation),data=pinedat)
a1 <- anova(gamfull,gam1,gam2)
#we want a curved function on Longitude (first pvalue says to keep curved function, second pval says include variable)
pvals1 <- a1$`Pr(>Chi)`[-1] %>% round(3)

#look at LAT 
gam3 <- gam::gam(Basal_Area ~ s(LON) + LAT + s(Slope) + s(Aspect) + s(Elevation),data=pinedat)
gam4 <- gam::gam(Basal_Area ~ s(LON) + s(LAT) + s(Aspect) + s(Elevation),data=pinedat)
a2 <- anova(gamfull,gam3,gam4)
#we don't need curvature on LAT but we do need it in the model 
pvals2 <- a2$`Pr(>Chi)`[-1] %>% round(3)

#look at Slope 
gam5 <- gam::gam(Basal_Area ~ Slope + s(Aspect) + s(Elevation),data=pinedat)
gam6 <- gam::gam(Basal_Area ~  s(Aspect) + s(Elevation),data=pinedat)
a3 <- anova(gamfull,gam5,gam6)
#we need slope in the model and with a spline 
pvals3 <- a3$`Pr(>Chi)`[-1] %>% round(3)

#look at aspect
gam7 <- gam::gam(Basal_Area ~ s(Slope) + Aspect + s(Elevation),data=pinedat)
gam8 <- gam::gam(Basal_Area ~ s(Slope) + s(Elevation),data=pinedat)
a4 <- anova(gamfull,gam7,gam8)
#we need curvature on aspect in the model 
pvals4 <- a4$`Pr(>Chi)`[-1] %>% round(3)

#look at Elevation
gam9 <- gam::gam(Basal_Area ~ s(Slope) + s(Aspect) + Elevation,data=pinedat)
gam10 <- gam::gam(Basal_Area ~ s(Slope) + s(Aspect),data=pinedat)
a5 <- anova(gamfull,gam9,gam10)

#need curvature on Elevation in the model 
pvals5 <- a5$`Pr(>Chi)`[-1] %>% round(3)

####NOTE: not including lat or lon because we are asked in the research q's about how the ENVIRONMENT affects basal area
#lon and lat (ie location) is useful to understand spatial correlation--we won't use it as a main effect
#when we did use lat and lon as main effects, it said slope was not significant, but I don't think that makes sense
#I would imagine that if the slope is super steep, very large and heavy trees won't grow there

var_tbl <- data.frame(Variable=c("Aspect","Slope","Elevation"),
                      Anova1=c(pvals3[1],pvals4[1],pvals5[1]),
                      Anova2=c(pvals3[2],pvals4[2],pvals5[1]))

kableExtra::kable(var_tbl) + kable_styling(position="center",latex_options="HOLD_position")

##### Now that we have the regression part, take care of autocorrelation 
#model looks like this so far: 
#gam(Basal_Area ~ s(LON) + LAT + s(Aspect) + s(Elevation),data=pinedat)
#Does GLS give same model?

#now we need to account for spatial autocorrelation 

##Create a heat map of the temperatures 
#ggplot(data=dat, mapping=aes(x=LON, y=LAT,fill=Basal_Area)) + geom_raster() + 
#  scale_fill_distiller(palette="Spectral",na.value=NA)

#which correlation structure to use? 
expmod <- gls(Basal_Area ~ s(Aspect) + s(Elevation), data=pinedat,correlation = corExp(form = ~ LAT + LON,nugget=TRUE))
gaussmod <- gls(Basal_Area ~ s(Aspect) + s(Elevation), data=pinedat,correlation = corGaus(form = ~ LAT + LON,nugget=TRUE))
sphermod <- gls(Basal_Area ~ s(Aspect) + s(Elevation), data=pinedat,correlation = corSpher(form = ~ LAT + LON,nugget=TRUE))

#compare AIC values
#definitely need to use Gaussian 

str_tb <- data.frame(Structure=c("Exponential","Gaussian","Spherical"),
                     AIC=c(AIC(expmod),
                           AIC(gaussmod),
                           AIC(sphermod)))

kableExtra::kable(str_tb) + kable_styling(position="center",latex_options="HOLD_position")

#we didn't learn gaussian correlation structure in lecture so using exponential correlation

#to answer the research questions, we will intepret the significance of the smoothed terms 
#(ie look at the univariate plots for patterns based on significance

gls1 <- gls(Basal_Area~ s(Aspect) + s(Slope) + s(Elevation),data=pinedat) #really OLS (no error term modeling)

#select smoothing parameter for univariate fits with cross validation 
s1 <- smooth.spline(x = pinedat$Aspect, y = pinedat$Basal_Area, cv = TRUE)
spar1 <- s1$spar %>% round(3)
s2 <- smooth.spline(x = pinedat$Slope, y = pinedat$Basal_Area, cv = TRUE)
spar2 <- s2$spar %>% round(3)
s3 <- smooth.spline(x = pinedat$Elevation, y = pinedat$Basal_Area, cv = TRUE)
spar3 <- s3$spar %>% round(3)

#my model: 
gls2 <- gls(Basal_Area~ s(Aspect) + s(Slope) + s(Elevation),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))

#list smoothing parameters for each variable 
spardf <- data.frame(Aspect=spar1,Slope=spar2,Elevation=spar3) %>% t()
colnames(spardf) <- c("Smoothing Parameter")
kableExtra::kable(spardf) + kable_styling(position="center",latex_options="HOLD_position")

#compare AIC values for OLS and GLS (to confirm we are on the right track)
compare_IC <- data.frame(AIC=c(AIC(gls1),AIC(gls2)),
                         BIC=c(BIC(gls1),BIC(gls2)))
rownames(compare_IC) <- c("OLS","GLS")
kableExtra::kable(compare_IC) + kable_styling(position="center",latex_options="HOLD_position")

#### assumptions? 

##Check variogram (decorrelated resids)
locations <- as.matrix(pinedat[,1:2])
resids <- stdres.gls(gls2)
variogram <- variog(coords=locations,data=resids)
par(mfrow=c(1,1))
plot(variogram,pch=19, col="navyblue",xlab="Distance",ylab="Semivariance") 
abline(h=1,col="red",lty=2)

#as distance increases, variance tends to increase (same thing we saw in lm b/c same model)

#anova(gls1,gls2) #we definitely want to account for correlation 

##check normality of decorrelated residuals 
ggplot()+geom_histogram(data=pinedat,mapping=aes(x=stdres.gls(gls2)),
                        color="white",size=1,fill="navyblue",binwidth=.4) + 
                        ggtitle("Histogram of Standardized Residuals") + 
                        labs(x="Standardized & Decorrelated Residuals", y="Frequency") + 
                        theme(plot.title = element_text(hjust = 0.5)) +
                        theme_bw() + stat_function(fun=dnorm)


plotdf <- data.frame(Std_Resids=stdres.gls(gls2),Std_Normal=rnorm(mean=0,sd=1,n=114))
pltdat <- melt(plotdf)

ggplot(pltdat,aes(x=value, fill=variable)) + geom_density(alpha=0.3) + 
  labs(x="Standard Deviation","Density") + 
  ggtitle("Standardized Residuals vs. Standard Normal") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_bw()

##check equal variance assumption
ggplot(data=pinedat, mapping=aes(x=fitted(gls2),y=stdres.gls(gls2))) + 
  geom_point(color="darkblue") + geom_abline(slope=0, intercept=0,size=1) +
  ggtitle("Fitted Values vs. Residuals") + labs(x="Fitted Values",y="Standardized Residuals") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


##run 50 CV studies to assess predictive accuracy 
n.cv <- 50
n.test <- round(nrow(pinedat)*0.2)
rpmse <- numeric()
bias <- numeric()
width <- numeric()
cvg <- numeric()
rpmse.lm <- numeric()
bias.lm <- numeric()
width.lm <- numeric()
cvg.lm <- numeric()
pb <- txtProgressBar(min = 0, max = n.cv, style = 3)

for(cv in 1:n.cv){
  
  test.obs <- sample(1:nrow(pinedat), n.test)
  test.set <- pinedat[test.obs,]
  train.set <- pinedat[-test.obs,]
  
  ##fit the gls model 
  gls2.train <- gls(Basal_Area~ s(Aspect) + s(Slope) + s(Elevation), data=train.set,
                    correlation=corGaus(form=~LAT+LON,nugget=TRUE))
  train.lm <- lm(Basal_Area ~ Aspect + Slope + Elevation, data=train.set)
  
  ##predictions for the training set 
  preds <- predictgls(gls2.train,newdframe = test.set) ##GLS preds
  preds_lm <- predict.lm(train.lm, newdata = test.set,interval = "prediction")
  
  ##rpmse 
  rpmse[cv] <- (preds$Prediction - test.set$Basal_Area)^2 %>% mean() %>% sqrt()
  rpmse.lm[cv] <- (preds_lm[,'fit'] - test.set$Basal_Area)^2 %>% mean() %>% sqrt()
  
  ##bias 
  bias[cv] <- (preds$Prediction - test.set$Basal_Area) %>% mean()
  bias.lm[cv] <- (preds_lm[,'fit'] - test.set$Basal_Area) %>% mean()
  
  ##width 
  width[cv] <- mean(preds$upr - preds$lwr)
  width.lm[cv] <- mean(preds_lm[,'upr'] - preds_lm[,'lwr'])
  
  ##coverage
  cvg[cv] <- ((preds$lwr < test.set$Basal_Area) & (preds$upr > test.set$Basal_Area)) %>% mean()
  cvg.lm[cv] <- ((preds_lm[,'lwr'] < test.set$Basal_Area) & (preds_lm[,'upr'] > test.set$Basal_Area)) %>% mean()
  
  ##update progress bar
  setTxtProgressBar(pb, cv)
}
close(pb)

##compare predictive accuracy metrics from GLS and MLR models
gls_metrics <- c(mean(rpmse),mean(bias),mean(width) , mean(cvg))
ols_metrics <- c(mean(rpmse.lm), mean(bias.lm), mean(width.lm), mean(cvg.lm))
#I mean, they're pretty similar (weirdly), so I'm just going to report GLS results 

compare_table <- as.data.frame(gls_metrics) %>% t()
colnames(compare_table) <- c("RPMSE","Bias","Width","Coverage")
rownames(compare_table) <- c("")

#the IQR is about 7.3, and on average we are off by about 4.5. This is probably not great, but not horrible
#the width is pretty wide though
kableExtra::kable(compare_table) + kable_styling(position="center",latex_options="HOLD_position")

Std_Resids <- stdres.gls(gls2)
mid <- mean(Std_Resids)
ggplot(mapping=aes(x=LON,y=LAT,color=Std_Resids),data=pinedat) + geom_point(size=2) + 
  scale_color_gradient2(midpoint=mid, low="darkred", mid="orange",high="yellow") + theme_bw()

#### Results and research questions 
# added variable plots 

#Aspect vs Basal Area after controlling for other variables 

no_aspect <- gls(Basal_Area~ s(Slope) + s(Elevation),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))
aspect_as_y <- gls(Aspect~ s(Slope) + s(Elevation),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))
resids_y <- stdres.gls(no_aspect)
resids_x <- stdres.gls(aspect_as_y)

g1 <- ggplot(mapping=aes(x=resids_x,y=resids_y)) + geom_point(size=2) + 
  theme_bw() + labs(x="Aspect | Others",y="Basal Area | Others") + geom_smooth(se=FALSE)

#control for Slope
no_slope <- gls(Basal_Area~ s(Aspect) + s(Elevation),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))
slope_as_y <- gls(Slope ~ s(Aspect) + s(Elevation),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))
resids_y1 <- stdres.gls(no_slope)
resids_x1 <- stdres.gls(slope_as_y)

g2 <- ggplot(mapping=aes(x=resids_x1,y=resids_y1)) + geom_point(size=2) + 
  theme_bw() + labs(x="Slope | Others",y="Basal Area | Others") + geom_smooth(se=FALSE)

#control for elevation 
no_elev <- gls(Basal_Area~ s(Aspect) + s(Slope),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))
elev_as_y <- gls(Elevation ~ s(Aspect) + s(Slope),data=pinedat,correlation=corGaus(form=~LAT+LON,nugget=TRUE))
resids_y2 <- stdres.gls(no_elev)
resids_x2 <- stdres.gls(elev_as_y)

g3 <- ggplot(mapping=aes(x=resids_x2,y=resids_y2)) + geom_point(size=2) + 
  theme_bw() + labs(x="Elevation | Others",y="Basal Area | Others") + geom_smooth(se=FALSE)

grid.arrange(g1,g2,g3,ncol=1)

# Create and map predictions of the temperature at each location that was impeded by cloud cover.
preds <- predictgls(gls2, newdframe = preddat)
preds_integrate <- as.data.frame((preds[,-6])) #get rid of NA column 

##integrate predictions with original data and plot together on a map: 
colnames(preds_integrate) <- c("LON","LAT","Slope","Aspect","Elevation","Basal_Area","SE.pred","lwr","upr") #rename prediction column to be "Temp"
preds_integrate <- preds_integrate[-(7:9)] 

combined <- data.frame(rbind(pinedat,preds_integrate)) #combine predictions and original data
combined$Type <- c(rep("Prediction",78),rep("Actual",114))
ggplot(mapping=aes(x=LON,y=LAT,color=Basal_Area,shape=Type),data=combined) + geom_point(size=2) + theme_bw()

#ggplot(data=combined, mapping=aes(x=Lon, y=Lat, fill= Temp)) + 
#  geom_raster() +
 # scale_fill_distiller(palette="Spectral",na.value=NA) ## plot the combined data to fill in the gaps left from the cloud cover

library(devtools)
library(ggmap)
library(colorspace)
#devtools::install_github("dkahle/ggmap")
register_google(key="AIzaSyDB1RLOM0e8npEnjWlkXXXDOeBUDSUS1Ks")

mybox <- c(left = -111.2, bottom = 40.45, right =
                    -109.2, top = 41.03)
mymap <- get_stamenmap(mybox)
bb <- make_bbox(lon=LON, lat=LAT, data=combined)
mymap2 <- get_map(location=bb, maptype="roadmap",zoom=9)

ggmap(mymap2) +
  geom_point(data=combined, aes(x=LON,y=LAT,color=Basal_Area),size=1.7) 

ggmap(mymap) +
  geom_point(data=combined, aes(x=LON,y=LAT,color=Basal_Area),size=1.7) #+ 
  #scale_color_gradient2(midpoint=mean(combined$Basal_Area), low="navyblue",mid="lightblue",high="blue") 



