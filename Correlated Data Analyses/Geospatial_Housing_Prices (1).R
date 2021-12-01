########## Libraries
library(ggplot2)
library(car)
library(nlme)
library(MASS) #stdres
library(dplyr)
library(geoR) #variog
library(lmtest)
library(kableExtra)
source("https://raw.githubusercontent.com/MJHeaton/glstools/master/predictgls.R")
source("https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R")

########## Research questions and how we will answer them
## 1 How well do the home characterisitics explain sale price? --> pseudo r^2
## 2 What factors increase the sale price of a home? --> beta coefficients
## 3 Does the variability of sale price increase with the size of the home (as given by living area)?
    # I think this has to do with identifying heteroskedasticity?
## 4 What is your predicted/appraised sale price for the homes in the dataset that do not have a sale price?
    # do cross validation and then prediction

########## Read in the data

housing <- read.csv(file = "https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/3%20-%20Project/Data/HousingPrices.csv",
                    header = TRUE)
kable(head(housing)) %>%
  kable_styling(bootstrap_options = c("striped"))

## we need to get rid of the NA's 
housing_obs <- housing %>% filter(!is.na(Price))
housing_na <- housing %>% filter(is.na(Price))

#########
## EDA ##
#########

########## Plots
# not the best plot. remember that we can see correlation the best through the residuals
ggplot(data = housing_obs, mapping = aes(x = Lon, y = Lat, col = Price)) +
  geom_point() +
  scale_color_distiller(palette = "RdBu", na.value = NA) 

# scatterplot of Longitude and Latitude colored in by the residuals
# notice that the positive residuals are all clumped together in the top right corner
housing_lm <- lm(Price ~. -Lon -Lat, data = housing)
resids_lm <- stdres(housing_lm)
ggplot(data = housing_obs, mapping = aes(x = Lon, y = Lat, col = resids_lm)) +
  geom_point() + 
  labs(x = "Longitude", y = "Latitude") +
  scale_color_distiller(palette = "RdBu", na.value = NA) #possible spatial correlation

# box plot of price and whether or not they have central air
ggplot(mapping = aes(x = Central.Air, y = Price), data = housing_obs) +
  geom_boxplot(col = "orchid4") + 
  labs(x = "Central Air", y = "Price")

# scatterplot of Price and Above-Gorund Living Area
ggplot(mapping = aes(x = Gr.Liv.Area, y = Price), data = housing_obs) +
  geom_point(col = "dodgerblue") +
  labs(x = "Above-Ground Living Area in SqFt", y = "Price of Home") #yikes possible heterskedasticity

# boxplot of house style and price
ggplot(mapping = aes(x = House.Style, y = Price), data = housing_obs) +
  geom_boxplot(col = "orchid4") + 
  labs(x = "House Style", y = "Price")

# variogram to see the spatial correlation
coords <- housing_obs[,2:3]
variogram <- variog(coords = coords, data = resids_lm)
plot(variogram) #yikes look at that spatial correlation

########## Statistics
cor(housing_obs$Price, housing_obs$Gr.Liv.Area) #0.8372 strong positive linear relationship
cor(housing_obs$Price, housing_obs$Year.Remod.Add) #0.5658 moderately strong 
cor(housing_obs$Price, housing_obs$Garage.Cars) #0.7613 strong positive 
cor(housing_obs$Price, housing_obs$Bedroom.AbvGr) #0.2509 weak positive
cor(housing_obs$Price, housing_obs$Full.Bath) #0.6764 moderate positive

#####################
## Fitting a Model ##
#####################

## double check on the heterskedasticity that we saw earlier
bptest(housing_lm) #p-value < 0.0001 so we reject H0 that the variance is constant

## check the different correlation structures to find which one we should use

gls_exp <- gls(model = Price ~. -Lon -Lat, data = housing_obs, weights = varExp(form = ~Gr.Liv.Area), 
                 correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = "ML")
gls_gauss <- gls(model = Price ~. -Lon -Lat, data = housing_obs, weights = varExp(form = ~Gr.Liv.Area),
                 correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), method = "ML")
gls_spherical <- gls (model = Price ~. -Lon -Lat, data = housing_obs, weights = varExp(form = ~Gr.Liv.Area),
                      correlation = corSpher(form = ~ Lon + Lat, nugget = TRUE), method = "ML")

# find lowest AIC
AIC(gls_exp) #AIC  = 10072.45 <-- this is the winner
AIC(gls_gauss) #AIC = 10072.9
AIC(gls_spherical) #AIC = 10073.24

#######################
## Model Assumptions ##
#######################

## Lineartiy
avPlots(housing_lm) #what does it mean when there isn't really any line like for full and half baths?

## Independence
decor_resids <- stdres.gls(gls_exp)
variogram2 <- variog(coords = coords, data = decor_resids)
plot(variogram2, pch= 19, col = "dodgerblue") #much better except for the last point 

## Normality of the residuals
qplot(x = decor_resids, geom = "histogram") #looks normal
ggplot(data = housing_obs, mapping = aes(x = decor_resids)) +
  geom_histogram(col = "dodgerblue") +
  labs(x = "Decorrelated Standardized Residuals", y = "Count")

## Equal Variance
# not really since we are adjusting for heteroskedasticity
ggplot(data = housing_obs, mapping = aes(x = fitted(gls_exp), y = decor_resids)) +
  geom_point(col = "dodgerblue") +
  geom_abline(slope = 0, intercept = 0, col = "red") +
  labs(x = "Fitted Values", y = "Residuals") #looks good

######################
## Cross Validation ##
######################

########## Figure out how much time this is going to take
system.time({
  gls_exp <- gls(model = Price ~. -Lon -Lat, data = housing_obs, weights = varExp(form = ~Gr.Liv.Area), 
                 correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = "ML")
}) #user 33.45, system 0.52, elapsed 34.23 --> we only care about elapsed time

########## Define variables
n_cv <- 50
n_samps <- round(x = nrow(housing_obs) * 0.8, digits = 0)
rpmse <- numeric()
bias <- numeric()
width <- numeric()
coverage <- numeric()
preds <- numeric
pb <- txtProgressBar(min = 0, max = n_cv, style = 3)

########## Cross Validate
set.seed(76)
for(i in 1:n_cv) {
  # split into train and test sets
  rows <- sample(x = nrow(housing_obs), size = n_samps)
  housing_train <- housing_obs[rows,]
  housing_test <- housing_obs[-rows,]
  
  # get the model
  my_gls_exp <- gls(model = Price ~. -Lon -Lat, data = housing_train, weights = varExp(form = ~Gr.Liv.Area), 
                 correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = "ML")
  
  # make predictions
  preds <- predictgls(my_gls_exp, newdframe = housing_test)
  
  # get info
  rpmse[i] <- (preds$Prediction - housing_test$Price)^2 %>% mean() %>% sqrt()
  
  bias[i] <- (preds$Prediction - housing_test$Price) %>% mean()
  
  width[i] <- (preds$upr - preds$lwr) %>% mean()
  
  coverage[i] <- mean((preds$upr > housing_test$Price) && (preds$lwr < housing_test$Price))
  
  # make a progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

########## Cross Validation on just the lm()
rpmse_lm <- numeric()
bias_lm <- numeric()
width_lm <- numeric()
coverage_lm <- numeric()

set.seed(76)
for(i in 1:n_cv) {
  # split into train and test sets
  rows <- sample(x = nrow(housing_obs), size = n_samps)
  housing_train <- housing_obs[rows,]
  housing_test <- housing_obs[-rows,]
  
  # get the model
  my_lm <- lm(formula = Price ~. -Lon -Lat, data = housing_train)
  
  # make predictions
  preds_lm <- predict.lm(my_lm, newdata = housing_test, interval = "prediction")
  
  # get info
  rpmse_lm[i] <- (preds_lm[,'fit'] - housing_test$Price)^2 %>%
    mean() %>%
    sqrt()
  
  bias_lm[i] <- (preds_lm[,'fit'] - housing_test[,'Price']) %>% mean
  
  width_lm[i] <- mean(preds_lm[,'upr'] - preds_lm[,'lwr'])
  
  coverage_lm[i] <- mean((preds_lm[,'lwr'] < housing_test$Price) & (preds_lm[,'upr'] > housing_test$Price))
}

########## Check Prediciton Validations for gls()
mean(rpmse) #13777.96
mean(bias) #36.16
mean(width) #50937.31
mean(coverage) #0.9

########## Check Prediciton Validations for lm()
mean(rpmse_lm) #18439.95
mean(bias_lm) #-183.5839
mean(width_lm) #69925.54
mean(coverage_lm) #0.9369892

###########################
## Statistical Inference ##
###########################

########## How well do the home characterisitics explain sale price?
# pseudo r^2
(cor(housing_obs$Price, fitted(gls_exp)))^2 #0.93 which is pretty darn good

MLR_Model <- c(18439.95, -183.5839, 69925.54, 0.9369892)
GLS_Model <- c(13777.96, 36.16, 50937.31, 0.9)
comparison <- as.data.frame(cbind(GLS_Model, MLR_Model))
row.names(comparison) <- c("RPMSE", "Bias", "Width", "Coverage")
comparison <- round(comparison, digits = 2)
kable(comparison) %>%
  kable_styling(bootstrap_options = c("striped"))

########## What factors increase the sale price of a home?
# beta.hat coefficients
coef(gls_exp)
dt <- cbind(round(confint(gls_exp), digits = 2), round(coef(gls_exp), digits = 2))
dt <- cbind(dt, c(0.00, 0.00, 0.00, 0.69, 0.00, 0.00, 0.13, 0.77, 0.00, 0.00))
dt <- as.data.frame(dt)
names(dt) <- c("Lower Bound", "Upper Bound", "Fit", "P-Value")
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

########## Does the variability of sale price increase with the size of the home (as given by living area)?
# Yes, as seen by the plot below 
ggplot(mapping = aes(x = Gr.Liv.Area, y = Price), data = housing_obs) +
  geom_point(col = "dodgerblue") +
  labs(x = "Above-Ground Living Area in SqFt", y = "Price of Home")
intervals(gls_exp)

########## What is your predicted/appraised sale price for the homes in the dataset that do not have a sale price?
preds_na <- predictgls(gls_exp, newdframe = housing_na)
housing_na$Price <- preds_na$Prediction
na_prices <- as.data.frame(preds_na$Price)
kable(housing_na) %>%
  kable_styling(bootstrap_options = c("striped")) #nice table of predictions

gls_na <- gls(model = Price ~. -Lon -Lat, data = housing_na, weights = varExp(form = ~Gr.Liv.Area), 
              correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = "ML")
resids_na <- stdres.gls(gls_na)
# I want to make a plot with both the decor_resids and the resids_na
ggplot(data = housing_obs, mapping = aes(x = Lon, y = Lat, col = decor_resids)) +
  geom_point() + 
  labs(x = "Longitude", y = "Latitude") +
  scale_color_distiller(palette = "RdBu", na.value = NA) #look s a lot better





  