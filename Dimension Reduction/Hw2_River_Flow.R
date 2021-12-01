library(dplyr)
library(kableExtra)
setwd('~/Grad_School/536')
riv <- read.csv("Rivers.csv",sep=",",header=TRUE)


## PCA
library(pls)
library(ggplot2)
#justify dropping these b/c they are zeroed out by our lasso analysis 
#also the standard deviations of these columns are zero so we can't scale the x's 
#talk about why scaling is important to justify dropping variables 
riv <- riv[,-which(colnames(riv) %in% c("meanPercentDC_VeryPoor","meanPercentDC_Well"))]
x = model.matrix(Metric ~ ., data=riv)[,-1]

y = riv$Metric
hist(y)
ggplot(mapping=aes(x=riv$Metric)) + geom_histogram(bins=25,fill="darkblue",color="white") +
  labs(x="Metric",y="Frequency") +  ggtitle("Histogram of Response (Metric)") +
  theme(plot.title = element_text(hjust = 0.5))

corrs <- cor(riv)[,1]
corrs_df <- tail(corrs[order(abs(cor(riv)[,1]))])[-6] 
kable(corrs_df) %>% kable_styling(position="center")

set.seed(536)
train = sample(1:nrow(x), 85)
test = (-train)
y.test = y[test]
x.test = x[test,]

#run on test/train set
pcr.fit1=pcr(Metric~., data=riv,subset=train, scale =TRUE,validation="CV")
validationplot(pcr.fit1,val.type="RMSEP")

#look at RMSEs from CV for each number of components
cverr <- RMSEP(pcr.fit1)$val[1,,]
which.min(cverr) - 1 

#MSE using 12 components
pcr.fit <- pcr(Metric~.,data=riv,scale=TRUE,ncomp=12)
pcr.pred <- predict(pcr.fit, riv, ncomp=12)
mean((pcr.pred -y.test)^2) %>% sqrt() #RMSE
rsquared <- 1 - (sum((y - pcr.pred)^2)/sum((y - mean(y))^2)) #??

#find cross-validated mse, bias
set.seed(536)
rmse <- c()
bias <- c()
rsquared <- c()
for(i in 1:500){
  train = sample(1:nrow(riv), 85)
  test = (-train)
  traindat = riv[train,]
  pcr.fit=pcr(Metric~., data=traindat, scale =TRUE,ncomp=12)
  pcr.pred <- predict(pcr.fit, x[test,]) 
  rmse[i] <- mean((pcr.pred -y[test])^2) %>% sqrt() #RMSE
  bias[i] <- mean(pcr.pred -y[test])
}

mean(rmse)
hist(rmse)
mean(bias)
hist(bias)

#which have the highest effects?
coefs_final <- pcr.fit$coefficients[,,12] %>% as.data.frame()
colnames(coefs_final) <- c("val_coef")

coefs_final$val_coef <- abs(coefs_final$val_coef)
coefs_final <- tibble::rownames_to_column(coefs_final)
ordered_dat <- coefs_final %>% arrange(val_coef)
tail(ordered_dat) #these are the genes with the highest effects (in value)

pcr.fit$coefficients[,,12] %>% as.matrix()

lc <- as.matrix(pcr.fit$coefficients[,,12])

##bootstrap for confidence interval for beta
Nboot <- 1000
mat1 <- matrix(NA, nrow=nrow(lc), ncol=Nboot)

for(i in 1:Nboot){
  sample_1 <- riv[sample(1:nrow(riv), replace=TRUE),]
  X <- model.matrix(Metric~., data=sample_1)[,-1]
  Y <- sample_1$Metric
  dat <- cbind(Y,X) %>% as.data.frame()
  pcr.fit <- pcr(Y~., data=dat, scale =TRUE,ncomp=13)
  mat1[,i] <- as.matrix(pcr.fit$coefficients[,,13])
}

#create final matrix
param_names <- pcr.fit$coefficients[,,13] %>% as.data.frame()
rownames(param_names)
mat <- cbind(rownames(param_names),mat1)

coefs_mat <- mat[which(mat[,1] %in% tail(ordered_dat)$rowname), ]
coefs_df <- coefs_mat %>% as.data.frame()

final_mat <- as.matrix(coefs_df)
numeric_vals <- final_mat[,-1] %>% as.numeric() 
final_mat2 <- matrix(numeric_vals,nrow=6,ncol=1000,byrow=FALSE)

#check that I didn't mess up the placement of the values
#final_mat2[,1]
#final_mat[,2] %>% as.numeric()

ests <- rowMeans(final_mat2)
ints <- apply(final_mat2,1,FUN=quantile,probs=c(0.025,0.975)) %>% t()
Variable <- as.matrix(coefs_df$V1) 

#format final table of bootstrapped values 
data <- cbind(Variable, ests,ints) %>% as.data.frame()
colnames(data) <- c("Variable","Estimate","2.5%","97.5%")
data$Estimate <- data$Estimate %>% as.character() %>% as.numeric() %>% round(3)
data$`2.5%` <- data$`2.5%` %>% as.character() %>% as.numeric() %>% round(3)
data$`97.5%` <- data$`97.5%` %>% as.character() %>% as.numeric() %>% round(3)

target <- tail(ordered_dat)$rowname %>% rev() #we want strongest to weakest
target <- rev(target)
ord_df <- data[match(target, data$Variable),] #order by what PCA said was strongest --> weakest

kable(ord_df) %>% kableExtra::kable_styling(position="center")
ord_df
