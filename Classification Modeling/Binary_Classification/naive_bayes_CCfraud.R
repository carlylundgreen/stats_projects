

library(klaR)
library(DMwR2)
credit <- read.csv("CCFraud.csv",sep=",",header=TRUE)

#Setting outcome variables as categorical
credit.scale <- as.data.frame(cbind(scale(credit[,-c(30)], center = T, scale = T), credit$Class))

ggplot(mapping=aes(x=credit.scale$Amount[credit.scale$V30 == 1])) + geom_histogram()
balanced_credit <- ovun.sample(V30 ~ ., data = credit.scale, method = "both",N=nrow(credit.scale), seed = 1)$data
table(balanced_credit$V30)
table(credit.scale$V30)
## Selected a Naive Bayes Model with Kernel density estimates instead of assumed Gaussian distributions on covariates
## We did not perform a variable selection because the covariates were already principle components from a PCA model 

###################################################################################################
#Out of sample prediction metrics
n.cv <- 50
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
f1 <- rep(NA,n.cv)

bsens <- rep(NA,n.cv)
bspec <- rep(NA,n.cv)
bppv <- rep(NA,n.cv)
bnpv <- rep(NA,n.cv)
bf1 <- rep(NA,n.cv)

n.test <- ceiling(0.25*nrow(credit.scale))
pb <- txtProgressBar(min = 0, max = n.cv, style = 3)

## Begin for loop
for(cv in 1:n.cv){
  
  test.obs <- sample(1:nrow(credit.scale), n.test) #sample from balanced dataset
  test.set <- credit.scale[test.obs,]
  train.set <- credit.scale[-test.obs,]
  train.set$V30 <- as.factor(train.set$V30)
  test.set$V30 <- as.factor(test.set$V30)
  
  btest.set <- balanced_credit[test.obs,]
  btrain.set <- balanced_credit[-test.obs,]
  btest.set$V30 <- as.factor(btest.set$V30)
  btrain.set$V30 <- as.factor(btrain.set$V30)
  
  train.model <- naive_bayes(V30~., data=train.set, prior = NULL, usekernel = TRUE)
  btrain.model <- naive_bayes(V30~.,data=btrain.set,prior=NULL,usekernel= TRUE)
  
  pred.class <- suppressWarnings(predict(train.model,newdata=test.set,type="class"))
  bpred.class <- suppressWarnings(predict(btrain.model,newdata=btest.set,type="class"))
  
  true.class <- test.set$V30 #1 is no, 2 is yes
  btrue.class <- btest.set$V30 
  
  conf.mat0 <- table(true.class,pred.class)
  conf.mat <- addmargins(conf.mat0) 
  bconf.mat0 <- table(btrue.class,bpred.class)
  bconf.mat <- addmargins(bconf.mat0)
  
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3] #yes,yes over the row sum
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2] #predicted yes over total for its column
  
  bsens[cv] <- bconf.mat[2,2]/bconf.mat[2,3] #yes,yes over the row sum
  bppv[cv] <- bconf.mat[2,2]/bconf.mat[3,2] #predicted yes over total for its column
 
  f1[cv] <- 2*(sens[cv]*ppv[cv])/(sens[cv] + ppv[cv])
  bf1[cv] <- 2*(bsens[cv]*bppv[cv])/(bsens[cv] + bppv[cv])
  setTxtProgressBar(pb, cv)
}
close(pb)

sens_uCV <- mean(sens)
ppv_uCV <- mean(ppv)
f1_uCV <- mean(f1)

sens_bCV <- mean(bsens)
ppv_bCV <- mean(bppv)
f1_bCV <- mean(bf1)


#in-sample (unbalanced)
credit.scale$V30 <- as.factor(credit.scale$V30)
umymod <- naive_bayes(V30~., data=credit.scale, prior = NULL, usekernel = TRUE)
preds_u <- predict(umymod, newdata = credit.scale, type = c("class"))
confmat_u <- table(preds_u,credit.scale$V30) %>% addmargins()
sens_u <- confmat_u[2,2]/confmat_u[3,2]
ppv_u <- confmat_u[2,2]/confmat_u[2,3]
f1_u <- 2*(sens_u*ppv_u)/(sens_u + ppv_u)
f1_u
sens_u
ppv_u

#in-sample (balanced)
balanced_credit$V30 <- as.factor(balanced_credit$V30)
mymod <- naive_bayes(V30~., data=balanced_credit, prior = NULL, usekernel = TRUE)
preds1a <- predict(mymod, newdata = balanced_credit, type = c("class"))
confmat <- table(preds1a,balanced_credit$V30) %>% addmargins()
bsens <- confmat[2,2]/confmat[3,2]
bppv <- confmat[2,2]/confmat[2,3]
bf1 <- 2*(bsens*bppv)/(bsens + bppv)
bf1
bsens
bppv

in_sample <- data.frame(Balanced=c(bsens,bppv,bf1),Unbalanced=c(sens_u,ppv_u,f1_u)) %>% t()
out_sample <- data.frame(Balanced=c(sens_bCV,ppv_bCV,f1_bCV),Unbalanced=c(sens_uCV,ppv_uCV,f1_uCV)) %>% t()
colnames(in_sample) <- colnames(out_sample) <- c("Recall","Precision","F1")

in_sample_dat <- in_sample %>% round(4)
out_sample_dat <- out_sample %>% round(4)

in_sample_dat
out_sample_dat
