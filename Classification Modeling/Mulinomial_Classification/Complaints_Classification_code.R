

dat <- read.csv("cleaned_complaints.csv",header=TRUE,sep = ",")
testdat <- read.csv("test_data_cleaned.csv",header=TRUE,sep=",")
#drop "X" column and complaints column
testdat <- testdat[,-c(1,2)]

set.seed(536)
n.test <- ceiling(.33*nrow(dat2))
test.obs <- sample(1:nrow(dat2), n.test)

test.set <- dat2[test.obs,]
train.set <- dat2[-test.obs,]
dim(test.set)
dim(train.set)

test.set[1:3,1:3]
train.set[1:3,1:3]

#################################################################################################
#cross validate to tune parameters 

library("xgboost")
#training parameters on 0.2*0.33 proportion of the data

n.test.small <- ceiling(0.2*nrow(test.set))
test.obs.small <- sample(1:nrow(test.set),n.test.small)

train_set_small <- train.set[-test.obs.small,]
test_set_small <- test.set[test.obs.small,]

dim(train_set_small)
dim(test_set_small)

dtrain <- xgb.DMatrix(as.matrix(train_set_small[, -1]), 
                      label = as.numeric(as.factor(train_set_small$Department)) -1)

#Note: levels are: 
#3: Debt collection  
#2: Credit reporting, credit repair services, or other personal consumer reports
#7: Student loan
#5: Mortgage 
#0: Checking or savings account
#1: Credit card or prepaid card 
#6: Payday loan, title loan, or personal loan
#8: Vehicle loan or lease
#4: Money transfer, virtual currency, or money service
                                                                       
dtest <- xgb.DMatrix(as.matrix(test_set_small[,-1]), 
                     label = as.numeric(as.factor(test_set_small$Department))-1)

params <- list(booster = "gbtree", objective = "multi:softmax", eta=.03, gamma=0,max_depth=8,min_child_weight=1,subsample=1,colsample_bytree=1)

watchlist <- list(train = dtrain, eval = dtest) #what is this

cv_model <- xgb.cv(params = params,
                   data = dtrain, 
                   nrounds = 30, 
                   watchlist = watchlist,
                   nfold = 5,
                   verbose = FALSE,
                   num_class = 9,
                   eval_metric = "merror",
                   prediction = TRUE)

cv_model$evaluation_log %>%
  filter(test_merror_mean == min(test_merror_mean))

#first default - model training
xgb1 <- xgb.train(params = params, data = dtrain , nrounds = 30, watchlist = list(eval=dtest,train=dtrain),
                   eval_metric = "merror",num_class=9)

#model prediction
xgbpred <- predict(xgb1,dtest)

#confusion matrix
library(caret)

labels <- train_set_small$Department 
ts_label <- test_set_small$Department

#convert factor to numeric 
labels <- as.numeric(as.factor(labels))-1
ts_label <- as.numeric(as.factor(ts_label))-1

confusionMatrix(as.factor(xgbpred), as.factor(ts_label))

new_tr <- model.matrix(~.+0,data = train_set_small[,-1]) 
new_ts <- model.matrix(~.+0,data = test_set_small[,-1])

#view variable importance plot
mat <- xgb.importance(feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:10],col="royalblue") 
dim(mat)
mat

dtest <- xgb.DMatrix(as.matrix(testdat))

yhat.test.preds <- predict(xgb1, dtest, missing = NULL,
                             outputmargin = FALSE, ntreelimit = NULL, predleaf = FALSE)

#rename levels 
  #3: Debt collection  
  #2: Credit reporting, credit repair services, or other personal consumer reports
  #7: Student loan
  #5: Mortgage 
  #0: Checking or savings account
  #1: Credit card or prepaid card 
  #6: Payday loan, title loan, or personal loan
  #8: Vehicle loan or lease
  #4: Money transfer, virtual currency, or money service

getname <- function(input){
  if(input == 3){
    return("Debt collection")
  } else if (input == 2){
    return("Credit reporting...")
  } else if (input == 7){
    return("Student Loan")
  } else if (input == 5){
    return ("Mortgage")
  } else if (input == 0){
    return ("Checking or Savings")
  } else if (input == 1){
    return ("Credit card or prepaid card")
  } else if (input == 6){
    return("Payday, title, or personal loan")
  } else if (input == 8){
    return("Vehicle loan or lease")
  } else if (input == 4){
    return ("Money transfer, virtual currency, or money service")
  }
}

test_predictions <- sapply(yhat.test.preds,getname)

final_preds <- data.frame(cnum = seq(1,10,by=1),preds = test_predictions)
colnames(final_preds) <- c("Complaint Number", "Department")

write.csv(final_preds,"predictions_table.csv")
