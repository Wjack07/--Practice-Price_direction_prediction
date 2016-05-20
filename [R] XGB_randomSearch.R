require(xgboost)
require(methods)
library(randomForest)
#setwd("D:/Google Drive/BigData/Kaggle/[Case 3] Otto Group Product Classification Challenge")
setwd("C:/Users/jsc69/Google Drive/BigData/Kaggle/[Self Case] Indices prediction")

train = read.csv("Train_small.csv",header=TRUE,stringsAsFactors = F)
test = read.csv("Test_small.csv",header=TRUE,stringsAsFactors = F)
id_train=train[1]
id_test=test[1]
train = train[,-1]    ## remove the 'id' col
test = test[,-1]      ## remove the 'id' col

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  #pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  #pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(abs(act[,1]-pred[,1]))    
  #ll = ll /(nrow(act))      
  return(nr);
}

train_x = train[,-ncol(train)]                 ## This remove 'id' column
train_x = as.matrix(train_x)                          ## This just change the format
train_x = matrix(as.numeric(train_x),nrow(train_x),ncol(train_x)) ## This just change the format (change to numeric)

train_y = train[,ncol(train)]
#train_y_fac = as.factor(train_y)                       #R's RF assums type of factor for classif
#train_y_num = as.integer(train_y)-1                       #xgboost take features in [0,numOfClass)

test_x = test[,-ncol(test)]                  ## This remove 'id' column
test_x = as.matrix(test_x)                          ## This just change the format
test_x = matrix(as.numeric(test_x),nrow(test_x),ncol(test_x)) ## This just change the format (change to numeric)

test_y = test[,ncol(test)]
#test_y_num = as.integer(test_y)-1 

mylogloss_list=0.5
myparam_list=0
run=0
run_max=50

while (run<run_max)
{
  ptm <- proc.time()
  run=run+1
  # Set necessary parameter
  param_xgb <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 2,
                   "eta" = runif(1,0.01,0.8),
                   "gamma" =1,
                   "max_depth" = sample(c(3:15),1),
                   "min_child_weight" = sample(c(2:5),1),
                   "nrounds" = 500,
                   "subsample" =0.8,
                   "colsample_bytree" =0.8,
                   "verbose"=0) # Number of trees to fit

  clf_xgb = xgboost(param=param_xgb, data = train_x, label = train_y, nrounds=param_xgb$nrounds, verbose=param_xgb$verbose)

## This is very specific
## xgboost prediction generates a 1D array, having a cetain arrangement
## This arrangement has to be rearranged by following
  pred_prob = predict(clf_xgb,test_x)
  pred_prob = matrix(pred_prob,2,length(pred_prob)/2)
  pred_prob = t(pred_prob)
  actual = matrix(0,nrow=nrow(pred_prob),ncol=ncol(pred_prob))
  for (x in 1:nrow(pred_prob)){
    actual[x,test_y[x]+1]=1  
  }
  
  for (y in 1:nrow(pred_prob)){
    for(x in 1:ncol(pred_prob)){
      if(pred_prob[y,x]<0.5){pred_prob[y,x]=0}
      else{pred_prob[y,x]=1}
    }
  }
  
  mylogloss <- sum(abs(actual[,1]-pred_prob[,1]))/nrow(pred_prob)
  ptm2 <- proc.time()-ptm
  message(mylogloss, " __ calculation costs ", ptm2[1])

  myparam_list=rbind(myparam_list,param_xgb)
  mylogloss_list= rbind(mylogloss_list,mylogloss)
}

param_xgb <- list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = 2,
                  "eta" = 0.1565526,
                  "gamma" =1,
                  "max_depth" = 4,
                  "min_child_weight" = 4,
                  "nrounds" = 500,
                  "subsample" =0.8,
                  "colsample_bytree" =0.8,
                  "verbose"=0) # Number of trees to fit

clf_xgb = xgboost(param=param_xgb, data = train_x, label = train_y, nrounds=param_xgb$nrounds, verbose=param_xgb$verbose)


pred_prob = predict(clf_xgb,test_x)
pred_prob = matrix(pred_prob,2,length(pred_prob)/2)
pred_prob = t(pred_prob)
actual = matrix(0,nrow=nrow(pred_prob),ncol=ncol(pred_prob))
for (x in 1:nrow(pred_prob)){
  actual[x,test_y[x]+1]=1  
}

for (y in 1:nrow(pred_prob)){
  for(x in 1:ncol(pred_prob)){
    if(pred_prob[y,x]<0.5){pred_prob[y,x]=0}
    else{pred_prob[y,x]=1}
  }
}

mylogloss <- sum(abs(actual[,1]-pred_prob[,1]))/nrow(pred_prob)
ptm2 <- proc.time()-ptm
message(mylogloss, " __ calculation costs ", ptm2[1])


#pred_prob = predict(clf_xgb,test_x)
#pred_prob = matrix(pred_prob,2,length(pred_prob)/2)
#pred_prob = t(pred_prob)
#yyy=cbind(pred_prob,test_y)
