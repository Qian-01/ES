##Stacked ensemble

library(data.table)
library(matrixStats)
library(quadprog)
#load prediction

#prediction from each child models of 5 fold
mydata <- read.csv("childprediction.csv", stringsAsFactors =  F)
mydata <- data.table(mydata)

#prediction for test
covs <- read.csv('predChild_test.csv', stringsAsFactors = F)
covs <- data.table(covs)

stackers <- data.table(mydata)
stackers <- data.frame(stackers)

# Rename specific columns
colnames(stackers)[colnames(stackers) == "GAM"] <- "GAM_cv_pred"
colnames(stackers)[colnames(stackers) == "BRT_cv_pred"] <- "BRT_cv_pred"
colnames(stackers)[colnames(stackers) == "RF"] <- "RF_cv_pred"

X <- as.matrix(stackers[colnames(stackers)[(grep('cv_pred', colnames(stackers)))]])
new_order <- paste(child_models, "_cv_pred", sep="")
new_order <- new_order[new_order %in% colnames(X)]
X <- X[, new_order]

family <- 'binomial' 
Y = if(family == 'binomial'){stackers$occurrence}else if(family == 'gaussian'){stackers$val}

#stack the predictions for all locations
C <- data.frame(covs)
child_models<-c("GAM", "BRT", "RF")
colnames(C)[colnames(C) == "GAMtest"] <- "GAM"
colnames(C)[colnames(C) == "BRTtest"] <- "BRT"
colnames(C)[colnames(C) == "RFtest"] <- "RF"
C <- as.matrix(C[c(child_models)])

#Select which stacker you are using and stack the estimates
stacker <- 'CWM'

if(stacker == 'CWM'){
  # The following code is from http://zoonek.free.fr/blosxom/R/2012-06-01_Optimization.html
  # Calculate coefficients (child stacker weights)
  # Coefficients must sum to 1 and >=0
  
  s <- solve.QP( 
    t(X) %*% X,t(Y) %*% X, 
    cbind(  # One constraint per COLUMN
      matrix(1, nr=length(child_models), nc=1),
      diag(length(child_models)),
      -diag(length(child_models))
    ),
    c(1, 
      rep(0.000001, length(child_models)),
      rep(-1, length(child_models))), 
    meq = 1 # Only the first constraint is an equality if meq = 1 so set to 0, the others are >=
  )
  
  #calculate weighted stackers
  #for the fits with data
  mydata$stacked_preds_CWM <- rowWeightedMeans(X, w = s$solution)   
  # or can use: mydata$crossprod <- crossprod(t(X), s$solution)
  
  #Calculate the stacked predictions
  covs$cv_custom_stage_1 <- rowWeightedMeans(C, w = s$solution)
}
s$solution


library(pROC)
#AUC on train data CWM
auc.train.CWM <- roc(mydata$occurrence~stacked_preds_CWM, data=mydata)
auc.train.CWM#0.9856
plot.roc(auc.train.CWM, print.auc=T, legacy.axes=T)

#AUC on test data CWM
auc.test.CWM <- roc(covs$occurrence~cv_custom_stage_1, data=covs)
auc.test.CWM#0.9718
plot.roc(auc.test.CWM, print.auc=T, legacy.axes=T)

stacker <- 'RWM'
if(stacker == 'RWM'){
  r2 <- rep(NA, length(child_models))
  for(i in 1:length(child_models)){
    column_name <- paste0(child_models[i], '_cv_pred')
    if(family == 'binomial'){
      r2[i] <- round(cor(stackers$occurrence, stackers[[column_name]])^2, 2)
    }
    if(family == 'gaussian'){
      # Replace 'stackers$val' with the appropriate column for the gaussian case
      r2[i] <- round(cor(stackers$val, stackers[[column_name]])^2, 2)
    }
  }
  
  total <-  sum(r2)
  weights <- r2/total
  stackers$stacked_preds_RWM <- rowWeightedMeans(X, w = weights)   
  covs$cv_custom_stage_2 <- rowWeightedMeans(C, w = weights)
}

weights

library(pROC)
#AUC on train data RWM
auc.train.RWM <- roc(stackers$occurrence~stacked_preds_RWM, data=stackers)
auc.train.RWM
plot.roc(auc.train.RWM, print.auc=T, legacy.axes=T)

#AUC on test data RWM
auc.test.RWM <- roc(covs$occurrence~cv_custom_stage_2, data=covs)
auc.test.RWM
plot.roc(auc.test.RWM, print.auc=T, legacy.axes=T)



