# 30 % data used for validation
testSet<-readRDS("testset30%-0527.rds")

####################child RF####################
library(ranger)
rf.fit1<-ranger(occurrence ~ prec+tmax+tmin+rh+ws+sp+ndvi+evi+
                          Water_bodies+Evergreen_Needleleaf_Forests+Evergreen_Broadleaf_Forests+
                          Deciduous_Needleleaf_Forests+Deciduous_Broadleaf_Forests+Mixed_Forests+
                          Closed_Shrublands+Open_Shrublands+Woody_Savannas+
                          Savannas+Grasslands+Permanent_Wetlands+
                          Croplands+Urban_and_Builtup+Cropland_Natural_Vegetation_Mosaics+
                          Snow_and_Ice+Barren_or_Sparsely_Vegetated+elevation+pop+urban_acc_30s,
                        data=trainSet,num.trees=900,mtry=5, keep.inbag=TRUE,probability = TRUE)
saveRDS(rf.fit1,"child_rf-0527.rds")
rf.fit<-readRDS("child_rf-0527.rds")

RF.predicted_probs_test <- predict(rf.fit1, testSet, type="se")[1]
RF.predicted_probs_test1 <- as.numeric(RF.predicted_probs_test[[1]][,2])
actual_labels_test <- as.numeric(testSet$occurrence)
RF.roc_obj_test <- roc(actual_labels_test, RF.predicted_probs_test1)
auc(RF.roc_obj_test)
#save the predict for the test dataset
results.test$`RFtest`<-RF.predicted_probs_test

####################child BRT####################
library(gbm)
formula_str<- as.formula("occurrence ~ ndvi + evi + pop + prec + tmin + tmax +sp+rh+ws+ Water_bodies+Evergreen_Needleleaf_Forests+Evergreen_Broadleaf_Forests+
               Deciduous_Needleleaf_Forests+Deciduous_Broadleaf_Forests+Mixed_Forests+
               Closed_Shrublands+Open_Shrublands+Woody_Savannas+
               Savannas+Grasslands+Permanent_Wetlands+
               Croplands+Urban_and_Builtup+Cropland_Natural_Vegetation_Mosaics+
               Snow_and_Ice+Barren_or_Sparsely_Vegetated + elevation + urban_acc_30s")
BRT_model <- gbm(
    formula = formula_str,
    data = trainSet,
    distribution = "bernoulli",
    n.trees = 10000,
    shrinkage = 0.005,
    interaction.depth = 4,
    bag.fraction = 0.75,
    n.minobsinnode = 10
)
saveRDS(BRT_model,"child_BRT.rds")
BRT_model<-readRDS("child_BRT.rds")

####################child GAM###################
library(mgcv)
gam.fit0.0<-readRDS("child_gam.rds")
predictors_names<-c("Water_bodies", "Evergreen_Needleleaf_Forests", "Evergreen_Broadleaf_Forests",
               "Deciduous_Needleleaf_Forests","Deciduous_Broadleaf_Forests","Mixed_Forests",
               "Closed_Shrublands","Open_Shrublands","Woody_Savannas","Grasslands","Permanent_Wetlands",
               "Croplands","Cropland_Natural_Vegetation_Mosaics",
               "Snow_and_Ice","Barren_or_Sparsely_Vegetated","ndvi", "pop", "prec", 
                     "tmax","ws","rh", "sp", "urban_acc_30s")
predictors=trainSet[,predictors_names]
predictors.test=testSet[,predictors_names]

pred.test.gam<- predict(gam.fit0.0, testSet, type="response")
auc.test.gam <- roc(testSet$occurrence~pred.test.gam, data=predictors.test)
auc.test.gam#Area under the curve: #0.8889
plot.roc(auc.test.gam, print.auc=T, legacy.axes=T)
#save the predict for the test dataset
results.test <- data.frame(testSet, `GAMtest` =pred.test.gam)


#save prediction
write.csv(results.test, "predChild_test.csv", row.names = FALSE)
