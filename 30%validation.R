# 30 % data used for validation
testSet<-readRDS("testset30%-0527.rds")

#child RF
#RF test data
library(ranger)
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

#child BRT

#child GAM

#save prediction
write.csv(results.test, "predChild_test.csv", row.names = FALSE)
