library(caret)
library(mgcv)

library(ranger)
set.seed(105591)

trainSet<-readRDS("trainset70%-0527.rds")

# Initialize vectors to store predictions from each model
gam_predictions <- numeric(length(trainSet$occurrence))
brt_predictions <- numeric(length(trainSet$occurrence))
rf_predictions <- numeric(length(trainSet$occurrence))

# Splitting the data into 5 folds
folds <- createFolds(trainSet$occurrence, k = 5, list = TRUE, returnTrain = TRUE)

for (i in 1:length(folds)) {
  # Split the data into training and test sets
  train_indices <- folds[[i]]
  test_indices <- setdiff(1:nrow(trainSet), train_indices)
  train_data <- trainSet[train_indices, ]
  test_data <- trainSet[test_indices, ]
  #GAM
  gam_formula <- as.formula(paste("occurrence ~", paste("s(", predictors_names_gam, ", k =9)", collapse = " + ")))
  GAM_model <- gam(gam_formula, data = train_data, family = binomial())
  gam_predictions[test_indices] <- predict(GAM_model, newdata = test_data, type = "response")
  
   BRT Model
  BRT_fit <- gbm(
    formula = formula_str,
    data = train_data,
    distribution = "bernoulli",
    n.trees = 10000,
    shrinkage = 0.005,
    interaction.depth = 4,
    bag.fraction = 0.75,
    n.minobsinnode = 10
    )
  brt_predictions[test_indices] <- predict(BRT_fit, newdata = test_data, n.trees = 10000, type = "response")

  # RF Model
  train_data$occurrence <- as.factor(train_data$occurrence)
  rf_model <- ranger(occurrence ~ prec+tmax+tmin+rh+ws+sp+ndvi+evi+
                          Water_bodies+Evergreen_Needleleaf_Forests+Evergreen_Broadleaf_Forests+
                          Deciduous_Needleleaf_Forests+Deciduous_Broadleaf_Forests+Mixed_Forests+
                          Closed_Shrublands+Open_Shrublands+Woody_Savannas+
                          Savannas+Grasslands+Permanent_Wetlands+
                          Croplands+Urban_and_Builtup+Cropland_Natural_Vegetation_Mosaics+
                          Snow_and_Ice+Barren_or_Sparsely_Vegetated+elevation+pop+urban_acc_30s,
                        data=trainSet,num.trees=900,mtry=5, keep.inbag=TRUE,probability = TRUE)  
  rf_predictions[test_indices] <- predict(rf_model, test_data, type = "se")[1][[1]][,2] 
}
result_data <- trainSet
result_data$GAM <- gam_predictions
result_data$BRT_cv_pred <- brt_predictions
result_data$RF <- rf_predictions
