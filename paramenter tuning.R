#Cross Validation and parameter Tuning
library(gbm)
set.seed(123)
formula_str <- as.formula("occurrence ~ ndvi + evi + pop + prec + tmin + tmax + Water_bodies+Evergreen_Needleleaf_Forests+Evergreen_Broadleaf_Forests+
               Deciduous_Needleleaf_Forests+Deciduous_Broadleaf_Forests+Mixed_Forests+
               Closed_Shrublands+Open_Shrublands+Woody_Savannas+
               Savannas+Grasslands+Permanent_Wetlands+
               Croplands+Urban_and_Builtup+Cropland_Natural_Vegetation_Mosaics+
               Snow_and_Ice+Barren_or_Sparsely_Vegetated + imr + elevation + urban_acc + mite + Income_Index")
trainset<-readRDS("trainset70%.rds")
library(caret)
train_control <- trainControl(method = "cv", number = 10)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = seq(.01,.05,.01),
                        n.minobsinnode = c(10,20))
gbmFit <- train(Class ~ ., data = trainset, 
                 method = "gbm", 
                 trControl = train_control, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)
param_grid <- expand.grid(n.trees = c(50, 100, 150),
                          interaction.depth = c(1, 5, 10),
                          shrinkage = c(0.01, 0.1),
                          n.minobsinnode = 10)
model <- train(formula = formula_str,
               data = trainset,
               method = "gbm",
               trControl = train_control,
               tuneGrid = param_grid,
               #metric = "Accuracy",  # or AUC
               verbose = FALSE)
print(model$bestTune)
