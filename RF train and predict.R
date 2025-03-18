#load RF
library(randomForest)
library(parallel)
library(doParallel)
library(raster)

set.seed(123) 
trainSet<-readRDS("trainset70%.rds")

n <- nrow(trainSet) 
n_bootstraps <- 100 
bootstrap_indices <- lapply(1:n_bootstraps, function(x) sample(1:n, replace = TRUE))

bootstrap_RF <- function(index) {
  bootstrap_sample <- trainSet[index, ]
  rf_model <- randomForest(occurrence ~ prec+tmax+tmin+rh+ws+sp+ndvi+evi+
                             Water_bodies+Evergreen_Needleleaf_Forests+Evergreen_Broadleaf_Forests+
                             Deciduous_Needleleaf_Forests+Deciduous_Broadleaf_Forests+Mixed_Forests+
                             Closed_Shrublands+Open_Shrublands+Woody_Savannas+
                             Savannas+Grasslands+Permanent_Wetlands+
                             Croplands+Urban_and_Builtup+Cropland_Natural_Vegetation_Mosaics+
                             Snow_and_Ice+Barren_or_Sparsely_Vegetated+elevation+pop+urban_acc_30s,
                           data=bootstrap_sample,
                           ntree=900,
                           mtry=5,
                           importance=TRUE)
  return(rf_model)
}

registerDoParallel(detectCores() - 1)
rf_models <- foreach(i = 1:n_bootstraps, .combine='c') %dopar% {
  bootstrap_RF(bootstrap_indices[[i]])
}
predict_RF <- function(model, chunk) {
  return(predict(model, newdata = chunk, type = "prob")[,2]) 
}


RFpredictions_list <- vector("list", length = n_bootstraps)


for (i in 1:n_bootstraps) {
  RFpredictions_list[[i]] <- lapply(data_chunks, predict_RF, model = rf_models[[i]])
}


RFpredictions_matrix <- do.call(cbind, lapply(RFpredictions_list, function(x) do.call(rbind, x)))

RFmean_predictions <- rowMeans(RFpredictions_matrix)
RFse_predictions <- apply(RFpredictions_matrix, 1, sd) / sqrt(n_bootstraps)
RFci_lower <- apply(RFpredictions_matrix, 1, quantile, probs = 0.025)
RFci_upper <- apply(RFpredictions_matrix, 1, quantile, probs = 0.975)

template_raster <- raster("annual_prec_2020.tif")
RF_predictions_raster <- template_raster
values(RF_predictions_raster) <- RFmean_predictions
RF_se_raster <- template_raster
values(RF_se_raster) <- RFse_predictions
RF_low_raster <- template_raster
values(RF_low_raster) <- RFci_lower
RF_up_raster <- template_raster
values(RF_up_raster) <- RFci_upper

writeRaster(RF_predictions_raster, "RF_predictions_glo.tif", format = "GTiff", overwrite = TRUE)
writeRaster(RF_se_raster, "RF_predictions_se.tif", format = "GTiff", overwrite = TRUE)
writeRaster(RF_low_raster, "RF_predictions_low.tif", format = "GTiff", overwrite = TRUE)
writeRaster(RF_up_raster, "RF_predictions_up.tif", format = "GTiff", overwrite = TRUE)
