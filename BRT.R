library(gbm)

##############Bootstrap to get 100 BRTmodel
trainSet<-readRDS("trainset70%.rds")
set.seed(123) 

n <- nrow(trainSet)
n_bootstraps <- 100 
bootstrap_indices <- lapply(1:n_bootstraps, function(x) sample(1:n, replace = TRUE))


bootstrap_BRT_parallel <- function(index) {
  bootstrap_sample <- trainSet[index, ]
  
  brt_model <- gbm(
    formula = occurrence ~ prec+tmax+tmin+rh+ws+sp+ndvi+evi+
                             Water_bodies+Evergreen_Needleleaf_Forests+Evergreen_Broadleaf_Forests+
                             Deciduous_Needleleaf_Forests+Deciduous_Broadleaf_Forests+Mixed_Forests+
                             Closed_Shrublands+Open_Shrublands+Woody_Savannas+
                             Savannas+Grasslands+Permanent_Wetlands+
                             Croplands+Urban_and_Builtup+Cropland_Natural_Vegetation_Mosaics+
                             Snow_and_Ice+Barren_or_Sparsely_Vegetated+elevation+pop+urban_acc_30s,
    data = bootstrap_sample,
    distribution = "bernoulli",
    n.trees = 10000,
    shrinkage = 0.005,
    interaction.depth = 4,
    bag.fraction = 0.75,
    n.minobsinnode = 10
  )
  
  return(brt_model)
}

cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c("trainSet", "bootstrap_BRT_parallel", "formula_str", "gbm", "bootstrap_indices"))
clusterEvalQ(cl, library(gbm))

brt_models <- parLapply(cl, bootstrap_indices, bootstrap_BRT_parallel)

stopCluster(cl)

predict_chunk_bootstrap <- function(model, chunk) {
  predict(model, newdata = chunk, n.trees=10000, type = "response")
}

cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c("brt_models", "predict_chunk_bootstrap", "data_chunks"))

BRTpredictions_list <- lapply(brt_models, function(model) {
  do.call(rbind, parLapply(cl, data_chunks, function(chunk) predict_chunk_bootstrap(model, chunk)))
})

stopCluster(cl)

BRTall_predictions <- do.call(cbind, BRTpredictions_list)
BRTmean_predictions <- rowMeans(BRTall_predictions)
BRTse_predictions <- apply(BRTall_predictions, 1, sd) / sqrt(n_bootstraps)
BRTci_lower <- apply(BRTall_predictions, 1, quantile, probs = 0.025)
BRTci_upper <- apply(BRTall_predictions, 1, quantile, probs = 0.975)

template_raster <- raster("annual_prec_2020.tif")
BRT_predictions_raster <- template_raster
values(BRT_predictions_raster) <- BRTmean_predictions
BRT_se_raster <- template_raster
values(BRT_se_raster) <- BRTse_predictions
BRT_low_raster <- template_raster
values(BRT_low_raster) <- BRTci_lower
BRT_up_raster <- template_raster
values(BRT_up_raster) <- BRTci_upper

# save
writeRaster(BRT_predictions_raster, filename = "BRT_predictions_glo.tif", format = "GTiff", overwrite = TRUE)
writeRaster(BRT_se_raster, filename = "BRT_predictions_se.tif", format = "GTiff", overwrite = TRUE)
writeRaster(BRT_low_raster, filename = "BRT_predictions_low.tif", format = "GTiff", overwrite = TRUE)
writeRaster(BRT_up_raster, filename = "BRT_predictions_up.tif", format = "GTiff", overwrite = TRUE)
