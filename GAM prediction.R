#load library
library(raster)
library(dplyr)
library(sf)
library(data.table)
#setwd()
#read scaled stack raster dataframe
predict_data<-readRDS("cor_data_2020.rds")

#load GAM
library(mgcv)
gam.fit0.0<-readRDS("child_gam.rds")

#try parallel
library(parallel)
n_cores <- 20
data_chunks <- split(predict_data, rep(1:n_cores, length.out = nrow(predict_data)))

# Define a function to make predictions on each chunk
predict_chunk <- function(chunk) {
  predictions <- predict(gam.fit0.0, newdata = chunk, type = "response", se.fit = TRUE)
  return(list(predictions = predictions$fit, se.fit = predictions$se.fit))
}

# Run predictions in parallel
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mgcv)
})
clusterExport(cl, varlist = c("gam.fit0.0", "predict_chunk"))

results_list <- parLapply(cl, data_chunks, predict_chunk)
GAM_predictions <- do.call(rbind, lapply(results_list, function(x) x$predictions))
GAM_se.fits <- do.call(rbind, lapply(results_list, function(x) x$se.fit))

stopCluster(cl)

# Optionally, convert the result to a vector if it's not already
GAM_predictions1 <- as.vector(GAM_predictions)
saveRDS(GAM_predictions1,"gam_glopred.rds")

GAM_se.fits <- as.vector(GAM_se.fits)
saveRDS(GAM_se.fits,"gam_glopred_se.rds")

lower_bounds <- predictions - 1.96 * se.fits
upper_bounds <- predictions + 1.96 * se.fits
GAM_lower <- as.vector(lower_bounds)
GAM_upper <- as.vector(upper_bounds)
saveRDS(GAM_lower,"gam_glopred_lower.rds")
saveRDS(GAM_upper,"gam_glopred_upper.rds")

# Assuming all_covariates_stack is original stack with the correct dimensions
template_raster <- raster("annual_prec_2020.tif")

# Convert GAM predictions to raster
GAM_raster <- template_raster
values(GAM_raster) <- GAM_predictions1
GAMse_raster <- template_raster
values(GAMse_raster) <- GAM_se.fits
GAMlow_raster <- template_raster
values(GAMlow_raster) <- GAM_lower
GAMup_raster <- template_raster
values(GAMup_raster) <- GAM_upper
# Save GAM prediction raster
writeRaster(GAM_raster, filename = "GAM_predictions_glo1.tif", format = "GTiff", overwrite = TRUE)
writeRaster(GAMse_raster, filename = "GAM_predictions_se.tif", format = "GTiff", overwrite = TRUE)
writeRaster(GAMlow_raster, filename = "GAM_predictions_low.tif", format = "GTiff", overwrite = TRUE)
writeRaster(GAMup_raster, filename = "GAM_predictions_up.tif", format = "GTiff", overwrite = TRUE)
