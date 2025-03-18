#stack ensemble weight mean by R-sqr
weights <- c(GAM = 0.2837838 , BRT = 0.3738739, RF = 0.3738739)#number obtained from stack ensemble method.R

#RWM se
weighted_GAMse <- GAM_se.fits * weights["GAM"]
weighted_BRTse <- BRTse_predictions * weights["BRT"]
weighted_RFse <- RFse_predictions * weights["RF"]
RWMse_predictions <- weighted_GAMse + weighted_BRTse + weighted_RFse
saveRDS(RWMse_predictions,"stackRWMse_glopred.rds")
RWMse_raster <- template_raster
values(RWMse_raster) <- RWMse_predictions
# Save RF prediction raster
writeRaster(RWMse_raster, filename = "RWMse_predictions_glo.tif", format = "GTiff", overwrite = TRUE)

#stack ensemble weight mean by CWM
weights2 <- c(GAM = 0.000001 , BRT = 0.000001 , RF = 0.999998)#number obtained from stack ensemble method.R

# CWM se
weighted_GAM2se <- GAM_se.fits * weights2["GAM"]
weighted_BRT2se <- BRTse_predictions * weights2["BRT"]
weighted_RF2se <- RFse_predictions * weights2["RF"]
CWMse_predictions <- weighted_GAM2se + weighted_BRT2se + weighted_RF2se
saveRDS(CWMse_predictions,"stackCWMse_glopred.rds")
CWMse_raster <- template_raster
values(CWMse_raster) <- CWMse_predictions
writeRaster(CWMse_raster, filename = "stackCWMse_glopred.tif", format = "GTiff", overwrite = TRUE)


# CWM Apply weights
weighted_GAM2 <- GAM_predictions1 * weights2["GAM"]
weighted_BRT2 <- BRT_predictions1 * weights2["BRT"]
weighted_RF2 <- RF_predictions1 * weights2["RF"]
stacked_predictions2 <- weighted_GAM2 + weighted_BRT2 + weighted_RF2
saveRDS(stacked_predictions2,"stackCWM_glopred.rds")
CWM_raster <- template_raster
values(CWM_raster) <- stacked_predictions2
# Save RF prediction raster
writeRaster(CWM_raster, filename = "CWM_predictions_glo.tif", format = "GTiff", overwrite = TRUE)


# RWM Apply weights
weighted_GAM <- GAM_predictions1 * weights["GAM"]
weighted_BRT <- BRT_predictions1 * weights["BRT"]
weighted_RF <- RF_predictions1 * weights["RF"]
stacked_predictions <- weighted_GAM + weighted_BRT + weighted_RF
saveRDS(stacked_predictions,"stackRWM_glopred.rds")
stack_raster <- template_raster
values(stack_raster) <- stacked_predictions
# Save RF prediction raster
writeRaster(stack_raster, filename = "RWM_predictions_glo.tif", format = "GTiff", overwrite = TRUE)
