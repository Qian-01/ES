#con-curvity of covariates in GAM
library(mgcv)
library(dplyr)
#update the predictors
predictors_names<- c("Water_bodies", "Evergreen_Needleleaf_Forests", "Evergreen_Broadleaf_Forests",
               "Deciduous_Needleleaf_Forests","Deciduous_Broadleaf_Forests","Mixed_Forests",
               "Closed_Shrublands","Open_Shrublands","Woody_Savannas",
               "Savannas","Grasslands","Permanent_Wetlands",
               "Croplands","Urban_and_Builtup","Cropland_Natural_Vegetation_Mosaics",
               "Snow_and_Ice","Barren_or_Sparsely_Vegetated","ndvi","evi", "pop", "prec", 
                     "tmin", "tmax", "sp","ws","rh", "elevation", "urban_acc_30s")
gam_formula0 <- as.formula(paste("occurrence ~", paste("s(", predictors_names, ", k = 10)", collapse = " + ")))

gam.fit0<- gam(gam_formula0,family= binomial(link = logit), data=trainSet)
summary(gam.fit0)
print(paste(" AIC :", gam.fit0$aic, "| deviance :", gam.fit0$deviance))

saveRDS(gam.fit0,"gam-full.rds")
conc0 <-concurvity(gam.fit0,full=FALSE)

###visualization of con-curvity of covariates
conc_matrix0 <- conc0$worst
conc_long0 <- as.data.frame(as.table(conc_matrix0))
names(conc_long0) <- c("Term1", "Term2", "Concurvity")
GAMcon0<-ggplot(conc_long0, aes(x=Term1, y=Term2, fill=Concurvity)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(fill="Concurvity", 
       title="Concurvity Matrix Visualization",
       x="", 
       y="") 
conc_long0[conc_long0$Concurvity > 0.8 & conc_long0$Term1 != conc_long0$Term2, ]


library(mgcv)
fit_model <- function(var_to_remove, formula, data) {
  reduced_vars <- setdiff(predictors_names, var_to_remove)
  reduced_formula <- as.formula(paste("occurrence ~", paste("s(", reduced_vars, ", k = 10)", collapse = " + ")))
  model <- gam(reduced_formula, family = binomial(link = logit), data = data)
  return(AIC(model))
}

aic_scores <- list()

#obtained from previous concurvity test
high_concurvity_pairs <- list(c("ndvi", "evi"), c("tmax", "tmin"), c("Barren_or_Sparsely_Vegetated", "Savannas"),c("pop","Urban_and_Builtup"),c("elevation","sp"))

for(pair in high_concurvity_pairs) {
  for(var in pair) {
    aic_scores[[paste("Remove", var)]] <- fit_model(var, original_formula, trainSet)
  }
}

aic_scores
