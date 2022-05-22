#### MunML analyses with the predictors from Linear models
## ML_A1
## all settings model 
#### Julia Ledien july 2021

model="ML_A1_RF"

lrn_SE<- lrn("regr.randomForest", importance ="mse", "ntree"=25, "nodesize"=5)
measures =  msr("regr.rsq")# measure
resamplings = rsmp("repeated_sptcv_cstf", folds = 10, repeats = 2) # resampling: spatiotemporal

#Nested resampling:

#repeat 10 times
it=100
years<- c(1960,1970,1980,1990,2000,2010)
#Definition of objects
R2<- rep(NA,it)
resampled_R2<- rep(NA,it)
R2_urb<- rep(NA,it)
R2_rur<- rep(NA,it)
R2_ind<- rep(NA,it)
R2_CV<-rep(NA,it)
pred_obsFoI<- matrix(NA, nrow(FoIobs_A1), it)
pred_new_urb<- array(NA, dim=c(1065,length(years), it))
pred_new_rur<- array(NA, dim=c(1065,length(years), it))
pred_new_ind<- array(NA, dim=c(1065,length(years), it))
CV_residuals_codes<-matrix(NA, nrow(FoIobs_A1)/2, it)
Moran_I<- matrix(NA,it, 2)
DW_test_stat<- rep(NA,it)
z_score<-matrix(NA,nrow(FoIobs_A1),it)

features<- colnames(FoIobs_A1[-c(1,2)])
var_imp<- matrix(NA, length(features), it)
#FoIobs_A1$logMedFoI<- log(FoIobs_A1$MedFoI)

for (i in 1:it) {
  
  # CV : out of sample = 25% of FoIobs_A1
  Code_t <- sample(FoIobs_A1$Code, round(nrow(FoIobs_A1)-(nrow(FoIobs_A1)*0.5)), replace=F)
  data_t00<- FoIobs_A1[which(FoIobs_A1$Code %in% Code_t),]
  data_t000<- data_t00[sample(1:nrow(data_t00)), ]
  data_t0<- data_t000#[order(data_t000$IndiZone),]
  
  # task: Spatiotemporal task, leave time out: temporal task
  Task_ML_A1_0<- TaskRegrST$new(id =  "ML_A1", data_t0[,-c(which(colnames(FoIobs_A1)=="Code"))], target = "MedFoI"
                                , extra_args = list(coords_as_features = T, crs = NA, coordinate_names = c( "lat", "lon", "year")))
  
  
  # training
  lrn_SE$train(Task_ML_A1_0)
  var_1 <-lrn_SE$importance()
  Keep<-  names(var_1[1:10])# Keep the 10 most important variable for the model
  
  
  data_t<- data_t0[,colnames(data_t0)%in% c(Keep, "MedFoI", "lat", "lon", "year")]
  Task_ML_A1<- TaskRegrST$new(id =  "ML_A1",data_t, target = "MedFoI"
                              , extra_args = list(coords_as_features = T, crs = NA, coordinate_names = c( "lat", "lon", "year")))
  
  # variable importance
  lrn_SE$train(Task_ML_A1)
  var_2 <-lrn_SE$importance()
  for (n in 1:length(features)){
    var_imp[n,i]<- ifelse(features[n] %in% names(var_2), var_2[which(names(var_2)==features[n])],0)
  }
  
  # prediction on observed data
  pred_obs_t<-lrn_SE$predict_newdata(FoIobs_A1)
  pred_obsFoI[,i]<-pred_obs_t$response
  
  # performances
  rr1 = resample(Task_ML_A1, lrn_SE, resamplings, store_models = F)
  resampled_R2[i]<-rr1$aggregate(measures)
  
  dat0<- as.data.frame(cbind(FoIobs_A1$MedFoI, pred_obs_t$response, FoIobs_A1$IndiZone, FoIobs_A1$UrbZone,  FoIobs_A1$RurZone, FoIobs_A1$Code, FoIobs_A1$lon, FoIobs_A1$lat, FoIobs_A1$year))
  colnames(dat0)<- c("obs", "pred", "ind", "urb", "rur", "code", "lon", "lat", "year")

  dat_in<-subset(dat0, dat0$code %in% Code_t)
  dat_in$obs<- as.numeric((dat_in$obs))
  dat_in$pred<- as.numeric(dat_in$pred)
  R2[i]<-  1-(sum((dat_in$obs - dat_in$pred)^2)/ sum((dat_in$obs- mean(dat_in$obs))^2))
  
  dat<-subset(dat0, !dat0$code %in% Code_t)
  dat$obs<- as.numeric((dat$obs))
  dat$pred<- as.numeric(dat$pred)
  R2_CV[i]<-  1-(sum((dat$obs - dat$pred)^2)/ sum((dat$obs- mean(dat$obs))^2))
  R2_urb[i]<- 1-(sum((dat$obs[which(dat$urb==1)] - dat$pred[which(dat$urb==1)])^2)/ sum((dat$obs[which(dat$urb==1)]- mean(dat$obs))^2))
  R2_rur[i]<- 1-(sum((dat$obs[which(dat$rur==1)] - dat$pred[which(dat$rur==1)])^2)/ sum((dat$obs[which(dat$rur==1)]- mean(dat$obs))^2))
  R2_ind[i]<- 1-(sum((dat$obs[which(dat$ind==1)] - dat$pred[which(dat$ind==1)])^2)/ sum((dat$obs[which(dat$ind==1)]- mean(dat$obs))^2))
  
  #spatial correlation test
  dat$lat<- jitter(as.numeric(dat$lat), 1e-6)
  dat$lon<- jitter(as.numeric(dat$lon), 1e-6)
  dat$resi<- dat$obs- dat$pred
 
  dat_t<- dat[which(dat$year==2005),]
  sp_cvDat<- SpatialPointsDataFrame(dat_t[,7:8], dat_t)
  lstw  <- nb2listw(knn2nb(knearneigh(sp_cvDat, k = 10)))
  mtest<- moran.test(sp_cvDat$resi, lstw)
  Moran_I[i,]<-c(mtest$estimate[1],mtest$p.value)
  CV_residuals_codes[,i]<-dat$code
  
  ##temporal correlation
  #Durbin-Watson (DW) test
  DW_test_stat[i] = sum((dat$resi - lag(dat$resi))^2, na.rm = TRUE) / sum(dat$resi^2, na.rm = TRUE)
  #Interpreting the Durban Watson Statistic
    #The Durban Watson statistic will always assume a value between 0 and 4.
    #A value of DW = 2 indicates that there is no autocorrelation. 
    #When the value is below 2, it indicates a positive autocorrelation, and a value higher than 2 indicates a negative serial correlation.
  
  ##z_score
  dat0$obs<- as.numeric((dat0$obs))
  dat0$pred<- as.numeric(dat0$pred)
  z_score[,i]<- (dat0$pred-mean(dat0$obs))/sd(dat0$obs)
  
  
  #predictions on new data
  for (y in 1:length(years)){
    #urban
    data_new_urb<-ADM2_Predictors_AllADM2(year=years[y],tSur= 2010, zone="urban")
    data_new_urb<-  data_new_urb[1:1065,]
    
    pred_new_urb_t<-lrn_SE$predict_newdata(data_new_urb)
    pred_new_urb[,y,i]<- pred_new_urb_t$response
    
    #rural
    data_new_rur<-ADM2_Predictors_AllADM2(year=years[y],tSur= 2010, zone="rural")
    data_new_rur<-  data_new_rur[1:1065,]
    
    pred_new_rur_t<-lrn_SE$predict_newdata(data_new_rur)
    pred_new_rur[,y,i]<- pred_new_rur_t$response
    
    #indigenous
    data_new_ind<-ADM2_Predictors_AllADM2(year=years[y],tSur= 2010, zone="indigenous")
    data_new_ind<-  data_new_ind[1:1065,]
    
    pred_new_ind_t<-lrn_SE$predict_newdata(data_new_ind)
    pred_new_ind[,y,i]<- pred_new_ind_t$response
  }
}



#save files
saveRDS(R2, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_",it, "it"))
saveRDS(R2_CV, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_CV_",it, "it"))
saveRDS(resampled_R2, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_resampled_R2_",it, "it"))
saveRDS(R2_urb, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_urb_",it, "it"))
saveRDS(R2_rur, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_rur_",it, "it"))
saveRDS(R2_ind, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_ind_",it, "it"))
saveRDS(pred_obsFoI, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_obsFoI_",it, "it"))
saveRDS(pred_new_urb, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_new_urb_",it, "it"))
saveRDS(pred_new_rur, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_new_rur_",it, "it"))
saveRDS(pred_new_ind, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_new_ind_",it, "it"))
saveRDS(var_imp, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_var_imp_",it, "it"))
saveRDS(CV_residuals_codes, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_CV_residuals_codes_",it, "it"))
saveRDS(Moran_I, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_Moran_I_",it, "it"))
saveRDS(DW_test_stat, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_DW_test_stat_",it, "it"))
saveRDS(z_score, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_z_score_",it, "it"))


performance_table <- rbind (summary(R2),
                            summary(resampled_R2),
                            summary(R2_CV),
                            summary(R2_urb),
                            summary(R2_rur),
                            summary(R2_ind),
                            summary(Moran_I[,1]),
                            summary(Moran_I[,2]),
                            summary(DW_test_stat),
                            summary(as.numeric(z_score)))
rownames(performance_table)<- c("R2","resampled_R2","R2_CV","R2_urb","R2_rur","R2_ind", "Moran_I_stat", "Moran_I_p_value" , "DW_test_stat", "z_score")
write.csv(performance_table,file=paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_performance_table_",it, "it.csv"))

sum_var_Imp<-rowSums (var_imp, na.rm = FALSE, dims = 1)
sum_var_Imp<- as.data.frame(cbind(features,sum_var_Imp))
sum_var_Imp[,2]<- as.numeric(sum_var_Imp[,2])
write.csv(sum_var_Imp,file=paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_factorWeight_table_",it, "it.csv"))

## make maps shapefiles
source("Scripts/Comparison_ML_LM/FUN_makingMaps.R")

MakingMaps(Pred=pred_new_urb, model = model, setting = "urban", years = years, PathToSave = "Comparison_with_LinearModels/res/Approach_comparison/Shapefiles")
MakingMaps(Pred=pred_new_rur, model = model, setting = "rural", years = years, PathToSave = "Comparison_with_LinearModels/res/Approach_comparison/Shapefiles")
MakingMaps(Pred=pred_new_ind, model = model, setting = "indigenous", years = years, PathToSave = "Comparison_with_LinearModels/res/Approach_comparison/Shapefiles")

##overlap
Ov_all <- rep(NA,length(unique(FoIobs_A3$Code)))
pred_obsFoI<- readRDS(paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_obsFoI_",it, "it"))


for (j in 1:length(unique(FoIobs_A3$Code))){
  z <- pred_obsFoI[j,]
  o <- as.vector(FoIobs_A3$FoI[which(FoIobs_A3$Code==FoIobs_A1$Code[j])])
  Ov_all[j] <- overlap(x = list(o,z),nbins = 100,plot = FALSE)$OV
  
  
}

Ov_urb<- Ov_all[which(FoIobs_A1$UrbZone==1)]
Ov_rur<- Ov_all[which(FoIobs_A1$RurZone==1)]
Ov_ind<- Ov_all[which(FoIobs_A1$IndiZone==1)]
mean(Ov_all)
mean(Ov_urb)
mean(Ov_rur)
mean(Ov_ind)