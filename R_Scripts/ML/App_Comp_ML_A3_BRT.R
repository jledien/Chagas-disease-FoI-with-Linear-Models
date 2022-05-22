#### MunML analyses with the predictors from Linear models
## ML_A3 RF using log
## all settings model 
#### Julia Ledien july 2021

model="ML_A3_BRT"

lrn_SE<- lrn("regr.gbm", distribution="gaussian", interaction.depth=2, n.trees=4556, n.minobsinnode=5, train.fraction=0.82, shrinkage=0.01  ) # learner: default parameters
measures =  msr("regr.rsq")# measure
resamplings = rsmp("repeated_sptcv_cstf", folds = 10, repeats = 2) # resampling: spatiotemporal

#Nested resampling:

#repeat 10 times
it=100
boot=10
years<- c(1960,1970,1980,1990,2000,2010)
#Definition of objects
R2<- rep(NA,it*boot)
resampled_R2<- rep(NA,it*boot)
R2_urb<- rep(NA,it*boot)
R2_rur<- rep(NA,it*boot)
R2_ind<- rep(NA,it*boot)
R2_CV<-rep(NA,it*boot)
pred_obsFoI<- matrix(NA, nrow(FoIobs_A1), it*boot)
pred_new_urb<- array(NA, dim=c(1065,length(years), it*boot))
pred_new_rur<- array(NA, dim=c(1065,length(years), it*boot))
pred_new_ind<- array(NA, dim=c(1065,length(years), it*boot))
CV_residuals_codes<-matrix(NA, nrow(FoIobs_A1)/2,  it*boot)
Moran_I<- matrix(NA, it*boot, 2)
DW_test_stat<- rep(NA, it*boot)

features<- colnames(FoIobs_A1[-c(1,2)])
var_imp<- matrix(NA, length(features), it*boot)

#FoIobs_A3$logFoI<- log(FoIobs_A3$FoI)

for (i in 1:it) {
  print(i)
  # CV : out of sample = 50% of FoIobs_A3
  Code_t <- sample(unique(FoIobs_A3$Code), round(length(unique(FoIobs_A3$Code))-(length(unique(FoIobs_A3$Code))*0.5)), replace=F) ## should be better to take out entire year and dataset and not only random values from posterior
  data_t00<- FoIobs_A3[which(FoIobs_A3$Code %in% Code_t),]
  
  for (b in 1:boot){
    Boot_it_set<- as.data.frame(stratified(data_t00, "Code", size=1, replace=FALSE,keep.rownames=TRUE))
    
    # task
    Task_ML_A3_0<- TaskRegrST$new(id =  "ML_A3",Boot_it_set[,-c(which(colnames(Boot_it_set)=="Code"),which(colnames(Boot_it_set)=="rn")  ,
                                                                which(colnames(Boot_it_set)=="row_code"))] , target = "FoI"
                                  , extra_args = list(coords_as_features = T, crs = NA, coordinate_names = c( "lat", "lon", "year")))
    
    
      # training
    filter1<- flt("importance", learner=lrn_SE)
    filter1$calculate(Task_ML_A3_0)
    var_1 <-as.data.table(filter1)
    Keep<-  data.frame(var_1[1:10,1])# Keep the 10 most important variable for the model
    
    
    data_t<- Boot_it_set[,colnames(Boot_it_set)%in% c(Keep$feature, "FoI", "lat", "lon", "year")]
    Task_ML_A3<- TaskRegrST$new(id =  "ML_A3",data_t, target = "FoI"
                                , extra_args = list(coords_as_features = T, crs = NA, coordinate_names = c( "lat", "lon", "year")))
    # variable importance
    lrn_SE$train(Task_ML_A3)
    filter2<- flt("importance", learner=lrn_SE)
    filter2$calculate(Task_ML_A3)
    var_2 <-as.data.table(filter2)
    for (n in 1:length(features)){
      var_imp[n,i]<- ifelse(features[n] %in% var_2$feature, var_2$score[which(var_2$feature==features[n])],0)
    }
    
    # prediction on observed data
    pred_obs_t<-lrn_SE$predict_newdata(FoIobs_A1)
    pred_obsFoI[,((i-1)*boot+b)]<-pred_obs_t$response 
    
    # performances
    rr1 = resample(Task_ML_A3, lrn_SE, resamplings, store_models = F)
    resampled_R2[((i-1)*boot+b)]<-rr1$aggregate(measures)
    
    dat0<- as.data.frame(cbind(FoIobs_A3$FoI, FoIobs_A3$IndiZone, FoIobs_A3$UrbZone,  FoIobs_A3$RurZone, FoIobs_A3$Code,  FoIobs_A3$row_code, FoIobs_A1$lon, FoIobs_A1$lat, FoIobs_A1$year))
    d_pred<-  as.data.frame(cbind(as.character(FoIobs_A1$Code), pred_obs_t$response ))
    colnames(dat0)<- c("obs", "ind", "urb", "rur", "code", "row_code", "lon", "lat", "year")
    colnames(d_pred)<- c( "code", "pred")
    dati<- merge(dat0, d_pred, all.x=T)
    
    dat_in<-subset(dati, dati$code %in% Code_t)
    dat_in$obs<- as.numeric(as.character(dat_in$obs))
    dat_in$pred<- as.numeric(as.character(dat_in$pred))
    R2[((i-1)*boot+b)]<-  1-(sum((dat_in$obs - dat_in$pred)^2)/ sum((dat_in$obs- mean(dat_in$obs))^2))
    
    dat<-subset(dati, !dati$code %in% Code_t)
    dat$obs<- as.numeric(as.character(dat$obs))
    dat$pred<- as.numeric(as.character(dat$pred))
    R2_CV[((i-1)*boot+b)]<-  1-(sum((dat$obs - dat$pred)^2)/ sum((dat$obs- mean(dat$obs))^2))
    R2_urb[((i-1)*boot+b)]<- 1-(sum((dat$obs[which(dat$urb==1)] - dat$pred[which(dat$urb==1)])^2)/ sum((dat$obs[which(dat$urb==1)]- mean(dat$obs))^2))
    R2_rur[((i-1)*boot+b)]<- 1-(sum((dat$obs[which(dat$rur==1)] - dat$pred[which(dat$rur==1)])^2)/ sum((dat$obs[which(dat$rur==1)]- mean(dat$obs))^2))
    R2_ind[((i-1)*boot+b)]<- 1-(sum((dat$obs[which(dat$ind==1)] - dat$pred[which(dat$ind==1)])^2)/ sum((dat$obs[which(dat$ind==1)]- mean(dat$obs))^2))
   
     #spatial correlation test
    dat$lat<- jitter(as.numeric(dat$lat), 1e-6)
    dat$lon<- jitter(as.numeric(dat$lon), 1e-6)
    dat$resi<- dat$obs- dat$pred
    
    dat_t<- dat[which(dat$year==2005),]
    sp_cvDat<- SpatialPointsDataFrame(dat_t[,7:8], dat_t)
    lstw  <- nb2listw(knn2nb(knearneigh(sp_cvDat, k = 10)))
    mtest<- moran.test(sp_cvDat$resi, lstw)
    Moran_I[((i-1)*boot+b),]<-c(mtest$estimate[1],mtest$p.value)
    CV_residuals_codes[,((i-1)*boot+b)]<-unique (dat$code)
    
    ##temporal correlation
      #Durbin-Watson (DW) test
    DW_test_stat[((i-1)*boot+b)] = sum((dat$resi - lag(dat$resi))^2, na.rm = TRUE) / sum(dat$resi^2, na.rm = TRUE)
   
    #predictions on new data
   for (y in 1:length(years)){
      #urban
      data_new_urb<-ADM2_Predictors_AllADM2(year=years[y],tSur= 2010, zone="urban")
      data_new_urb<-  data_new_urb[1:1065,]
      
      pred_new_urb_t<-lrn_SE$predict_newdata(data_new_urb)
      pred_new_urb[,y,((i-1)*boot+b)]<- pred_new_urb_t$response
      
      #rural
      data_new_rur<-ADM2_Predictors_AllADM2(year=years[y],tSur= 2010, zone="rural")
      data_new_rur<-  data_new_rur[1:1065,]
      
      pred_new_rur_t<-lrn_SE$predict_newdata(data_new_rur)
      pred_new_rur[,y,((i-1)*boot+b)]<- pred_new_rur_t$response
      
      #indigenous
      data_new_ind<-ADM2_Predictors_AllADM2(year=years[y],tSur= 2010, zone="indigenous")
      data_new_ind<-  data_new_ind[1:1065,]
      
      pred_new_ind_t<-lrn_SE$predict_newdata(data_new_ind)
      pred_new_ind[,y,((i-1)*boot+b)]<- pred_new_ind_t$response
    }
  }
}


#save files
saveRDS(R2, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_",it, "it_",boot,"boots"))
saveRDS(R2_CV, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_CV_",it,  "it_",boot,"boots"))
saveRDS(resampled_R2, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_resampled_R2_",it,  "it_",boot,"boots"))
saveRDS(R2_urb, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_urb_",it, "it_",boot,"boots"))
saveRDS(R2_rur, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_rur_",it,  "it_",boot,"boots"))
saveRDS(R2_ind, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_R2_ind_",it, "it_",boot,"boots"))
saveRDS(pred_obsFoI, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_obsFoI_",it,  "it_",boot,"boots"))
saveRDS(pred_new_urb, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_new_urb_",it, "it_",boot,"boots"))
saveRDS(pred_new_rur, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_new_rur_",it,  "it_",boot,"boots"))
saveRDS(pred_new_ind, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_pred_new_ind_",it, "it_",boot,"boots"))
saveRDS(var_imp, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_var_imp_",it,  "it_",boot,"boots"))
saveRDS(CV_residuals_codes, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_CV_residuals_codes_",it, "it",boot,"boots"))
saveRDS(Moran_I, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_Moran_I_",it, "it",boot,"boots"))
saveRDS(DW_test_stat, paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_DW_test_stat_",it, "it",boot,"boots"))

sum_var_Imp<-rowSums (var_imp, na.rm = FALSE, dims = 1)
sum_var_Imp<- as.data.frame(cbind(features,sum_var_Imp))
sum_var_Imp[,2]<- as.numeric(sum_var_Imp[,2])
write.csv(sum_var_Imp,file=paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_factorWeight_table_",it,  "it_",boot,"boots.csv"))

#overlap
Ov_all <- rep(NA,length(unique(FoIobs_A3$Code)))

for (j in 1:length(unique(FoIobs_A3$Code))){
  z <- pred_obsFoI[j,]
  o <- as.vector(FoIobs_A3$FoI[which(FoIobs_A3$Code==FoIobs_A1$Code[j])])
  Ov_all[j] <- overlap(x = list(o,z),nbins = 100,plot = FALSE)$OV
  
  
}

Ov_urb<- Ov_all[which(FoIobs_A1$UrbZone==1)]
Ov_rur<- Ov_all[which(FoIobs_A1$RurZone==1)]
Ov_ind<- Ov_all[which(FoIobs_A1$IndiZone==1)]

Ind_all<- (mean(R2_CV)+mean(Ov_all))/2
Ind_urb<- (mean(R2_urb)+mean(Ov_urb))/2
Ind_rur<- (mean(R2_rur)+mean(Ov_rur))/2
Ind_ind<- (mean(R2_ind)+mean(Ov_ind))/2

performance_table <- rbind (summary(R2),
                            summary(resampled_R2),
                            summary(R2_CV),
                            summary(R2_urb),
                            summary(R2_rur),
                            summary(R2_ind),
                            summary(Ov_all),
                            summary(Ov_urb),
                            summary(Ov_rur),
                            summary(Ov_ind),
                            summary(Ind_all),
                            summary(Ind_urb),
                            summary(Ind_rur),
                            summary(Ind_ind),
                            summary(Moran_I[,1]),
                            summary(Moran_I[,2]),
                            summary(DW_test_stat)
                            )

rownames(performance_table)<- c("R2","as.character","R2_CV","R2_urb","R2_rur","R2_ind", "Ov","Ov_urb","Ov_rur","Ov_ind", "Ind","Ind_urb","Ind_rur","Ind_ind" , "Moran_I_stat", "Moran_I_p_value" , "DW_test_stat")
write.csv(performance_table,file=paste0("Comparison_with_LinearModels/res/Approach_comparison/",model, "_performance_table_",it, "it_",boot,"boots.csv"))

## make maps shapefiles

MakingMaps(Pred=pred_new_urb, model = model, setting = "urban", years = years, PathToSave = "Comparison_with_LinearModels/res/Approach_comparison/Shapefiles")
MakingMaps(Pred=pred_new_rur, model = model, setting = "rural", years = years, PathToSave = "Comparison_with_LinearModels/res/Approach_comparison/Shapefiles")
MakingMaps(Pred=pred_new_ind, model = model, setting = "indigenous", years = years, PathToSave = "Comparison_with_LinearModels/res/Approach_comparison/Shapefiles")
