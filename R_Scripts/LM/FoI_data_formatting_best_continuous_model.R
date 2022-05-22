setwd("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/Data_preparation/")
info<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021_revised.RDS")
Foi_001<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/best/COL-035-05.RDS")

## the file only contained the 3 best models and we want the best continuous model
## will add a description of the model used in the supp of the paper

library(readxl)
library(reshape2)

datasetID <- unique(info$survey)
BigData<- data.frame(year=NA, FoI=NA, dataset_id=NA)
Model_selected<- matrix(NA, length(datasetID),2)
rownames(Model_selected)<- datasetID

for (d in datasetID){
  file<- readRDS(paste0("Raw_data/Raw_FoI_1000it/FOI-COL-2021/best/",d,".RDS"))
  file$foi_post_1000s$selectScore<-ifelse(file$foi_post_1000s$name_model=="Constant" |file$foi_post_1000s$name_model=="2Dec-T",0,10 )
  file$foi_post_1000s$selectScore<-ifelse(file$foi_post_1000s$best=="best1", 
                                          file$foi_post_1000s$selectScore+2, 
                                          ifelse(file$foi_post_1000s$best=="best2",
                                                 file$foi_post_1000s$selectScore+1,
                                                 file$foi_post_1000s$selectScore+0 ))
  
  bestFoI<- file$foi_post_1000s[which(file$foi_post_1000s$selectScore==max(file$foi_post_1000s$selectScore)),
                                -c(which(colnames(file$foi_post_1000s)=="best"),
                                   which(colnames(file$foi_post_1000s)=="name_model"),
                                   which(colnames(file$foi_post_1000s)=="selectScore"))]
  data<- melt(bestFoI)
  colnames(data) <- c("year", "FoI")
  data$dataset_id <- d
  BigData<-rbind(BigData,data)
  Model_selected[d,]<- c(unique(file$foi_post_1000s$best[which(file$foi_post_1000s$selectScore==max(file$foi_post_1000s$selectScore))]),
                         unique(file$foi_post_1000s$name_model[which(file$foi_post_1000s$selectScore==max(file$foi_post_1000s$selectScore))]))
}

saveRDS(BigData[2:nrow(BigData),],"Raw_data/Raw_FoI_1000it/COL_FoI_merged_1000it")
saveRDS(Model_selected, "Rdata/Model_selected")

####Adding covariates
FoI<- BigData[2:nrow(BigData),]
COL_predictors <- readRDS("Raw_data/predictors_ADM2")

FoI_predictors<- merge(FoI, info, by.x="dataset_id", by.y="survey", all.x=T)
FoI_predictors<- merge(FoI_predictors, COL_predictors, by.x="GID_2", by.y="GID_2_GDAM", all.x=T)

saveRDS(FoI_predictors, "Rdata/COL_BestModel_FullPost_FoI_1000it.RDS")

FoI_predictors_ADM2<- subset(FoI_predictors, is.na(FoI_predictors$GID_2)==F)
saveRDS(FoI_predictors_ADM2, "Rdata/COL_Best_Continuous_Model_FullPost_FoI_1000it_ADM2only.RDS")
