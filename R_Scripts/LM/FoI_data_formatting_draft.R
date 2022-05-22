setwd("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/")
info<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021_revised.RDS")
#Foi_prev<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/data-COL-2021.RDS")
Foi_001<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/best/COL-035-05.RDS")
## the file only contained the 3 best models and we can't be sure that the continuous model (taht has been divided on 5 new models) will be selected
## decided thus to select the best model for each survey
## will add a description of the model used in the supp of the paper

OriginalDataEx<- readRDS("C://Users/ledie/Dropbox/Chagas modelling/backups/chagas-ML/analysis/FoI Linear Regression/Continuous FoI/Scripts/Pierre/approach1_2_3/Rdata/ContFoI_Median_ADM2Covariates_2020_ADM2only_Final.RDS")
library(readxl)
library(reshape2)

datasetID <- unique(info$survey)
BigData<- data.frame(year=NA, FoI=NA, dataset_id=NA)

for (d in datasetID){
  file<- readRDS(paste0("Raw_data/Raw_FoI_1000it/FOI-COL-2021/best/",d,".RDS"))
  bestFoI<- file$foi_post_1000s[which(file$foi_post_1000s$best=="best1"),-c(which(colnames(file$foi_post_1000s)=="best"),which(colnames(file$foi_post_1000s)=="name_model"))]
  data<- melt(bestFoI)
  colnames(data) <- c("year", "FoI")
  data$dataset_id <- d
  BigData<-rbind(BigData,data)
}

saveRDS(BigData[2:nrow(BigData),],"Raw_data/Raw_FoI_1000it/COL_FoI_merged_1000it")

####Adding covariates
FoI<- BigData[2:nrow(BigData),]
COL_predictors <- readRDS("Raw_data/predictors_ADM2")

FoI_predictors<- merge(FoI, info, by.x="dataset_id", by.y="survey", all.x=T)
FoI_predictors<- merge(FoI_predictors, COL_predictors, by.x="GID_2", by.y="GID_2_GDAM", all.x=T)

saveRDS(FoI_predictors, "Rdata/COL_BestModel_FullPost_FoI_1000it.RDS")

FoI_predictors_ADM2<- subset(FoI_predictors, is.na(FoI_predictors$GID_2)==F)
saveRDS(FoI_predictors_ADM2, "Rdata/COL_BestModel_FullPost_FoI_1000it_ADM2only.RDS")
