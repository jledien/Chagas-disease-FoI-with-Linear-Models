setwd("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/Data_preparation/")
info<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021_revised.RDS")
Foi_001<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/MNormal/COL-001-01.RDS")

## the file only contained the 3 best models and we want the best continuous model
## will add a description of the model used in the supp of the paper

library(readxl)
library(reshape2)

datasetID <- unique(info$survey)
BigData<- matrix (NA,2,1002 )

for (d in datasetID){
  file<- readRDS(paste0("Raw_data/Raw_FoI_1000it/FOI-COL-2021/MNormal/",d,".RDS"))
  data<- t(file[,-c(which(colnames(file)=="survey"),which(colnames(file)=="model"))])
  
  data <- cbind(data,rep(d, nrow(data)))
  data <- cbind(data,as.numeric(rownames(data)))
  BigData<-rbind(BigData,data)
}

colnames(BigData) <- c(paste0("FoI_it", 1:1000), "dataset_id", "year")

#### correction of the code for COL-001-01 and COL-001-02
BigData<- as.data.frame(BigData)
BigData$dataset_id [which(BigData$dataset_id== "COL-001-01")]<-"ToChange"
BigData$dataset_id [which(BigData$dataset_id== "COL-001-02")]<-"COL-001-01"
BigData$dataset_id [which(BigData$dataset_id== "ToChange")]<-"COL-001-02"

saveRDS(BigData[3:nrow(BigData),],"Rdata/COL_FoI_1000it_MNormal_Model.RDS")

###ADM2 only
datasetID <- unique(info$survey[which(is.na(info$GID_2)==F)])
BigData<- matrix (NA,2,1001 )
CodeID<- rep(NA,2)

for (d in datasetID){
  file<- readRDS(paste0("Raw_data/Raw_FoI_1000it/FOI-COL-2021/MNormal/",d,".RDS"))
  data<- t(file[,-c(which(colnames(file)=="survey"),which(colnames(file)=="model"))])
  CodeID <- c(CodeID,rep(d, nrow(data)))
  data <- cbind(data,as.numeric(rownames(data)))
  BigData<-rbind(BigData,data)
 
}

BigData2<- as.data.frame(BigData)
colnames(BigData2) <- c(paste0("FoI_it", 1:1000),  "year")
BigData2$dataset_id<- CodeID

#### correction of the code for COL-001-01 and COL-001-02
BigData2<- as.data.frame(BigData2)
BigData2$dataset_id [which(BigData2$dataset_id== "COL-001-01")]<-"ToChange"
BigData2$dataset_id [which(BigData2$dataset_id== "COL-001-02")]<-"COL-001-01"
BigData2$dataset_id [which(BigData2$dataset_id== "ToChange")]<-"COL-001-02"

saveRDS(BigData2[3:nrow(BigData2),],"Rdata/COL_FoI_1000it_MNormal_Model_ADM2only.RDS")

####matrix of covariates
FoI<- as.data.frame(BigData2[3:nrow(BigData2),1001:1002])
source("Scripts/FUN_ADM2_Predictors.R")

FoI_predictors<- merge(FoI, info, by.x="dataset_id", by.y="survey", all.x=T)

FoI_predictors_ADM2<-  as.data.frame(ADM2_Predictors (FoI_predictors$year[1], FoI_predictors$tsur[1], FoI_predictors$setting[1], FoI_predictors$GID_2[1]))
for (i in 2:nrow(FoI_predictors)){
  FoI_predictors_ADM2[i,]<-  ADM2_Predictors (FoI_predictors$year[i], FoI_predictors$tsur[i], FoI_predictors$setting[i], FoI_predictors$GID_2[i])
}

FoI_predictors<- cbind(FoI_predictors[,1], FoI_predictors_ADM2)

saveRDS(FoI_predictors, "Rdata/COL_MNormal_Model_predictors_ADM2only.RDS")

