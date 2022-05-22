#### MunML analyses with the predictors from Linear models
##Data preparation and package loading
#### Julia Ledien june 2021

library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3viz)
library(mlbench)
library(dplyr)
library(partykit)
library(ggplot2)
library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3extralearners)
library(mlr3tuning)
library(paradox)
source("Comparison_with_LinearModels/FUN_ADM2_Predictors.R")
library(splitstackshape)
library(rgdal)
library(reshape2)
source("Scripts/Comparison_ML_LM/FUN_makingMaps.R")
library(overlapping)
library(spdep)
library(sp)
library(rgdal)
library(gstat)

# Use of the best model from tuning analyses and add bootstrap on the top to take into account uncertainty
#indigenous included
obsFoI<- readRDS("FoI_Observed/COL_FoI_1000it_MNormal_Model_ADM2only.RDS")
obsFoIPred<-readRDS("FoI_Observed/COL_MNormal_Model_predictors_ADM2only.RDS")
ss<- as.data.frame(cbind(apply(obsFoI[,1:1000], 1, median, na.rm=T), paste0(obsFoI$year,"_",obsFoI$dataset_id)))
colnames(ss)<- c("MedFoI", "Code")
obsFoIPred$Code<- paste0(obsFoIPred$year,"_",obsFoIPred$`FoI_predictors[, 1]`)
length(unique(obsFoIPred$Code))
length(unique(ss$Code))

FoI<- merge(ss, obsFoIPred, by.x="Code", by.y="Code")
FoI$RurZone<- ifelse(FoI$zone=="rural",1,0)

## need to remove classified variables
FoIobs_A1<- FoI[, -c(#which(colnames(FoI)=="Code"), 
                     which(colnames(FoI)=="FoI_predictors[, 1]"), 
                     which(colnames(FoI)=="GID_2_GDAM"), 
                     which(colnames(FoI)=="DPMP"), 
                     which(colnames(FoI)=="NAME_1_GDAM"), 
                     which(colnames(FoI)=="DP"), 
                     which(colnames(FoI)=="DPNOM"),
                     which(colnames(FoI)=="MPIO"), 
                     which(colnames(FoI)=="NAME_2_GDAM"), 
                     which(colnames(FoI)=="NAME_2"), 
                     which(colnames(FoI)=="zone"), 
                     which(colnames(FoI)=="Intervention"), 
                     which(colnames(FoI)=="TropicalClimate"), 
                     which(colnames(FoI)=="HouseIntervClass"))] 
rm(obsFoI,obsFoIPred,ss,FoI)                     

FoIobs_A1$MedFoI<- as.numeric(FoIobs_A1$MedFoI)
FoIobs_A1<- FoIobs_A1[which(FoIobs_A1$tSur>=1980),]


##ML_A3

obsFoI_brut<- readRDS("FoI_Observed/COL_FoI_1000it_MNormal_Model_ADM2only.RDS")
obsFoIPred<-readRDS("FoI_Observed/COL_MNormal_Model_predictors_ADM2only.RDS")
obsFoI<- melt(obsFoI_brut, id=c("year", "dataset_id"), value.name = "FoI")
obsFoI$Code<- paste0(obsFoI$year,"_",obsFoI$dataset_id)
obsFoI<-obsFoI[,-c(which(colnames(obsFoI)=="year"),which(colnames(obsFoI)=="dataset_id"),which(colnames(obsFoI)=="variable")) ]
length(unique(obsFoI$Code))
obsFoIPred$Code<- paste0(obsFoIPred$year,"_",obsFoIPred$`FoI_predictors[, 1]`)

FoI<- merge(obsFoI, obsFoIPred, by.x="Code", by.y="Code")
FoI$RurZone<- ifelse(FoI$zone=="rural",1,0)

## need to remove classified variables
FoIobs_A3<- FoI[, -c(#which(colnames(FoI)=="Code"), 
  which(colnames(FoI)=="FoI_predictors[, 1]"), 
  which(colnames(FoI)=="GID_2_GDAM"), 
  which(colnames(FoI)=="DPMP"), 
  which(colnames(FoI)=="NAME_1_GDAM"), 
  which(colnames(FoI)=="DP"), 
  which(colnames(FoI)=="DPNOM"),
  which(colnames(FoI)=="MPIO"), 
  which(colnames(FoI)=="NAME_2_GDAM"), 
  which(colnames(FoI)=="NAME_2"), 
  which(colnames(FoI)=="zone"), 
  which(colnames(FoI)=="Intervention"), 
  which(colnames(FoI)=="TropicalClimate"), 
  which(colnames(FoI)=="HouseIntervClass"))] 
rm(obsFoI,obsFoIPred,FoI, obsFoI_brut)                     

FoIobs_A3<- FoIobs_A3[which(FoIobs_A3$tSur>=1980),]
FoIobs_A3$row_code<- paste0("rn", 1:nrow(FoIobs_A3))
