setwd("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/")
info<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021.RDS")
Foi_prev<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/data-COL-2021.RDS")
Foi_001<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/best/COL-001-01.RDS")
## the file only contained the 3 best models and we can't be sure that the continuous model (taht has been divided on 5 new models) will be selected
## decided thus to select the best model for each survey
## will add a description of the model used in the supp of the paper

OriginalDataEx<- readRDS("C://Users/ledie/Dropbox/Chagas modelling/backups/chagas-ML/analysis/FoI Linear Regression/Continuous FoI/Scripts/Pierre/approach1_2_3/Rdata/ContFoI_Median_ADM2Covariates_2020_ADM2only_Final.RDS")
library(readxl)
library(reshape2)
OriginalMetadata<- read_xlsx("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/Raw_FoI_1000it/old_FOI_metadata.xlsx")
length(unique(OriginalMetadata$New_Name))

##surveys that are not matching
surv<- c("COL-035-90", "COL-031-01", "COL-035-38", "COL-035-39", "COL-035-85", "COL-035-86", "COL-035-71", "COL-035-72", "COL-002-01", "COL-002-02", "COL-003-01", "COL-003-02", "COL-035-62", "COL-035-63", "COL-035-96")
info_sub<- info[-which(info$survey %in% OriginalMetadata$New_Name),]

datasetID <- unique(info$survey)
BigData<- data.frame(year=NA, FoI=NA, dataset_id=NA)

for (d in datasetID){
  file<- readRDS(paste0("best/",d,".RDS"))
  bestFoI<- file$foi_post_1000s[which(file$foi_post_1000s$best=="best1"),-c(which(colnames(file$foi_post_1000s)=="best"),which(colnames(file$foi_post_1000s)=="name_model"))]
  data<- melt(bestFoI)
  colnames(data) <- c("year", "FoI")
  data$dataset_id <- d
  BigData<-rbind(BigData,data)
}

saveRDS(BigData[2:nrow(BigData),],"C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/Raw_FoI_1000it/COL_FoI_merged_1000it")

####Adding covariates
library(raster)
library(rgeos)
setwd("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/")
FoI<- readRDS("Raw_data/Raw_FoI_1000it/COL_FoI_merged_1000it")
info<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021.RDS")
COL <- readRDS("Raw_data/COL_layer_format sp/gadm36_COL_2_sp.rds")

info_ADM2<- subset(info, !info$ADM2=="NA")
coord_surveys<-cbind(as.numeric(info_ADM2$long_dec), as.numeric(info_ADM2$lat_dec))
#coord_surveys<-coord_surveys[which(coord_surveys[,2]>1),]
sp_survey<-SpatialPoints(coords= coord_surveys, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(sp_survey)
coord<- extract(COL,  sp_survey)
info_ADM2_2<- cbind(info_ADM2, coord[,8:9])
GDAM_Coord<- cbind(coordinates(COL), COL$GID_2, COL$NAME_1, COL$NAME_2)

info_ADM2_2$not_match<- ifelse(info_ADM2_2$ADM2==info_ADM2_2$NAME_2, 0, 1)
table(info_ADM2_2$not_match)

##### not using the coordinates, they are not good enough to identify location
info_ADM2$CodeADM2_Name<- paste0(info_ADM2$ADM1, "_", info_ADM2$ADM2)
COL_data<- COL@data
COL_data$CodeADM2_Name<- paste0(COL_data$NAME_1, "_", COL_data$NAME_2)

info_ADM2_3<- merge(info_ADM2, COL_data, by="CodeADM2_Name")
