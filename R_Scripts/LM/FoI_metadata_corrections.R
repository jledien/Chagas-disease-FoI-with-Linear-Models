
## correction of the metadata
setwd("C://Users/ledie/Dropbox/Chagas modelling/Linear models/Final_LM_analyses/Data_preparation/")
info<- readRDS("Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021.RDS")
library(readxl)
#OriginalMetadata<- read_xlsx("Raw_data/Raw_FoI_1000it/old_FOI_metadata.xlsx")
COL <- readRDS("Raw_data/COL_layer_format sp/gadm36_COL_2_sp.rds")

# corrections on zone
#info$setting[which(info$survey=="COL-035-39")]<-"rural"

# correction on ADM2 for the serosurveys organised in several municipalities, we are excluding them for the moment I will include them for the ML model
info$ADM2[which(info$survey=="COL-002-01")]<-NA
info$ADM2[which(info$survey=="COL-002-02")]<-NA
info$ADM2[which(info$survey=="COL-003-02")]<-NA
info$ADM2[which(info$survey=="COL-003-01")]<-NA
info$ADM2[which(info$survey=="COL-035-89")]<-NA

# Put indigenous serosurveys organised the Sierra Nevada of Santa Marta in Santa Marta municipality (Magdalena)
info$ADM2[which(info$survey=="COL-035-92")]<-"Santa Marta (Dist. Esp.)"
info$ADM2[which(info$survey=="COL-035-96")]<-"Santa Marta (Dist. Esp.)"
info$ADM2[which(info$survey=="COL-035-97")]<-"Santa Marta (Dist. Esp.)"
info$ADM1[which(info$survey=="COL-035-92")]<-"Magdalena"
info$ADM1[which(info$survey=="COL-035-96")]<-"Magdalena"
info$ADM1[which(info$survey=="COL-035-97")]<-"Magdalena"

##Add ADM2 GDAM codes
info$CodeADM2_Name<- paste0(info$ADM1, "_", info$ADM2)
COL_data<- COL@data
COL_data$CodeADM2_Name<- paste0(COL_data$NAME_1, "_", COL_data$NAME_2)
COL_data$CodeADM2_Name<- gsub("á", "a",COL_data$CodeADM2_Name) 
info$CodeADM2_Name<- gsub("á", "a",info$CodeADM2_Name)

COL_data$CodeADM2_Name<- gsub("í", "i",COL_data$CodeADM2_Name)
info$CodeADM2_Name<- gsub("í", "i",info$CodeADM2_Name)

COL_data$CodeADM2_Name<- gsub("ó", "o",COL_data$CodeADM2_Name)
info$CodeADM2_Name<- gsub("ó", "o",info$CodeADM2_Name)

COL_data$CodeADM2_Name<- gsub("ñ", "n",COL_data$CodeADM2_Name) 
info$CodeADM2_Name<- gsub("ñ", "n",info$CodeADM2_Name)

COL_data$CodeADM2_Name<- gsub("ú", "u",COL_data$CodeADM2_Name)
info$CodeADM2_Name<- gsub("ú", "u",info$CodeADM2_Name)

COL_data$CodeADM2_Name<- gsub("é", "e",COL_data$CodeADM2_Name)
info$CodeADM2_Name<- gsub("é", "e",info$CodeADM2_Name)

info_ADM2_code<- merge(info, COL_data[,c(6,14)], by="CodeADM2_Name", all.x=T)
length(unique(info$ADM2))
length(unique(info_ADM2_code$GID_2))

#Manual corrections:
info_ADM2_code$GID_2[which(info_ADM2_code$CodeADM2_Name=="Norte de Santander_Cucuta")]<-"COL.22.32_1"
info_ADM2_code$GID_2[which(info_ADM2_code$CodeADM2_Name=="Boyaca_San jose de Pare")]<-"COL.6.81_1"
info_ADM2_code$GID_2[which(info_ADM2_code$CodeADM2_Name=="Boyaca_Santa maria")]<-"COL.6.86_1"

length(unique(info$ADM2))
length(unique(info_ADM2_code$GID_2))

saveRDS(info_ADM2_code, "Raw_data/Raw_FoI_1000it/FOI-COL-2021/summary-COL-2021_revised.RDS")
