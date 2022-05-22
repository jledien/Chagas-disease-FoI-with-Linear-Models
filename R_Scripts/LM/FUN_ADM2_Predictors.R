#### Function to create predictors dataset for CD Linear Models at ADm2 level, using ADM2 only serosurveys
##Julia Ledien 15/12/20

ADM2_Predictors_AllADM2 <- function(year, tSur, zone,Predictors){
  
  # Predictors <- readRDS( "C:/Users/Julia L/Dropbox/Chagas modelling/backups/chagas-ML/analysis/FoI Linear Regression/Continuous FoI/Median/ADM2/R-based selection/predictions/predictors_ADM2")
  Predictors$tSur <- tSur
  Predictors$zone[1:1065] <- zone
  Predictors$IndiZone<- ifelse(Predictors$zone=="indigenous", 1, 0)
  Predictors$UrbZone <- ifelse(Predictors$zone=="urban", 1,0)
  Predictors$TemperateFreq<- Predictors$TempFreq
  
  

  Predictors$year <- year
  Predictors$decade <- ifelse(Predictors$year>=1950 & Predictors$year <=1959,6, ifelse(Predictors$year>=1960 & Predictors$year <=1969, 7, ifelse(Predictors$year>=1970 & Predictors$year <=1979, 8,ifelse(Predictors$year>=1980 & Predictors$year <=1989,9,ifelse(Predictors$year>=1990 & Predictors$year <=1999, 10, ifelse(Predictors$year>=2000 & Predictors$year <=2009, 11,12))))))
  
  Predictors$InterventionIntensity <- ifelse(Predictors$year<1996, Predictors$IntervIntensity1930_1996_M,ifelse(Predictors$year>=1996 & Predictors$year<2001, Predictors$IntervIntensity1996_2000_M, ifelse(Predictors$year >= 2001 & Predictors$year<2011, Predictors$IntervIntensity2001_2010_M, Predictors$IntervIntensity2011_2014_M)))
  Predictors$InterventionIntensity[is.na(Predictors$InterventionIntensity)==T]<- 0
  Predictors$Intervention <- ifelse(Predictors$InterventionIntensity>0 & Predictors$InterventionIntensity<= 0.05, "2  Low", ifelse(Predictors$InterventionIntensity>0.05 & Predictors$InterventionIntensity<=0.25,"2  Low", ifelse(Predictors$InterventionIntensity>0.25 & Predictors$InterventionIntensity<=0.5, "3  Medium", ifelse(Predictors$InterventionIntensity>0.5 & Predictors$InterventionIntensity<=0.75, "4  High", ifelse(Predictors$InterventionIntensity>0.75, "5  Very High", "0 Absence"))) ))
  
  Predictors$HouseInterv <- ifelse(Predictors$year<1996, Predictors$IntervIntensity1930_1996_H,ifelse(Predictors$year>=1996 & Predictors$year<2001, Predictors$IntervIntensity1996_2000_H, ifelse(Predictors$year >= 2001 & Predictors$year<2011, Predictors$IntervIntensity2001_2010_H, Predictors$IntervIntensity2011_2014_H)))
  Predictors$HouseInterv[is.na(Predictors$HouseInterv)==T]<- 0
  Predictors$HouseIntervClass <- ifelse(Predictors$HouseInterv>0.10, "High", ifelse(Predictors$HouseInterv==0, "Absence", "medium"))
  
  Predictors$TempHouseholdDef <- ifelse(Predictors$year<= 1999 , Predictors$HouseholdDeficit1993ADM2,  Predictors$HouseholdDeficit2005ADM2)
  
  Predictors$TempPopDensity <- ifelse(Predictors$year< 1985, Predictors$`1985`/Predictors$ADM2area_sqkm,  Predictors[,as.character(year)]/Predictors$ADM2area_sqkm)
  Predictors$TempPopDensity [Predictors$GID_2_GDAM=="COL.32.4_1"] <-mean(Predictors$TempPopDensity[Predictors$NAME_1_GDAM=="Vichada"], na.rm=T) 
  Predictors$TempPopDensity [Predictors$GID_2_GDAM=="COL.32.5_1"] <-mean(Predictors$TempPopDensity[Predictors$NAME_1_GDAM=="Vichada"], na.rm=T) 
  Predictors$TempPopDensity [Predictors$GID_2_GDAM=="COL.26.1_1"] <-mean(Predictors$TempPopDensity[Predictors$NAME_1_GDAM=="San Andr?s y Providencia"], na.rm=T) 
  
  Predictors$PolarClimatePresence <- ifelse(Predictors$PolarFreq>0,1,0)
  
  data<- Predictors[,c(1:18,29:35,73:86)]
  
    return(data)
}
## The output is a table of the predictors for the 1065 municipalities plus 6 lines to account for all the options in classified variables

 ADM2_Predictors <- function(year, tSur, zone, ADM2_GDAM_Code){
   
   Predictors <- readRDS( "Raw_data/predictors_ADM2")
   Predictors$tSur <- tSur
   Predictors$zone[1:1065] <- zone
   Predictors$IndiZone<- ifelse(Predictors$zone=="indigenous", 1, 0)
   Predictors$UrbZone <- ifelse(Predictors$zone=="urban", 1,0)
   Predictors$TemperateFreq<- Predictors$TempFreq
   
   
   
   Predictors$year <- year
   Predictors$decade <- ifelse(Predictors$year>=1950 & Predictors$year <=1959,6, ifelse(Predictors$year>=1960 & Predictors$year <=1969, 7, ifelse(Predictors$year>=1970 & Predictors$year <=1979, 8,ifelse(Predictors$year>=1980 & Predictors$year <=1989,9,ifelse(Predictors$year>=1990 & Predictors$year <=1999, 10, ifelse(Predictors$year>=2000 & Predictors$year <=2009, 11,12))))))
   
   Predictors$InterventionIntensity <- ifelse(Predictors$year<1996, Predictors$IntervIntensity1930_1996_M,ifelse(Predictors$year>=1996 & Predictors$year<2001, Predictors$IntervIntensity1996_2000_M, ifelse(Predictors$year >= 2001 & Predictors$year<2011, Predictors$IntervIntensity2001_2010_M, Predictors$IntervIntensity2011_2014_M)))
   Predictors$InterventionIntensity[is.na(Predictors$InterventionIntensity)==T]<- 0
   Predictors$Intervention <- ifelse(Predictors$InterventionIntensity>0 & Predictors$InterventionIntensity<= 0.05, "2  Low", ifelse(Predictors$InterventionIntensity>0.05 & Predictors$InterventionIntensity<=0.25,"2  Low", ifelse(Predictors$InterventionIntensity>0.25 & Predictors$InterventionIntensity<=0.5, "3  Medium", ifelse(Predictors$InterventionIntensity>0.5 & Predictors$InterventionIntensity<=0.75, "4  High", ifelse(Predictors$InterventionIntensity>0.75, "5  Very High", "0 Absence"))) ))
   
   Predictors$HouseInterv <- ifelse(Predictors$year<1996, Predictors$IntervIntensity1930_1996_H,ifelse(Predictors$year>=1996 & Predictors$year<2001, Predictors$IntervIntensity1996_2000_H, ifelse(Predictors$year >= 2001 & Predictors$year<2011, Predictors$IntervIntensity2001_2010_H, Predictors$IntervIntensity2011_2014_H)))
   Predictors$HouseInterv[is.na(Predictors$HouseInterv)==T]<- 0
   Predictors$HouseIntervClass <- ifelse(Predictors$HouseInterv>0.10, "High", ifelse(Predictors$HouseInterv==0, "Absence", "medium"))
   
   Predictors$TempHouseholdDef <- ifelse(Predictors$year<= 1999 , Predictors$HouseholdDeficit1993ADM2,  Predictors$HouseholdDeficit2005ADM2)
   
   Predictors$TempPopDensity <- ifelse(Predictors$year< 1985, Predictors$`1985`/Predictors$ADM2area_sqkm,  Predictors[,as.character(year)]/Predictors$ADM2area_sqkm)
   Predictors$TempPopDensity [Predictors$GID_2_GDAM=="COL.32.4_1"] <-mean(Predictors$TempPopDensity[Predictors$NAME_1_GDAM=="Vichada"], na.rm=T) 
   Predictors$TempPopDensity [Predictors$GID_2_GDAM=="COL.32.5_1"] <-mean(Predictors$TempPopDensity[Predictors$NAME_1_GDAM=="Vichada"], na.rm=T) 
   Predictors$TempPopDensity [Predictors$GID_2_GDAM=="COL.26.1_1"] <-mean(Predictors$TempPopDensity[Predictors$NAME_1_GDAM=="San Andr?s y Providencia"], na.rm=T) 
   
   Predictors$PolarClimatePresence <- ifelse(Predictors$PolarFreq>0,1,0)
   
   data<- Predictors[which(Predictors$GID_2_GDAM==ADM2_GDAM_Code),c(1:18,29:35,73:86)]
   
   return(data)
 }
 ## the output is the predictiors for one municipalities so can be use to build predictors in new subset of ADM2