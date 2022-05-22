##Function to prepare the figures of observed vs predicted FoIs
## September 2021
#Julia Ledien, Pierre Nouvellet
#model_name<- "A3_RF"
#predFoI<-readRDS("Comparison_with_LinearModels/res/Approach_comparison/ML_A3_RF_pred_obsFoI_10it_100boots")

## Create fig of obs vs pred for total serosurvey and normalised
Make_Obs_vs_Pred_Muni_Figs<- function(predFoI, model_name){
  
  library(ggplot2)
  
  obsFoIRaw<- readRDS("FoI_Observed/COL_FoI_1000it_MNormal_Model_ADM2only.RDS")
  obsFoIPred<-readRDS("FoI_Observed/COL_MNormal_Model_predictors_ADM2only.RDS")
  
  #exclude serosurveys organised before 1980
  excl<- unique(obsFoIPred$`FoI_predictors[, 1]`[which(obsFoIPred$tSur<1980)])
  obsFoIRaw<- obsFoIRaw[-which(obsFoIRaw$dataset_id %in% excl),]
  obsFoIPred<- obsFoIPred[-which(obsFoIPred$`FoI_predictors[, 1]` %in% excl),]
  
  #merge observation by municipality
  obsFoIPred$Code_mun<-paste0(obsFoIPred$GID_2_GDAM, "_",obsFoIPred$year)
  obsFoIPred$Code<-paste0(obsFoIPred$`FoI_predictors[, 1]`, "_",obsFoIPred$year)
  
  mun<- unique(obsFoIPred$Code_mun)
  ss<- matrix(NA,length(mun),6 )
  
  for (i in 1:length(mun)){
    d<- obsFoIPred$`FoI_predictors[, 1]`[which(obsFoIPred$Code_mun==mun[i])]
    y<- unique(obsFoIPred$year[which(obsFoIPred$Code_mun==mun[i])])
    m<- unique(obsFoIPred$GID_2_GDAM[which(obsFoIPred$Code_mun==mun[i])])
    tt<-as.numeric(as.matrix( obsFoIRaw[which(obsFoIRaw$dataset_id %in% d & obsFoIRaw$year==y ),1:1000]))
    ss[i,]<-c(quantile(tt)[2],quantile(tt)[3],quantile(tt)[4],y,m,mun[i])
    
                           
  }
  obsFoI<- as.data.frame(ss)
  colnames(obsFoI)<- c("low","med","up","year","municipality", "Code_mun")
  
  obsFoI$med<- as.numeric(obsFoI$med)
  obsFoI$year<- as.numeric(obsFoI$year)
  obsFoI$low<- as.numeric(obsFoI$low)
  obsFoI$up<- as.numeric(obsFoI$up)
  
  
  #merge predictions by municipality
pp<- matrix(NA,length(mun),6 )
  
  for (i in 1:length(mun)){
    d<- obsFoIPred$`FoI_predictors[, 1]`[which(obsFoIPred$Code_mun==mun[i])]
    y<- unique(obsFoIPred$year[which(obsFoIPred$Code_mun==mun[i])])
    m<- unique(obsFoIPred$GID_2_GDAM[which(obsFoIPred$Code_mun==mun[i])])
    cod<- paste0(y,"_",d)
    f<- which(FoIobs_A1$Code %in% cod )
    tt<-as.numeric(as.matrix( predFoI[f,]))
    pp[i,]<-c(quantile(tt)[2],quantile(tt)[3],quantile(tt)[4],y,m,mun[i])
    
    
  }
PredFoI<- as.data.frame(pp)
colnames(PredFoI)<- c("low","med","up","year","municipality", "Code_mun")

PredFoI$med<- as.numeric(PredFoI$med)
PredFoI$year<- as.numeric(PredFoI$year)
PredFoI$low<- as.numeric(PredFoI$low)
PredFoI$up<- as.numeric(PredFoI$up)


  merge_med <- data.frame(x=seq(min(obsFoI$year),max(obsFoI$year)))
  merge_low <- merge_med
  merge_up <- merge_med
  
  muni<-unique(obsFoI$municipality)
  
  for(i in 1:length(muni)){
    
    f <- which(obsFoI$municipality %in% muni[i])
    
    x <- obsFoI$year[f] 
    idx <- order(x)
    x <- x[idx]
    obs <- obsFoI$med[f] 
    obs_lower <- obsFoI$low[f] 
    obs_upper <- obsFoI$up[f] 
    
    pred_t <-t(PredFoI[which(PredFoI$municipality %in% muni[i]),1:3])
    pred_t <- pred_t[,idx]
    
    #over_within <- perf_MA$over_per_survey[f]*100
    
    
    dist_t <- matrix(NA,nrow = nrow(pred_t),ncol = ncol(pred_t))
    for (j in 1:length(f)){
      for (k in 1:3){
        if(obs[j] > pred_t[k,j]){
          dist_t[k,j] <- (pred_t[k,j] - obs[j])/(obs[j] - obs_lower[j])
        }else{
          dist_t[k,j] <- (pred_t[k,j] - obs[j])/(obs_upper[j] - obs[j])
        }
      }
    }
    
    merge_med <- merge(x = merge_med, 
                       y = data.frame(x = x, y = dist_t[2,]),
                       by = 'x', all = TRUE)
    names(merge_med)[ncol(merge_med)]<- muni[i]
    merge_low <- merge(x = merge_low,
                       y = data.frame(x = x, y =dist_t[1,]),
                       by = 'x',all = TRUE)
    names(merge_low)[ncol(merge_low)]<- muni[i]
    merge_up <- merge(x = merge_up, 
                      y = data.frame(x = x, y = dist_t[3,]),
                      by = 'x',all = TRUE)
    names(merge_up)[ncol(merge_up)]<- muni[i]
    
    ggplot()+ geom_line(aes( x, dist_t[2,]), colour="#FF9900") + geom_ribbon(aes(x=x,ymin=dist_t[1,],ymax=dist_t[3,]),alpha=0.3,fill="#FF9900")+theme_classic()+labs(title = paste(model_name, muni[i], sep=" "), y="Distance FoI observed vs predicted", x="Years") + coord_cartesian(ylim=c(-5,10))+ geom_hline(aes(yintercept=0), colour="red")+ geom_hline(aes(yintercept=1), colour="red",linetype=2) + geom_hline(aes(yintercept=-1), colour="red",linetype=2) 
    ggsave(paste0( "Comparison_with_LinearModels/res/ML_A3_Pred_vs_obs/Centered_graphs/",model_name,"/municipality_", muni[i], "_model_", model_name, ".png"))
  }
  
  a1 <- apply(merge_med[,-1], 1,quantile,c(.5,.25,.75),na.rm=TRUE)
  a2 <- apply(merge_low[,-1], 1,quantile,c(.5,.25,.75),na.rm=TRUE)
  a3 <- apply(merge_up[,-1], 1,quantile,c(.5,.25,.75),na.rm=TRUE)
  
  ggplot()+ geom_line(aes( merge_med$x,a1[1,]), colour="#9900FF") + geom_ribbon(aes(x=merge_med$x,ymin=a1[2,],ymax=a1[3,]),alpha=0.3,fill="#9900FF")+ geom_line(aes( merge_med$x,a3[1,]), colour="#FF6699") + geom_ribbon(aes(x=merge_med$x,ymin=a3[2,],ymax=a3[3,]),alpha=0.3,fill="#FF6699")+ geom_line(aes( merge_med$x,a2[1,]), colour="#66CCFF") + geom_ribbon(aes(x=merge_med$x,ymin=a2[2,],ymax=a2[3,]),alpha=0.3,fill="#66CCFF")+theme_classic()+labs(title = paste(model_name,"All municipalities", sep=" "), y="Distance FoI observed vs predicted", x="Years") + coord_cartesian(ylim=c(-5,5))+ geom_hline(aes(yintercept=0), colour="#9900FF")+ geom_hline(aes(yintercept=1), colour="#FF6699",linetype=2) + geom_hline(aes(yintercept=-1), colour="#66CCFF",linetype=2) 
  
  ggsave(paste0( "Comparison_with_LinearModels/res/ML_A3_Pred_vs_obs/Centered_graphs/",model_name,"/All_municipalities",  "_model_", model_name, ".png"))
  
}