approach1_inference_bootstrap <- function(FoI_ADM2, obs_dist, models, 
                                          predictorlist, n_boot, p_infer){
  
  
  Max_obs_foi <- apply(obs_dist,1,max)
  
  n_dataset <- length(unique(FoI_ADM2$dataset_id))
  
  # inference on half of the dataset - prechoose the surveys
  n_infer <- round(n_dataset*p_infer)
  f_incl <- list(f_infer = matrix(NA,n_infer,n_boot),
                 f_out = matrix(NA,nrow(FoI_ADM2)-n_infer,n_boot)) 
  f_ind <- which(FoI_ADM2$zone %in% 'indigenous')
  f_urban <- which(FoI_ADM2$zone %in% 'urban')
  f_rural <- which(FoI_ADM2$zone %in% 'rural')
  
  #get sepearate vector of indices for each survey
  index_byID <- split(1:nrow(FoI_ADM2),FoI_ADM2$dataset_id)
  
  Betas_full <- list()
  
  # choose the selection
  for(k in 1:n_boot){
    f_incl$f_infer[,k] <- sample(unlist(lapply(index_byID,sample,size=1)),
                                 replace = FALSE,size = n_infer)
    f_incl$f_out[,k] <- c(1:nrow(FoI_ADM2))[-f_incl$f_infer[,k]]
  }
  
  # run model and predictions for all models
  yfull <- apply(obs_dist,1,median)
  log_yfull <- log(yfull)
  # plot(yfull,FoI_ADM2$FoI_Med)
  
  # run inference for each of >400 models
  for(i in 1:length(predictorlist)){
    print(i)
    # initial setup of variables
    ff <- as.formula( paste0("logFoIMed~", predictorlist[[i]]))
    m <- model.frame(ff, FoI_ADM2)
    mm <- model.matrix(ff,m) # cbind(1, log(trees$Girth))   # model matrix
    
    Betas <- matrix(NA,n_boot,ncol(mm))
    names_Betas <- colnames(mm)
    colnames(Betas) <- names_Betas
    FoI_pred <- matrix(NA,length(yfull),n_boot)
    
    Perf <- rep(NA,3*3) # performance: 3 settings * (all,first half,out)
    over <- rep(NA,3) # performance: 3 settings * (all,first half,out)
    Perf_out <- matrix(NA,n_boot,3)
    #run bootstrap and predictions
    for (k in 1:n_boot){
      y <- log_yfull[f_incl$f_infer[,k]]
      ## bare-bones direct interface
      flm2 <- lm.fit(x = mm[f_incl$f_infer[,k],],
                     y = y)
      
      flm2$coefficients[is.na(flm2$coefficients)] <- 0
      Betas[k,] <- flm2$coefficients
      
      Y_pred <- mv_mult( lhs = mm , rhs = flm2$coefficients)
      
      FoI_pred[,k] <- exp(Y_pred)
      
      yfull2 <- yfull
      yfull2[f_incl$f_infer[,k]] <- NA
      A <- (yfull2- FoI_pred[,k])^2
      B <- (yfull2 - mean(yfull))^2
      

      Perf_out[k,1] <- 1-( matrixStats::sum2(x = A ,idxs = f_ind, na.rm = TRUE )/
                             matrixStats::sum2(x = B ,idxs = f_ind, na.rm = TRUE) )
      
      Perf_out[k,2] <- 1-( matrixStats::sum2(x = A ,idxs = f_rural, na.rm = TRUE )/
                             matrixStats::sum2(x = B ,idxs = f_rural, na.rm = TRUE ) )
      
      Perf_out[k,3] <- 1-( matrixStats::sum2(x = A ,idxs = f_urban, na.rm = TRUE )/
                             matrixStats::sum2(x = B ,idxs = f_urban, na.rm = TRUE ) )
      
    }
    
    ###########################
    # get Rsq for all dataset
    FoI_pred_sum <- apply(FoI_pred, 1, quantile,c(.5))#,.25,.75,.025,.975))
    A <- (yfull- FoI_pred_sum)^2#[1,])^2
    B1 <- sum((yfull[f_ind] - mean(yfull))^2)
    B2 <- sum((yfull[f_rural] - mean(yfull))^2)
    B3 <- sum((yfull[f_urban] - mean(yfull))^2)
    
    Perf[1] <-  1 - ( matrixStats::sum2(x = A ,idxs = f_ind )/B1)
    Perf[2] <-  1 - ( matrixStats::sum2(x = A ,idxs = f_rural )/B2)
    Perf[3] <-  1 - ( matrixStats::sum2(x = A ,idxs = f_urban )/B3)
    
    ###########################
    ## get Rsq for out of sample
    Perf[4:6] <-  apply(Perf_out, 2, median)
    
    ###########################
    ## get Rsq for out of sample for first half dataset 
    Perf[7:9] <-  apply(Perf_out[1:round(n_boot/2),], 2, median)
    
    ###########################
    ## overlap
    
    temp <- rep(NA,nrow(FoI_ADM2))
    for (j in 1:nrow(FoI_ADM2)){
      z <- FoI_pred[j,]
      z[which(z>Max_obs_foi[j])] <- 1.2*Max_obs_foi[j]
      temp[j] <- overlapping::overlap(x = list(obs_dist[j,],
                                      z),nbins = 100,plot = FALSE)$OV
      
      # overlapping::overlap(x = list(obs_dist[j,],
      #                               FoI_pred[j,]),nbins = 100,plot = TRUE)#,
      #                      # boundaries = list( from = 0, to = 1e20 ))
    }
    over <- c(mean(temp[f_ind]), mean(temp[f_rural]), mean(temp[f_urban]))
        
    # save output performance, coefficient and predictions summary
    Betas_full[[i]] <- list(Betas = Betas, Perf = Perf, over = over, 
                            over_per_survey = temp )
    

  }
  
  return(Betas_full)
}