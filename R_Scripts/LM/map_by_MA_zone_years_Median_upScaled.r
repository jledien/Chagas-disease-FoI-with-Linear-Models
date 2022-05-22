
titles <- c('Median','up95','CV','up50','up50 scaled')
up_lim <- c(0.1, 0.01,0.01)

breaks_map1=list(c(0,0.05,.1),c(0,0.005,.01),c(0,0.005,.01))
breaks_map2=c(0,5,10)


for (zone in 1:length(zone_unique)){
  p <- list()
  q <- list()
  for (year in Years){
    
    # year = Years[1]
    # zone = 3
    
    
    # iu <- Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]]
    New_pred<- data.frame(GID_2_GDAM = pred_mat$GID_2_GDAM[1:1065],
                          Median = Y_pred_colombia_sum_MA[[as.character(year)]][[zone]][,1],
                          up95 = Y_pred_colombia_sum_MA[[as.character(year)]][[zone]][,5],
                          CV = Y_pred_colombia_sum_MA[[as.character(year)]][[zone]][,7],
                          up50 = Y_pred_colombia_sum_MA[[as.character(year)]][[zone]][,3],
                          up50_sc = Y_pred_colombia_sum_MA[[as.character(year)]][[zone]][,3]/
                            Y_pred_colombia_sum_MA[[as.character(year)]][[zone]][,1])
    
    New_pred$up50_sc[which(New_pred$up50_sc > 10)] <- NA
    
    # create spatial file
    temp <- Pred_ggplotdDF
    temp = data.frame(temp, 
                      New_pred[match(temp[,"GID_2"], New_pred[,"GID_2_GDAM"]),])
    
    temp2 <- BoundADM2
    temp2@data = data.frame(temp2@data,
                            New_pred[match(temp2@data[,"GID_2"], New_pred[,"GID_2_GDAM"]),])
    
    rgdal::writeOGR(temp2, paste0("Rdata/maps/",approach), # maps: name folder 1 per approach
                    layer=paste0("ADM2_FOI_Pred_Med_",year,'_',zone_unique[zone]), # name of file - year_zone
                    driver="ESRI Shapefile",overwrite_layer = TRUE, check_exists=TRUE)
    # rg <- c(0,max(temp$up95))
    
    p[[which(Years %in% year)]] <- ggplot(data = temp, aes(x=long, y=lat,
                                                           group = group, fill = Median))+
      geom_polygon()  + xlab('') + ylab('') +
      # geom_path(color = "white") +
      scale_fill_gradientn('',colours = terrain.colors(7),
                           breaks = breaks_map1[[zone]], limits = c(0,up_lim[zone])) +
      coord_equal() + ggtitle(paste0(titles[1],' - ',year)) +
      theme(axis.text = element_blank(), 
            legend.text=element_text(size=6),
            legend.key.height = unit(.3,'cm'),
            legend.key.width = unit(.2,'cm'))
    
    q[[which(Years %in% year)]] <- ggplot(data = temp, aes(x=long, y=lat,
                                                           group = group, fill = up50_sc))+
      geom_polygon()  + xlab('') + ylab('') +
      # geom_path(color = "white") +
      scale_fill_gradientn('',colours = terrain.colors(7), 
                           breaks = breaks_map2, limits = c(0,10)) +
      coord_equal() + ggtitle(paste0(titles[5],' - ',year)) +
      theme(axis.text = element_blank(), 
            legend.text=element_text(size=6),
            legend.key.height = unit(.3,'cm'),
            legend.key.width = unit(.2,'cm'))
    
  }
  gridExtra::grid.arrange(p[[1]], p[[2]],p[[3]],p[[4]],
                          q[[1]], q[[2]],q[[3]],q[[4]],
                          ncol=4, nrow = 2,top = paste0(zone_unique[zone]))
}

