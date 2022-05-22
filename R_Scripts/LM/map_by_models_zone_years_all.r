
titles <- c('Median','up95','CV','Median','up50','up50 scaled')

for (year in Years){
  for (zone in 1:length(zone_unique)){
    for(rank_model in 1:5){
      
      # year = Years[1]
      # zone = 3
      # rank_model = 1
      
      # iu <- Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]]
      New_pred<- data.frame(GID_2_GDAM = pred_mat$GID_2_GDAM[1:1065],
                            Median = Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]][,1],
                            up95 = Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]][,5],
                            CV = Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]][,7],
                            up50 = Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]][,3],
                            up50_sc = Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]][,3]/
                              Y_pred_colombia_sum[[as.character(year)]][[zone]][[rank_model]][,1])
      
      
      # create spatial file
      temp <- Pred_ggplotdDF
      temp = data.frame(temp, 
                        New_pred[match(temp[,"GID_2"], New_pred[,"GID_2_GDAM"]),])

      
      # rg <- c(0,max(temp$up95))
      
      p1 <- ggplot(data = temp, aes(x=long, y=lat,
                                    group = group, fill = Median))+
        geom_polygon()  +
        # geom_path(color = "white") +
        scale_fill_gradientn(colours = terrain.colors(7)) + #,limits = rg) +
        coord_equal() + ggtitle(titles[1]) +
        theme(axis.text = element_blank())
      
      p2 <- ggplot(data = temp, aes(x=long, y=lat,
                                    group = group, fill = up95))+
        geom_polygon()  +
        # geom_path(color = "white") +
        scale_fill_gradientn(colours = terrain.colors(7)) + #,limits = rg) +
        coord_equal() + ggtitle(titles[2]) +
        theme(axis.text = element_blank())
      
      p3 <- ggplot(data = temp, aes(x=long, y=lat,
                                    group = group, fill = CV))+
        geom_polygon()  +
        # geom_path(color = "white") +
        scale_fill_gradientn(colours = terrain.colors(7)) +
        coord_equal() + ggtitle(titles[3]) +
        theme(axis.text = element_blank())
      
      p4 <- ggplot(data = temp, aes(x=long, y=lat,
                                    group = group, fill = up50))+
        geom_polygon()  +
        # geom_path(color = "white") +
        scale_fill_gradientn(colours = terrain.colors(7)) +
        coord_equal() + ggtitle(titles[4]) +
        theme(axis.text = element_blank())
      
      p5 <- ggplot(data = temp, aes(x=long, y=lat,
                                    group = group, fill = up50_sc))+
        geom_polygon()  +
        # geom_path(color = "white") +
        scale_fill_gradientn(colours = terrain.colors(7)) +
        coord_equal() + ggtitle(titles[5]) +
        theme(axis.text = element_blank())
      
      
      gridExtra::grid.arrange(p1, p2, p3, p1, p4, p5, 
                              ncol=3, nrow = 2,top = paste0(zone_unique[zone],' - ',year))
    }
  }
}
