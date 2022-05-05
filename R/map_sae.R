
map_sae<-function(shapefile,data,area,indicators,breaks=FALSE,main=FALSE,output_data=FALSE)

  {
  
  area.name<-all.vars(area)
  indicators.name<-all.vars(indicators)
  
  b<-ifelse(class(breaks)=="list",TRUE,FALSE)
  
  tryCatch(map <- merge(shapefile, data, by = area.name),
           error = function (e) {print(paste("Error:",area, "argument is not present in both shapefile and data input"))})
  
  
  for (i in indicators.name)
  {
    if (b)
    {
    map[,i] <- cut(as.data.frame(map[,i])[,i], breaks[[match(i,indicators.name)]])
    }
    
    if (length(indicators.name)>1)
    {
      readline(prompt="Press [enter] to continue...")
    }
    
  dev.new()
  print(ggplot(map) +
    geom_sf(aes_string(fill = i))+
    {if(main) ggtitle(paste0(i," map"))}+
    {if (b) scale_fill_brewer(palette = "OrRd")}+
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
      style = north_arrow_fancy_orienteering)) 
  

 }
  if (output_data)
  {
    return(map)
  }
}