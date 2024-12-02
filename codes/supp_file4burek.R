rm(list=ls())
library(stringr)
library(gdxrrw)
library(dplyr)
library(tidyr)
library(readr)
library(terra)
library(downscalr)
library(rworldmap)
library(ggplot2)
GAMSPath = c("C:/GAMS/40")
igdx(GAMSPath)

simu_raster <- rast("./input/simu_raster/w001001.adf")

EU_only <- TRUE
if(EU_only){
  eu.extent <- ext(c(-25, 35, 34, 71))
  simu_raster <- crop(simu_raster, eu.extent)
}
curr.ns <- unique(values(simu_raster))


scenarios <- list.files("./results/", full.names = T)
short.names <- list.files("./results/")
short.names <- gsub(".RData","",short.names)

scen <- scenarios[1]
for(scen in scenarios){

  scen.name <- short.names[which(scen==scenarios)]

  load(scen)
  luc.res <- luc.res %>% filter(ns%in%as.character(curr.ns))

  start.areas <-  luc.res %>% filter(times=="2010") %>% group_by(ns, lu.from) %>% summarise(value=sum(value)) %>% mutate(times="2000") %>% rename("lu.to"="lu.from")
  pixel.size <- start.areas %>% group_by(ns) %>% summarise(area=sum(value))
  data <-  luc.res %>% group_by(ns, times, lu.to) %>% summarise(value=sum(value))

  data <- start.areas %>% bind_rows(data)

  data <- data %>% group_by(ns, times) %>% mutate(value=value/sum(value))


  grid <- expand.grid(lu=unique(data$lu.to), time=unique(data$times))

  x <- list()
  temp.raster <- simu_raster

a <- 1
for(lu in unique(data$lu.to)){
  temp.data <- data %>% filter(lu.to==lu)
  for(tt in unique(data$times)){
    curr.data <- temp.data %>% filter(times==tt)

    curr.tag <- paste0(lu,"_", tt)

    values(temp.raster) <- curr.data$value[match(values(simu_raster), curr.data$ns)]
    names(temp.raster) <- curr.tag
    x[[a]] <- temp.raster
    a <- a+1
  }
}

  z <- rast(x)
  writeRaster(z, paste0("results/EU_areashares_",scen.name,"_.tiff"), overwrite=TRUE)
}

#
#
#
# test <- updated_map %>% filter(lu.class=="prot.other")
#
# values(temp.raster) <- test$value[match(values(simu_raster), test$SimUID)]
#
# updated_map
#
#
# raster_layer <- z[[310:381]]
#
# # Convert the SpatRaster object to a data frame
# df_raster <- as.data.frame(temp.raster, xy = TRUE)
#
# ggplot() +
#   geom_raster(data = df_raster, aes(x = x, y = y, fill = w001001)) +
#   scale_fill_viridis_c() + # Optional: For a nice color scale
#   coord_equal() + # Ensures the aspect ratio is maintained
#   theme_minimal() +
#   labs(title = "Raster Layer Visualization", fill = "Values")




