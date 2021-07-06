#Code to produce figure of sampling design
library(cartography)
library(sf)
#library(stars)
library(tmap)

#open shapefiles of patch areas
folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/GIS"
#pubs folder
ptemp<-"Publications/2021/CoffeeLandscapes/"
#setwd(paste0(folder_names,dtemp))
folder_names2 <- "C:/Users/AMorel001/OneDrive - University of Dundee/Documents/Research/Proposals/NERC/Frontiers/imagery"


patches<-st_read(paste0(folder_names,dtemp,"/shapefiles/yayu/newstrata_ll.shp"))
ndvi<-raster::raster(paste0(folder_names,dtemp,"/imagery/YayuNDVI.tif"))
#plot(ndvi,patches)
oromia<-st_read(paste0(folder_names2,"/oromia.shp"))
ethiopia<-st_read(paste0(folder_names2,"/ETH_outline.shp"))
yayu<-st_read(paste0(folder_names,dtemp,"/shapefiles/yayu/Merged_areas.shp"))
cities<-st_read(paste0(folder_names2,"/main_cities.shp"))

plot(st_geometry(patches),add=T)
typoLayer(x = patches, var = "Label",
                 legend.title.txt = "Patch Size/Elevation\nClasses",
          col = c("aquamarine4", "yellow3","wheat","red","orange","purple"))

pts<-st_read(paste0(folder_names,dtemp,"/shapefiles/yayu/final_points_v5.shp"))

bx<-st_bbox(patches)
boundary <- tmaptools::bb_poly(bx)

map1<-tm_shape(ndvi,bbox=bx) + tm_raster(style = "cont", palette = "YlGn", legend.show = F,alpha=0.7) +
  tm_shape(patches) + tm_polygons(col="Label",title = "Patch Size/\nElevation Classes",palette = "Accent") + #tm_borders() + 
  tm_compass(type = "8star", position = c("right", "bottom")) +
  tm_scale_bar( size = 1.0,position=c("left","bottom")) + tm_shape(pts) + tm_dots(shape=21,size=0.5,col="yellow") +
  tm_layout(legend.text.size = 1.25,legend.title.size=1.5,
    legend.position = c("left","top"))

inset<-tm_shape(ethiopia) + tm_polygons(border.col="black",lwd=2) +
  #tm_shape(oromia) + tm_polygons(col="darkgrey",border.col="black",lwd=2) + 
  tm_shape(boundary) + tm_dots(col="black",size=0.5) #+
  #tm_shape(cities) + tm_dots(size=0.5,col="black")

library(grid)
# print insets
map1
print(inset, vp=viewport(x= 0.75, y= 0.8, width= 0.3, height= 0.3))


tmap_save(map1,insets_tm=inset,insets_vp=viewport(x= 0.825, y= 0.825, width= 0.3, height= 0.3),
          filename=paste0(folder_names,ptemp,"/sampling_network.tiff"), dpi=600)
