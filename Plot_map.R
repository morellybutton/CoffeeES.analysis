#Code to produce figure of sampling design
library(cartography)
library(sf)
#library(stars)
library(tmap)

#open shapefiles of patch areas
patches<-st_read("/users/alex/Documents/Research/Africa/ECOLIMITS/GIS/shapefiles/yayu/newstrata_ll.shp")
ndvi<-raster::raster("/users/alex/Documents/Research/Africa/ECOLIMITS/GIS/imagery/yayu/YayuNDVI.tif")
plot(ndvi,pal)

plot(st_geometry(patches),add=T)
typoLayer(x = patches, var = "Label",
                 legend.title.txt = "Patch Size/Elevation\nClasses",
          col = c("aquamarine4", "yellow3","wheat","red","orange","purple"))

pts<-st_read("/users/alex/Documents/Research/Africa/ECOLIMITS/GIS/shapefiles/yayu/final_points_v5.shp")

bx<-st_bbox(patches)

map<-tm_shape(ndvi,bbox=bx) + tm_raster(style = "cont", palette = "YlGn", legend.show = F,alpha=0.7) +
  tm_shape(patches) + tm_polygons(col="Label",title = "Patch Size/\nElevation Classes") + tm_borders() + 
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar( size = 1.0,position=c("right","bottom")) + tm_shape(pts) + tm_dots(size=0.1) +
  tm_layout(legend.text.size = 1.0,legend.title.size=1.25,
    legend.position = c("left","top"))
tmap_save(map,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/FigureS1.pdf",
          width=7,height=5)
