library(tidyverse)
library(tigris)
library(sf)
library(cartogram)
library(tmap)
library(tmaptools)



#read in the data file
df<-read_csv("honeyproduction.csv") %>%
  filter(year=="2012")

#from the tigris library get states shapes
us_sp<-states()

#using tmaptools, append the bee data to the shapefile key.shp is from the
#shapefile and is the two-letter abbreviation for us states key.data is
#the "state" column from the df
beemap <- tmaptools::append_data(us_sp, df, key.shp = "STUSPS", key.data = "state")

#calculate the cartogram for the beemap.  This takes a little while so I saved the RDS for later use.
beecarto<-cartogram(beemap, "totalprod")
saveRDS(beecarto, "beecarto")
beecarto<-readRDS("beecarto")

#tmap allows for a quick map of any shapefile.  This differs from ggplot,
# which requires a dataframe, usually created by fortify(<the spatial data
# frame>). After fortifying the beecarto data, I couldn't figure out how to
#  join the df data back in so I stayed with tmap.
#
tm_shape(beecarto)+
  tm_fill("totalprod", style="jenks")+
  tm_borders()+tm_layout(frame=T)
