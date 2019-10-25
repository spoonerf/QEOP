library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(grid)
library(gridExtra)
library(dplyr)
library(ggmap)
library(lubridate)
library(palettetown)
library(raster)
library(here)
library(sf)


all_bats<-readRDS("all_sensor_deployments.RDS")

all_bats<-readRDS("hourly_bats_complete_cases_for_models_sun_subs_lag_clim.RDS")

all_bats$month<-month(all_bats$time_hour)
all_bats$hour<-hour(all_bats$time_hour)

all_bats<-all_bats %>%
          filter(deployment_id != "1.1" & deployment_id != "6.1" & deployment_id != "11.1") %>% #high rates of false positives
          filter(deployment_id != "3.1" & deployment_id != "4.2" & deployment_id != "5.1"& deployment_id != "15.1")   #no record of false positive/false negative rates


#all_bats<-read.csv("all_sensors_all_calls.csv", stringsAsFactors = FALSE)
all_bats<- all_bats%>%
  group_by(deployment_id, month, lat, lon, Habitat)%>%
  summarise(sum_month = sum(count))%>%
  ungroup()


months_all<-data.frame(deployment_id = rep(unique(all_bats$deployment_id), each =12),month = 1:12)

all_habs<-unique(merge(months_all, all_bats[,c("deployment_id", "lat", "lon", "Habitat")], by = c("deployment_id")))

all_bats$deployment_id<-as.character(all_bats$deployment_id)
all_habs$deployment_id<-as.character(all_habs$deployment_id)

all_habs<-all_habs%>%
  filter(deployment_id != "5.2")

# monthly_bats<-all_bats %>%
#   group_by(month, deployment_id)%>%
#   mutate(sum_month = sum(count_date))%>%
#   dplyr::select(deployment_id, Lat, Lon, Habitat, month, sum_month)%>%
#   filter(deployment_id != "5.2")%>%
#   distinct()


monthly_bats<-merge(all_habs, all_bats, by = c("deployment_id", "month", "lat", "lon", "Habitat"), all = TRUE)  

monthly_bats$sum_month[is.na(monthly_bats$sum_month)]<-0


####average daily count when active in each month

xy<-cbind(monthly_bats$lon, monthly_bats$lat)
xy<-unique(xy)
S<-SpatialPoints(xy)
proj4string(S)<-CRS("+init=epsg:4326")

# BNG = CRS("+init=epsg:27700")
# S_BNG<-spTransform(S, BNG)
# BNG_df<-data.frame(unique(monthly_bats$deployment_id), S_BNG)
# write.csv(BNG_df, "BNG_bat_deployments.csv", row.names = FALSE)

lnd<-bbox(S)
buff<-0.0016

lnd[1]<-lnd[1]-buff
lnd[2]<-lnd[2]-buff
lnd[3]<-lnd[3]+buff
lnd[4]<-lnd[4]+buff

####habitat
qeop_vec<-readOGR(dsn ="D:/Fiona/QEOP/Habitat_Data/HabitatMaps",layer = "QEOP_Habitat_Diss")
qeop_sf<-st_as_sf(qeop_vec)

qeop_sf_wgs<-st_transform(qeop_sf, "+init=epsg:4326")

alpha_val<-0.5

qeop<-raster(here::here("Habitat_Data/HabitatMaps/QEOP_Habitat_10cm.tif"))

proj4string(qeop)

wgs = CRS("+init=epsg:4326")

#qeop_wgs<-spTransform(qeop,wgs)
#qeop_wgs<-projectRaster(qeop, crs = "+init=epsg:4326")

#writeRaster(qeop_wgs, here::here("Habitat_Data/HabitatMaps/QEOP_Habitat_10cm_WGS84.tif"))
qeop_wgs<-raster(here::here("Habitat_Data/HabitatMaps/QEOP_Habitat_10cm_WGS84.tif"))

qeop_wgs_crop<-crop(qeop_wgs, lnd)
qeop_wgs_crop[qeop_wgs_crop ==2]<-NA  #removing the habitats we don't want - temporary landscapes and living roofs

test_spdf <- as(qeop_wgs_crop, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")


#qeop_wgs_crop_pol <- rasterToPolygons(qeop_wgs_crop)

####
cols <- c("Grassland" = alpha("darkolivegreen3", alpha_val), "Water" = alpha("deepskyblue3", alpha_val), "Parkland" = alpha("darkkhaki", alpha_val), "Trees" = alpha("forestgreen", alpha_val))
cols <- c("Grassland" = "darkolivegreen3", "Water" = "deepskyblue3", "Parkland" = "darkkhaki", "Trees" = "forestgreen")

map.test.centroids <- data.frame(Lon = all_bats$lon, Lat = all_bats$lat, OBJECTID = all_bats$deployment_id)
map.test.centroids<-unique(map.test.centroids)

map.test.centroids<-merge(unique(all_bats[,c("Habitat", "deployment_id")]), map.test.centroids, by.x = "deployment_id", by.y= "OBJECTID")


map.test <- ggmap(get_map(location = lnd, source = "stamen", maptype = "toner-lite", color = "bw"), force = TRUE)+
  labs(x = "Longitude", y = "Latitude", fill = "Habitat")+
  geom_sf(data = qeop_sf_wgs, aes(fill = QEOP_Hab),colour = NA, inherit.aes = FALSE)+
  scale_fill_manual(values = cols)+
  geom_point(data =map.test.centroids, aes(x = Lon, y= Lat, col = Habitat), size = 3)+
  scale_color_manual(values = cols)+
  theme(legend.title=element_text(size=16), 
        legend.text=element_text(size=14),
        text = element_text(size=14), 
        axis.text.x  = element_text(size=12),
        axis.text.y = element_text(size=12) )

map.test


geo_data <- data.frame(who=monthly_bats$deployment_id,
                       value=monthly_bats$sum_month,
                       id=monthly_bats$month,
                       hab = monthly_bats$Habitat)

geo_data$who<-as.numeric(as.character(geo_data$who))
geo_data <- geo_data[order(geo_data$who),] 

map.test.centroids$deployment_id<-as.numeric(map.test.centroids$deployment_id)
map.test.centroids <- map.test.centroids[order(map.test.centroids$deployment_id),] 

# geo_data<-geo_data[geo_data$who != 4.1,]
# map.test.centroids<-map.test.centroids[map.test.centroids$OBJECTID != 4.1,]


geo_data$col<-NULL
geo_data$col[geo_data$id == 1| geo_data$id == 2 | geo_data$id == 3]<-"First"
geo_data$col[geo_data$id == 4 | geo_data$id == 5 | geo_data$id == 6]<-"Second"
geo_data$col[geo_data$id == 7 | geo_data$id == 8| geo_data$id == 9]<-"Third"
geo_data$col[geo_data$id == 10| geo_data$id == 11 |geo_data$id == 12]<-"Fourth"

bar.testplot_list <- 
  lapply(unique(geo_data$who), function(i) { 
    gt_plot <- ggplotGrob(
      ggplot(geo_data[geo_data$who == i,])+
        geom_bar(aes( x= factor(id, levels = month.name),y = log10(value+1),group=as.factor(who), fill = col),
                 position='dodge',stat='identity', colour = "black") +
        scale_fill_poke(pokemon = 250, spread = 4)+
        labs(x = NULL, y = NULL) + 
        theme(legend.position = "none", rect = element_blank(),
              line = element_blank(), text = element_blank())+
        ylim(-0.4,  4) + 
        coord_polar() 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

bar_annotation_list <- lapply(1:length(unique(geo_data$who)), function(i) 
  inset(bar.testplot_list[[i]], 
        xmin = map.test.centroids$Lon[i] - 0.002,
        xmax = map.test.centroids$Lon[i] + 0.002,
        ymin = map.test.centroids$Lat[i] - 0.002,
        ymax = map.test.centroids$Lat[i] + 0.002) )

result_plot <- Reduce(`+`, bar_annotation_list, map.test)

result_plot

png("histo_polar_bar.png", width = 8, height = 12, units = 'in', res = 750)
result_plot# Make plot
dev.off()
