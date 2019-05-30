library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(grid)
library(gridExtra)
library(dplyr)
library(ggmap)

all_bats<-read.csv("all_sensors_all_calls.csv", stringsAsFactors = FALSE)
all_bats<-distinct(select(all_bats, c("sensor_id", "Lat", "Lon", "Habitat", "month", "date", "month_ch", "count_date")))

months_all<-data.frame(sensor_id = rep(1:15, each =12),month = c("January", "February", "March", "April", "May", "June", "July", 
                                              "August", "September", "October", "November", "December"))

all_habs<-unique(merge(months_all, all_bats[,c("sensor_id", "Lat", "Lon", "Habitat")], by = c("sensor_id")))

monthly_bats<-all_bats %>%
  group_by(month, sensor_id)%>%
  mutate(sum_month = sum(count_date))%>%
  select(sensor_id, Lat, Lon, Habitat, month, sum_month)%>%
  distinct()


monthly_bats<-merge(all_habs, monthly_bats, by = c("sensor_id", "month", "Lat", "Lon", "Habitat"), all = TRUE)  

monthly_bats$sum_month[is.na(monthly_bats$sum_month)]<-0


xy<-cbind(monthly_bats$Lon, monthly_bats$Lat)
xy<-unique(xy)
S<-SpatialPoints(xy)
lnd<-bbox(S)
buff<-0.0015

lnd[1]<-lnd[1]-buff
lnd[2]<-lnd[2]-buff
lnd[3]<-lnd[3]+buff
lnd[4]<-lnd[4]+buff

map.test.centroids <- data.frame(Lon = all_bats$Lon, Lat = all_bats$Lat, OBJECTID = all_bats$sensor_id)
map.test.centroids<-unique(map.test.centroids)


map.test <- ggmap(get_map(location = lnd))+
  geom_point(data=map.test.centroids, aes(x=Lon, y=Lat), size=2, alpha=6/10)+
  labs(x = "Longitude", y = "Latitude")

map.test

#get centroids

#Plot map
# map.test <- ggplot(kt_geom)+
#   geom_polygon(aes(long, lat, group=group), fill="white")+
#   coord_fixed()+
#   geom_path(color="gray48", mapping=aes(long, lat, group=group), size=0.2)+
#   geom_point(data=map.test.centroids, aes(x=x, y=y), size=2, alpha=6/10)
# 

set.seed(1)
geo_data <- data.frame(who=monthly_bats$sensor_id,
                       value=monthly_bats$sum_month,
                       id=monthly_bats$month,
                       hab = monthly_bats$Habitat)


geo_data$col<-NULL
geo_data$col[geo_data$hab == "Water"]<-"Blue"
geo_data$col[geo_data$hab == "Grassland"]<-"Brown"
geo_data$col[geo_data$hab == "Parkland"]<-"Grey"
geo_data$col[geo_data$hab == "Trees"]<-"Green"

bar.testplot_list <- 
  lapply(1:length(unique(geo_data$who)), function(i) { 
    gt_plot <- ggplotGrob(
      ggplot(geo_data[geo_data$who == i,])+
        geom_bar(aes( x= factor(id, levels = month.name),y = value,group=who, fill = id),
                 position='dodge',stat='identity', color = "black") +
        ylim(-1000, 368720)+
        labs(x = NULL, y = NULL) + 
        theme(legend.position = "none", rect = element_blank(),
              line = element_blank(), text = element_blank()) 
      #+  coord_polar() 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

bar_annotation_list <- lapply(1:length(unique(geo_data$who)), function(i) 
  inset(bar.testplot_list[[i]], 
                    xmin = map.test.centroids$Lon[i] - 0.0008,
                    xmax = map.test.centroids$Lon[i] + 0.0008,
                    ymin = map.test.centroids$Lat[i] - 0.0008,
                    ymax = map.test.centroids$Lat[i] + 0.0008) )

result_plot <- Reduce(`+`, bar_annotation_list, map.test)

result_plot


png("histo_bar.png", width = 4, height = 8, units = 'in', res = 750)
result_plot# Make plot
dev.off()
