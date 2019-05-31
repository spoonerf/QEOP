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

all_bats<-readRDS("all_sensor_deployments.RDS")

colnames(all_bats)[c(1,3)]<-c("sensor_id", "count_date")

#all_bats<-read.csv("all_sensors_all_calls.csv", stringsAsFactors = FALSE)
all_bats<-distinct(select(all_bats, c("sensor_id", "Lat", "Lon", "Habitat", "month", "date", "count_date", "deployment_id")))
all_bats$count_date[is.na(all_bats$count_date)]<-0


months_all<-data.frame(deployment_id = rep(unique(all_bats$deployment_id), each =12),month = c("January", "February", "March", "April", "May", "June", "July", 
                                              "August", "September", "October", "November", "December"))

all_habs<-unique(merge(months_all, all_bats[,c("deployment_id", "Lat", "Lon", "Habitat")], by = c("deployment_id")))

all_bats$deployment_id<-as.character(all_bats$deployment_id)
all_habs$deployment_id<-as.character(all_habs$deployment_id)

all_habs<-all_habs%>%
  filter(deployment_id != "5.2")

monthly_bats<-all_bats %>%
  group_by(month, deployment_id)%>%
  mutate(sum_month = sum(count_date))%>%
  select(deployment_id, Lat, Lon, Habitat, month, sum_month)%>%
  filter(deployment_id != "5.2")%>%
  distinct()


monthly_bats<-merge(all_habs, monthly_bats, by = c("deployment_id", "month", "Lat", "Lon", "Habitat"), all = TRUE)  

monthly_bats$sum_month[is.na(monthly_bats$sum_month)]<-0


xy<-cbind(monthly_bats$Lon, monthly_bats$Lat)
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

map.test.centroids <- data.frame(Lon = all_bats$Lon, Lat = all_bats$Lat, OBJECTID = all_bats$deployment_id)
map.test.centroids<-unique(map.test.centroids)


map.test <- ggmap(get_map(location = lnd, source = "stamen", maptype = "toner", color = "bw", force = TRUE))+
# geom_point(data=map.test.centroids, aes(x=Lon, y=Lat), size=2, alpha=6/10)+
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

geo_data <- data.frame(who=monthly_bats$deployment_id,
                       value=monthly_bats$sum_month,
                       id=monthly_bats$month,
                       hab = monthly_bats$Habitat)

geo_data$col<-NULL
geo_data$col[geo_data$id == "January" | geo_data$id == "February" | geo_data$id == "March"]<-"First"
geo_data$col[geo_data$id == "April" | geo_data$id == "May" | geo_data$id == "June"]<-"Second"
geo_data$col[geo_data$id == "July" | geo_data$id == "August"| geo_data$id == "September"]<-"Third"
geo_data$col[geo_data$id == "October" | geo_data$id == "November" | geo_data$id == "December"]<-"Fourth"

bar.testplot_list <- 
  lapply(unique(geo_data$who), function(i) { 
    gt_plot <- ggplotGrob(
      ggplot(geo_data[geo_data$who == i,])+
        geom_bar(aes( x= factor(id, levels = month.name),y = log10(value+1),group=as.factor(who), fill = col),
                 position='dodge',stat='identity', colour = "black") +
        scale_fill_poke(pokemon = 71, spread = 4)+
        labs(x = NULL, y = NULL) + 
        theme(legend.position = "none", rect = element_blank(),
              line = element_blank(), text = element_blank())+
       ylim(-0.5, 10) 
      #+ coord_polar() 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

bar_annotation_list <- lapply(1:length(unique(geo_data$who)), function(i) 
  inset(bar.testplot_list[[i]], 
                    xmin = map.test.centroids$Lon[i] - 0.00075,
                    xmax = map.test.centroids$Lon[i] + 0.00075,
                    ymin = map.test.centroids$Lat[i] - 0.00075,
                    ymax = map.test.centroids$Lat[i] + 0.00075) )

result_plot <- Reduce(`+`, bar_annotation_list, map.test)

result_plot


png("histo_bar.png", width = 4, height = 8, units = 'in', res = 750)
result_plot# Make plot
dev.off()
