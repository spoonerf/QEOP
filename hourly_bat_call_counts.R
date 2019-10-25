library(lubridate)

all_sens_info<-read.csv("all_bat_calls.csv")

all_sens_info$timestamp<-as.POSIXct(all_sens_info$timestamp)

all_sens_info$uni_hr<-round_date(all_sens_info$timestamp, unit = "hour")

all_sens_info$deployment<-1


#changing the deployment of all sensor four records after 14th May 2018 to deploment 2
#Update all deployments here
#When did 5 move to Duncan's garden??
#6 might've moved?
#8 has or is due to be removed due to building work - is likely to be in the bin.

all_sens_info[as.Date(as.character(all_sens_info$date)) > as.Date(as.character("2018-05-14")) & all_sens_info$sensor_id == 4,]<-all_sens_info %>%
  filter(as.Date(as.character(date)) > as.Date("2018-05-14") & sensor_id == 4)%>%
  mutate(deployment=replace(deployment, deployment==1, 2), Lat = replace(Lat, Lat==51.5367, 51.536553), Lon = replace(Lon, Lon==-0.0128, -0.013169))

# all_sens_info %>%
# filter(sensor_id == 4 & date >= as.Date("2018-05-14"))

all_sens_info[as.Date(as.character(all_sens_info$date)) > as.Date("2018-08-07") & all_sens_info$sensor_id == 5,]<-all_sens_info %>%
  filter(as.Date(as.character(date)) > as.Date("2018-08-07") & sensor_id == 5)%>%
  mutate(deployment=replace(deployment, deployment==1, 2), Lat = replace(Lat, Lat==51.5362, 51.590616), Lon = replace(Lon, Lon==-0.0127, -0.0139243), Habitat = replace(Habitat, Habitat == "Grassland", "Duncan's Garden" ))


all_sens_info$deployment_id<-paste(all_sens_info$sensor_id, all_sens_info$deployment, sep = ".")


###need to add in deployment_id


hourly_data<-all_sens_info %>%
  filter(deployment_id != "4.1" & deployment_id != "5.2")%>%
  group_by(deployment_id, Habitat, uni_hr) %>%
  summarise(hourly_count = n())%>%
  arrange(-hourly_count)

##adding in the hours where there were no bat calls

all_hours<-seq(from = min(hourly_data$uni_hr), to = max(hourly_data$uni_hr), by = "hour")

all_deps<-rep(unique(hourly_data$deployment_id), each = length(all_hours))

habs<-unique(hourly_data[,c("deployment_id", "Habitat")])

all_df<-data.frame(all_hours, all_deps,hourly_count = 0)


all_hours_df<-merge(all_df, hourly_data, by.x = c("all_deps","all_hours"), by.y = c("deployment_id", "uni_hr"), all=TRUE)


all_hours_df<-merge(all_hours_df, habs, by.x = "all_deps", by.y = "deployment_id")

all_hours_df$hourly_count.y[is.na(all_hours_df$hourly_count.y)]<-0

df<-dplyr::select(all_hours_df, c(all_deps, all_hours, hourly_count.y, Habitat.y))
colnames(df)<-c("deployment_id", "time_hour", "count", "Habitat")



break_finder<-function(x){
  nl_out<-rep(x, each = x)
}

all_sensors_df<-NULL

for (i in unique(df$deployment_id)){
  
  test<-df[df$deployment_id == i, ]
  test<-arrange(test, time_hour)
  rl<-rle(test$count == 0)  
  bf_out<-sapply(rl$lengths, break_finder)
  test_na_run<-unlist(bf_out)
  #test_na_run[!is.na(test$count)]<-NA
  test$hours_inactive<-test_na_run
  all_sensors_df<-rbind(all_sensors_df,test)
  #print(i)
  
}

sensor_down<-which(all_sensors_df$hours_inactive >= 168)


all_sensors_df$active<-1

all_sensors_df$active[sensor_down]<-0



###getting rid of dates where the sensors were thought not to be working


ggplot(all_sensors_df, aes(log10(count), group = deployment_id))+
  geom_histogram(bins = 100)+
  facet_wrap(.~deployment_id)


all_sensors_df$month<-month(all_sensors_df$time_hour)

ggplot(all_sensors_df, aes(log10(count), group = month))+
  geom_histogram(bins = 100)+
  facet_wrap(.~month)


all_sensors_df %>%
  group_by(deployment_id)%>%
  summarize(mean_count = mean(count), sd_count = sd(count), ratio_s_m = sd_count/mean_count)%>%
  arrange(ratio_s_m)


all_sensors_df %>%
  group_by(month)%>%
  summarize(mean_count = mean(count), sd_count = sd(count), ratio_s_m = sd_count/mean_count)%>%
  arrange(ratio_s_m)



head(arrange(all_sensors_df,-count))



#saveRDS(all_sensors_df, "hourly_bat_counts.RDS")














