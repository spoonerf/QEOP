library(glmmTMB)
library(bbmle)
library(DHARMa)
library(dplyr)
library(lubridate)
library(suncalc)
library(ggplot2)

bats_cc<-read.csv("hourly_bats_complete_cases_for_models.csv")
colnames(bats_cc)<-c("count", "time_hour", "date", "deployment_id", "Habitat", 
                     "event_bin", "temp", "humid", "wind_speed", 
                     "wind_gust", "precip", "dist_roost")

bats_cc$deployment_id<-as.factor(bats_cc$deployment_id)
bats_cc$month<-as.factor(month(bats_cc$time_hour))

#Sensors 1, 6 and 11 all have a false of rate of >20% so we excluded these

bats_cc2<-bats_cc %>%
  filter(deployment_id != "1.1" & deployment_id != "6.1" & deployment_id != "11.1") %>% #high rates of false positives
  filter(deployment_id != "3.1" & deployment_id != "4.2" & deployment_id != "5.1"& deployment_id != "15.1")   #no record of false positive/false negative rates

bats_cc2$time_hour<-as.POSIXct(bats_cc2$time_hour)


bats_cc2 %>%
  group_by(time_hour)%>%
  filter(. <= time_hour - 24*60*60)


sum_month<-bats_cc2%>%
  group_by(month, deployment_id)%>%
  summarise(sum_month = sum(count))

ggplot(sum_month, aes(x = month, y = log10(sum_month)))+
  geom_bar(stat = "identity")+
  facet_wrap(~deployment_id)
  


ggplot(bats_cc2, aes(x =date, y = log10(count)))+
  geom_bar(stat = "identity")+
  facet_wrap(~deployment_id)


sum_day<-bats_cc2%>%
  group_by(date, deployment_id)%>%
  summarise(sum_dat = sum(count))


bats_cc2$hour<-hour(bats_cc2$time_hour)
bats_cc2$date<-as.Date(bats_cc2$date)


sunset<-getSunlightTimes(date = as.Date(bats_cc2$date),lat = 51.543523, lon = -0.016214 ,keep = "sunset", tz = "UTC")

bat_sun<-merge(bats_cc2,unique(sunset[,c("date", "sunset")]), by = "date")

bat_sun$time_since_ss<-as.numeric(as.POSIXct(bat_sun$time_hour) - bat_sun$sunset)

bat_sun$time_since_ss<-ifelse(bat_sun$time_since_ss < -43200,  bat_sun$time_since_ss +86400, bat_sun$time_since_ss)

hist(bat_sun$time_since_ss[bat_sun$count >10])

## Do we want to control for distance from the roost? 
## Seems to be more zero inflated in areas that are further from the roost

#dir.create(here::here("/hourly_models/"))


fit_zipoisson <- glmmTMB(count~ Habitat+ event_bin+ temp+
                        wind_gust + 
                        precip+ (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                         data=bats_cc2,
                         ziformula=~1,
                         family=poisson)

#saveRDS(fit_zipoisson, here::here("hourly_models/zi_poisson.RDA"))


fit_zinbinom1 <- glmmTMB(count~ Habitat+ event_bin+ temp+
                         wind_gust +
                         precip+ (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                         data=bats_cc2,
                         ziformula=~1,
                         family=nbinom1)

saveRDS(fit_zinbinom1, here::here("hourly_models/zin_binom1.RDA"))


fit_zinbinom2 <- glmmTMB(count~ Habitat+ event_bin+ temp+
                         wind_gust +
                         precip+ (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                         data=bats_cc2,
                         ziformula=~1,
                         family=nbinom2)

saveRDS(fit_zinbinom2, here::here("hourly_models/zin_binom2.RDA"))

fit_hnbinom1 <- glmmTMB(count~ Habitat+ event_bin+ temp+
                        wind_gust +
                        precip+ (1|deployment_id), 
                        data=bats_cc2,
                        ziformula=~1,
                        family=truncated_nbinom1)

saveRDS(fit_hnbinom1, here::here("hourly_models/zin_hnbinom1.RDA"))

##Doesn't converge

fit_hnbinom2 <- glmmTMB(count~ Habitat+ event_bin+ temp+
                        wind_gust +
                        precip+ (1|deployment_id), 
                        data=bats_cc2,
                        ziformula=~1,
                        family=truncated_nbinom2)

saveRDS(fit_hnbinom2, here::here("hourly_models/zin_hnbinom2.RDA"))

fit_zipoisson_dist <- glmmTMB(count~ Habitat+ event_bin+ temp+
                         wind_gust + 
                         precip+  (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                         data=bats_cc2,
                         ziformula=~dist_roost,
                         family=poisson)

saveRDS(fit_zipoisson_dist, here::here("hourly_models/zin_poisson_dist.RDA"))

fit_zinbinom1_dist <- glmmTMB(count~ Habitat+ event_bin+ temp+
                         wind_gust +
                         precip+ (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                         data=bats_cc2,
                         ziformula=~dist_roost,
                         family=nbinom1)

saveRDS(fit_zinbinom1_dist, here::here("hourly_models/zin_binom1_dist.RDA"))

fit_zinbinom2_dist <- glmmTMB(count~ Habitat+ event_bin+ temp+
                         wind_gust +
                         precip+ (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                         data=bats_cc2,
                         ziformula=~dist_roost,
                         family=nbinom2)

saveRDS(fit_zinbinom2_dist, here::here("hourly_models/zin_binom2_dist.RDA"))

fit_hnbinom1_dist <- glmmTMB(count~ Habitat+ event_bin+ temp+
                        wind_gust +
                        precip+ (1|deployment_id), 
                        data=bats_cc2,
                        ziformula=~dist_roost,
                        family=truncated_nbinom1)

saveRDS(fit_hnbinom1_dist, here::here("hourly_models/zin_hnbinom1_dist.RDA"))

###Doesn't converge

fit_hnbinom2_dist <- glmmTMB(count~ Habitat+ event_bin+ temp+
                        wind_gust +
                        precip+ (1|deployment_id), 
                        data=bats_cc2,
                        ziformula=~dist_roost,
                        family=truncated_nbinom2)

saveRDS(fit_hnbinom2_dist, here::here("hourly_models/zin_binom2_dist.RDA"))


fit_zinbinom1_dist_int_mo <- glmmTMB(count~ Habitat+ event_bin+ temp+
                                   wind_gust +
                                   precip+(1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                                   data=bats_cc2,
                                   ziformula=~dist_roost+month,
                                   family=nbinom1)

saveRDS(fit_zinbinom1_dist_int_mo, here::here("hourly_models/zin_binom1_dist_int_mo.RDA"))



bats_cc2$ID<-1:nrow(bats_cc2)


fit_olre <- glmmTMB(count~ Habitat+ event_bin+ temp+
                wind_gust +
                precip + (1|deployment_id)+
                (1|ID),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                data=bats_cc2,
                family=poisson)

#doesn't converge
saveRDS(fit_olre, here::here("hourly_models/zin_olre.RDA"))



AICtab(fit_zipoisson,fit_zinbinom1,fit_zinbinom2, fit_zipoisson_dist,fit_zinbinom1_dist,
       fit_zinbinom2_dist,fit_zinbinom1_dist_int_mo)

m0 <- update(fit_zinbinom1_dist_int_mo, . ~ 1)


expCoef <- exp(coef(fit_zinbinom1_dist_int_mo))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(hurdlePart))
colnames(expCoef) <- c("Count_model","Zero_inflation_model")
expCoef


# fit_hbinom1
#fit_hbinom2,
# fit_hbinom1_dist,
#, fit_hbinom2_dist



fit_zinbinom2_disp <- glmmTMB(count~ Habitat+ event_bin+ temp+
                              humid + wind_gust +
                              precip + (1|month) + 
                              (1|deployment_id),   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                              #data=bats_cc2,
                              data=sample_n(bats_cc, 10000, replace = FALSE),
                              ziformula=~dist_roost,
                              family=nbinom2)


bats_cc$ID<-1:nrow(bats_cc)


fit_olre <- glmmTMB(count~ Habitat+ event_bin+ temp+
                                wind_gust +
                                precip + (1|month) + 
                                (1|deployment_id)+   #have removed sensor no as there are no sensors with more than one deployment (that we are including)
                                (1|ID), data = bats_cc, family=poisson)






summary(fit_zinbinom2_disp)


###need to understand the output of these models better! 


res <- simulateResiduals(fit_zinbinom)



newdata = bats_cc
newdata$sensor_no <-NA
newdata$deployment_id <-NA

pred = predict(fit_zinbinom, new_data = new_data)

plotResiduals(pred, res$scaledResiduals)



###need to look at lagged impacts of events

#Try running models with hourly data - need to think about how we would capture events with this.

###Try running models with MCMCglmm too? 



