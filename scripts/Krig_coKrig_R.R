#####Packages#####
library(tidyverse)
library(sf)
library(sp)
library(gstat)
library(dadjoke)
library(lubridate)
library(nlme)

#####Load Data#####
###Full data


chesflux<-read.csv("C:/Users/Erin/Desktop/DNR_EPC/CHESFLUX.csv", stringsAsFactors=FALSE)

chesflux$Month<-factor(chesflux$Month, levels= c("December","January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September", "October", "November"))
chesflux <- chesflux %>% 
  rename(PO_flux="PO4.Flux..umol.m2.hr.", DO_sat = "BW.DO..uM.", DO_mg = "BW.DO..mg.L.", Depth = "Water.Depth..m.", Temp = "BW.Temp...C.")
  
  
chesflux_sf<-chesflux %>% 
  rename("x" = "Longitude.DD", "y"="Latitude.DD") %>% 
  st_as_sf(., coords = c("x", "y")) %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#chesflux_sf<-chesflux %>% 
#  select(ID., Longitude.DD, Latitude.DD) %>% 
#  full_join(., chesflux_sf) %>% 
#  rename(Longitude = "Longitude.DD", Latitude = "Latitude.DD")
###bounded data ## pulling shpaefiles into arc and back switched NA values to 0 
#so make sure you rerun code with newlycreated layers
boundary<-st_read("ARC_files/boundary2.shp")
pts<-st_transform(x = chesflux_sf, crs=st_crs(boundary))
b_flux_sub<-st_intersection(boundary, pts)
rm(pts, boundary)

b_flux_sub$Date <-ymd(b_flux_sub$Date)
b_flux_sub<-b_flux_sub %>% 
  select(-Shape_Leng, -Shape_Area)
b_flux_sub<-b_flux_sub %>% 
  filter(!PO_flux=="NaN")

###Subset 1998 data and summer data
flux_1998<-b_flux_sub %>%
  filter(Year==1998)
summer_flux<-b_flux_sub %>% 
  filter(Month %in% c("July", "August", "September"))

st_write(summer_flux, "ARC_files/summer_flux3.shp", driver = "ESRI Shapefile")

View(summer_flux)

summer_flux<-chesflux %>% 
  select(ID., Longitude.DD, Latitude.DD) %>% 
  right_join(., summer_flux)

not_summer_flux<-b_flux_sub %>% 
  filter(!(Month %in% c("July", "August", "September")))

#####Exploring Colinearity#####

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(names(fit$model)[2], "~", y = names(fit$model)[1], "     ",
      "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


depth_reg<-lm(PO_flux~ Depth, data=chesflux)
ggplotRegression(depth_reg)
ggsave("plots_images/depth_reg.png")
sal_reg<-lm(PO_flux~ BW.Sal, data=chesflux)
ggplotRegression(sal_reg)
ggsave("plots_images/sal_reg.png")
DO_reg<-lm(PO_flux~ DO_sat, data=chesflux)
ggplotRegression(DO_reg)
ggsave("plots_images/DOSat_reg.png")
DO_mg<-lm(PO_flux~DO_mg, data=chesflux)
ggplotRegression(DO_mg)
ggsave("plots_images/DOmg_reg.png")

upper_depth_reg<-lm(P_flux~ Depth, data=b_flux_sub)
ggplotRegression(upper_depth_reg)
ggsave("plots_images/upper_depth_reg.png")
upper_sal_reg<-lm(P_flux~ Salinity, data=b_flux_sub)
ggplotRegression(upper_sal_reg)
ggsave("plots_images/upper_sal_reg.png")
upper_DO_reg<-lm(P_flux~ DO_sat, data=b_flux_sub)
ggplotRegression(upper_DO_reg)
ggsave("plots_images/upper_DOSat_reg.png")
upper_DO_mg<-lm(P_flux~DO_mg, data=b_flux_sub)
ggplotRegression(upper_DO_mg)
ggsave("plots_images/upper_DOmg_reg.png")

######Working through AED stuff#####
####Run the AED functions Script so that you have all of the functions that you need####
###Run Linear Regressions for the Explanatory Variables
sum_do<-lm(PO_flux ~ DO_mg, data = summer_flux)
sum_depth<-lm(PO_flux ~ Depth, data= summer_flux)
sum_salinity<-lm(PO_flux ~ BW.Sal, data = summer_flux)
sum_temp<-lm(PO_flux ~ Temp, data = summer_flux)

summary(sum_do)
summary(sum_depth)
summary(sum_salinity)
summary(sum_temp)

#the summaries showed that DO, Depth, Salinity are 
#signigicant but Temperature is not.  None of the variables are highly
#significant, but they do have a p value less than 0.05

sum_do_resid<-resid(sum_do)
sum_depth_resid<-resid(sum_depth)
sum_salinity_resid<-resid(sum_salinity)
sum_temp_resid<-resid(sum_temp)

plot(summer_flux$DO_mg, sum_do_resid)
abline(0,0)

plot(summer_flux$Depth, sum_depth_resid)
abline(0,0)

plot(summer_flux$BW.Sal, sum_salinity_resid)
abline(0,0)

plot(summer_flux$Temp, sum_temp_resid)
abline(0,0)
#residuals seem to show no trends, now we are going to look at spatial trends with the residuals
# bubble plots should not show any spatial patterns
E<-rstandard(sum_do)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
bubble(mydata, "E", col = c("orange", "blue"), main = "Residuals DO", xlab = "X-coordinates", ylab = "Y-coordinates")

E<-rstandard(sum_depth)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
bubble(mydata, "E", col = c("orange", "blue"), main = "Residuals Depth", xlab = "X-coordinates", ylab = "Y-coordinates")

E<-rstandard(sum_salinity)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
bubble(mydata, "E", col = c("orange", "blue"), main = "Residuals Salinity", xlab = "X-coordinates", ylab = "Y-coordinates")

E<-rstandard(sum_temp)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
bubble(mydata, "E", col = c("orange", "blue"), main = "Residuals Temp", xlab = "X-coordinates", ylab = "Y-coordinates")

#Making variograms
E<-rstandard(sum_do)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
vario_do<-variogram(E~1, data=mydata)
plot(vario_do)

E<-rstandard(sum_depth)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
vario_depth<-variogram(E~1, data=mydata, alpha = c(45, 135, 225, 315))
plot(vario_depth)

E<-rstandard(sum_salinity)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
vario_salinity<-variogram(E~1, data=mydata, alpha = c(45, 135, 225, 315))
plot(vario_salinity)

E<-rstandard(sum_temp)
mydata<-data.frame(E, summer_flux$Longitude.DD, summer_flux$Latitude.DD)
coordinates(mydata)<-c("summer_flux.Longitude.DD", "summer_flux.Latitude.DD")
vario_temp<-variogram(E~1, data=mydata, alpha = c(45, 135, 225, 315))
plot(vario_temp)

#My data show anistropy, the buble plots seem to show clustering but the variograms don't show spatial corelation.  
#despite all of this I am going to keep following along because at the moment I don't get it and
#hopefully this will help me understand.  but I am very confused

f1<- formula(PO_flux ~ DO_mg)
DO1_gls<-gls(f1, data = summer_flux)
Vario_gls<-Variogram(DO1_gls, form =~ Longitude.DD + Latitude.DD,
                     robust = TRUE, maxDist = 2000, resType = "pearson")
plot(Vario_gls, smooth = TRUE)

DO1A <- gls(f1, correlation = corSpher(form =~ Longitude.DD + Latitude.DD,
                                      nugget = TRUE), data = summer_flux)
DO1B <- gls(f1, correlation = corLin(form =~ Longitude.DD + Latitude.DD,
                                      nugget = TRUE), data = summer_flux)
DO1C <- gls(f1, correlation = corRatio(form =~ Longitude.DD + Latitude.DD,
                                        nugget = TRUE), data = summer_flux)
DO1D <- gls(f1, correlation = corGaus(form =~ Longitude.DD + Latitude.DD,
                                       nugget = TRUE), data = summer_flux)
DO1E <- gls(f1, correlation = corExp(form =~ Longitude.DD + Latitude.DD,
                                      nugget = TRUE), data = summer_flux)
AIC(DO1, DO1A, DO1B, DO1C, DO1D, DO1E)
