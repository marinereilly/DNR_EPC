#####Packages#####
library(tidyverse)
library(sf)
library(sp)
library(gstat)
library(dadjoke)
library(lubridate)

#####Load Data#####
###Full data
chesflux<-read.csv("CHESFLUX.csv")
chesflux$Month<-factor(chesflux$Month, levels= c("December","January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September", "October", "November"))
chesflux <- chesflux %>% 
  rename(PO_flux="PO4.Flux..umol.m2.hr.", DO_sat = "BW.DO..uM.", DO_mg = "BW.DO..mg.L.", Depth = "Water.Depth..m.")
  
  
chesflux_sf<-chesflux %>% 
  rename("x" = "Longitude.DD", "y"="Latitude.DD") %>% 
  st_as_sf(., coords = c("x", "y")) %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
###bounded data ## pulling shpaefiles into arc and back switched NA values to 0 
#so make sure you rerun code with newlycreated layers
b_flux_sub %>% 
  rename(Depth="Wt_D___", Salinity="BW_Sal", DO_sat="BW_DO__M", DO_mg="BW_DO___", P_flux="PO4_F__")
b_flux_sub$Month<-factor(b_flux_sub$Month, 
                         levels= c("December","January", "February", "March", "April","May", "June", "July", 
                                   "August", "September", "October", "November"))
b_flux_sub$Date <-ymd(b_flux_sub$Date)
###Subset 1998 data
flux_1998<-b_flux_sub %>%
  filter(Year==1998)


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

#####Mapping homogeniety#####
depth_1998<-lm(P_flux~Depth,data=flux_1998)
summary(depth_1998)
sal_1998<-lm(P_flux~Salinity,data=flux_1998)
summary(sal_1998)
DO_1998<-lm(P_flux~DO_sat,data=flux_1998)
summary(DO_1998)

a<-rstandard(depth_reg)#####You are having issues here because there are 
#some NA points so the length of the residuals is different than the length of the data
mydata<-data.frame(a, chesflux$Longitude.DD, chesflux$Latitude.DD)
coordinates(mydata)<-c(chesflux.Longit)
bubble(flux_1998$geometry,"a",col = c("blue","orange"),
       main = "Residulas", xlab = "Xcoordinates", ylab = "ycoordinates")


Z<-b_flux_sub %>% 
  select(Date, Year, Depth, Salinity, BW_T___, DO_sat, DO_mg, P_flux, -geometry) 
st_geometry(Z) <- NULL

pairs(Z, lower.panel = panel.smooth2, upper.panel = panel.cor, diag.panel = panel.hist)

corvif(Z[,c(-1,-7)])


b<-lm(DO_sat ~ DO_mg, data = b_flux_sub)
a<-ggplotRegression(b)
a
#####Exploring for outliers#####

