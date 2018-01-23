#####Packages#####
library(tidyverse)
library(sf)
library(gstat)
library(dadjoke)

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
###bounded data
b_flux<-st_read("ARC_files/chesflux.shp")
b_flux_sub<- b_flux %>%
  filter(In_Bound=="Y") %>%
  rename(Depth="Wt_D___", Salinity="BW_Sal", DO_sat="BW_DO__M", DO_mg="BW_DO___", P_flux="PO4_F__")
b_flux_sub$Month<-factor(b_flux_sub$Month, 
                         levels= c("December","January", "February", "March", "April","May", "June", "July", 
                                   "August", "September", "October", "November"))

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

#####Mapping homogeniety#####
summary(depth_reg)
summary()