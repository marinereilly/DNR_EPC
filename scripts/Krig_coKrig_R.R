#####Packages#####
library(tidyverse)
library(sf)
library(dadjoke)


#####Load Data#####
chesflux<-read.csv("CHESFLUX.csv")
chesflux$Month<-factor(chesflux$Month, levels= c("December","January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September", "October", "November"))
chesflux <- chesflux %>% 
  rename(PO_flux="PO4.Flux..umol.m2.hr.", DO_sat = "BW.DO..uM.", DO_mg = "BW.DO..mg.L.", Depth = "Water.Depth..m.")
  
  
chesflux_sf<-chesflux %>% 
  rename("x" = "Longitude.DD", "y"="Latitude.DD") %>% 
  st_as_sf(., coords = c("x", "y")) %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#####Exploring Colinearity#####

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


depth_reg<-lm(PO_flux~ Depth, data=chesflux)
ggplotRegression(depth_reg)

sal_reg<-lm(PO_flux~ BW.Sal, data=chesflux)
ggplotRegression(sal_reg)
DO_reg<-lm(PO_flux~ DO_sat, data=chesflux)
ggplotRegression(DO_reg)
DO_mg<-lm(PO_flux~DO_mg, data=chesflux)
ggplotRegression(DO_mg)
