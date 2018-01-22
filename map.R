#####Load Packages#####
library(sf)
library(tidyverse)
library(dadjoke)
library(ggthemes)

#####Load Data to turn into shapefile for use in Arc#####
chesflux<-read.csv("CHESFLUX.csv")
chesflux$Month<-factor(chesflux$Month, levels= c("December","January", "February", "March", "April", 
                                                    "May", "June", "July", "August", "September", "October", "November"))
chesflux_sf<-chesflux %>% 
  rename(x = "Longitude.DD", y="Latitude.DD") %>% 
  st_as_sf(., coords = c("x", "y")) %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(st_geometry(chesflux_sf))
st_write(chesflux_sf, "chesflux.shp")

flux_sites<-chesflux %>% 
  select(site_id = "ID.", x = "Longitude.DD", y="Latitude.DD")
flux_sites_sf <-st_as_sf(flux_sites, coords = c("x", "y")) %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(st_geometry(flux_sites_sf))
st_write(flux_sites_sf, "fluxsites.shp")

summer_flux_sf<-b_flux_sub %>% 
  filter(.$Month=="July"|.$Month=="August"|.$Month=="September" )
plot(st_geometry(summer_flux_sf))
st_write(summer_flux_sf,"summer_flux2.shp")

#####Loading the shapefile created in ArcMap where points were selected by being within the AOI#####
b_flux<-st_read("chesflux.shp")
b_flux_sub<- b_flux %>% 
  filter(In_Bound=="Y") %>% 
  rename(Depth="Wt_D___")
b_flux_sub$Month<-factor(b_flux_sub$Month, levels= c("December","January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September", "October", "November"))
boundary<-st_read("boundary2.shp")


#####Frequency Distributions#####
a<-b_flux_sub %>%
  ggplot()
b<-a+geom_histogram(aes(Month), color = "black", fill = "steelblue3", stat = "count")+
  ggtitle("CHESFLUX Monthly Frequency Distribution")+
  theme_minimal()
b

c<-a+geom_histogram(aes(Depth), binwidth = 1, color = "black", fill = "steelblue3")+
  ggtitle("CHESFLUX Depth Frequency Distribution")+
  theme_minimal()
c

d<-a+geom_histogram(aes(Year), binwidth = 1, color = "black", fill ="steelblue3")+
  ggtitle("CHESFLUX Year Frequency Distribution")+
  scale_x_continuous(minor_breaks = seq(1980,2018, by=1))
d

e<-ggplot(flux_1998)+
  geom_histogram(aes(logP))
e

#####Map#####
flux_1996<-b_flux_sub %>% 
  filter(Year==1996)
flux_1998<-b_flux_sub %>% 
  filter(Year==1998)

e<-ggplot(b_flux_sub)+
  geom_sf(aes(color=b_flux_sub$Month, fill=b_flux_sub$Month))+
  theme_few()+scale_color_pander()+scale_fill_pander()+
  geom_sf(data = boundary, fill = NA)
e  

f<-ggplot()+
  geom_sf(data = boundary, fill = NA)+
  geom_sf(data=flux_1996, color="red")+
  geom_sf(data=flux_1998, color="blue")+theme_few()
f
