#Figure3

#Paired boreal and Arctic species
#Red fox, arctic fox; Moose, muskox; salix lanata, Salix polaris; Vaccinium myrtillus, Cassiope tetragona

#a Range maps, #b temperature niches, #c trait distributions

if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install rasterSp from Github if not previously installed
if(!"rasterSp" %in% installed.packages()[,"Package"]) remotes::install_github("RS-eco/rasterSp", build_vignettes = T)

#Libraries
library(sf)
library(ggplot2)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(rgbif)
library(data.table)
library(geodata)

# #Biome maps -------------------------------------------------------------


#Load WWF biomes
#Citation: Olson, D. M., Dinerstein, E., Wikramanayake, E. D., Burgess, N. D., Powell, G. V. N., Underwood, E. C., D'Amico, J. A., Itoua, I., Strand, H. E., Morrison, J. C., Loucks, C. J., Allnutt, T. F., Ricketts, T. H., Kura, Y., Lamoreux, J. F., Wettengel, W. W., Hedao, P., Kassem, K. R. 2001. Terrestrial ecoregions of the world: a new map of life on Earth. Bioscience 51(11):933-938.
globalbiomes<-st_read("Figure3/data/BiomesWWF","wwf_terr_ecos")

#Biome 6 is boreal forest and 11 is tundra
borealforest<-st_union(globalbiomes[globalbiomes$BIOME==6,])
tundra<-st_union(globalbiomes[globalbiomes$BIOME==11 &(globalbiomes$REALM=="PA"|globalbiomes$REALM=="NA"), ])
borealtundra<-globalbiomes[(globalbiomes$BIOME==6 | globalbiomes$BIOME==11)& (globalbiomes$REALM=="PA"|globalbiomes$REALM=="NA"),]

bandt<-st_intersection(borealforest,tundra)

#World map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


#Set polar projection
projchoice<-"+proj=laea +lat_0=90 +lon_0=0 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "
borealtundra$BIOME<-as.factor(borealtundra$BIOME)

mycols<-c("darkolivegreen","darkgoldenrod")

ggplot()+geom_sf(data=borealtundra,aes(fill=BIOME),color=NA)+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))+
  scale_fill_manual(labels=c("Boreal forest","Arctic tundra"),"Biome",values=mycols)


# Range data --------------------------------------------------------------
#IUCN Range maps for vertebrates
#Downloaded from IUCN - citation in the folder

arcticfox<-st_read("Figure3/data/MammalRangeMaps/arcticfox","data_0")
muskox<-st_read("Figure3/data/MammalRangeMaps/muskox","data_0")
redfox<-st_read("Figure3/data/MammalRangeMaps/redfox","data_0")
moose<-st_read("Figure3/data/MammalRangeMaps/moose","data_0")

#Plot each pair, using alpha blending (probably can find some better colours)
ggplot()+#geom_sf(data=borealtundra,fill=NA,color="black")+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=moose,fill=mycols[1],color=NA,alpha=0.5)+geom_sf(data=muskox,fill=mycols[2],color=NA,alpha=0.5)+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))+
  scale_fill_manual(labels=c("Moose","Muskox"),"Species",values=mycols)

ggplot()+#geom_sf(data=borealtundra,aes(fill=BIOME),color=NA)+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=redfox,fill=mycols[1],color=NA,alpha=0.5)+geom_sf(data=arcticfox,fill=mycols[2],color=NA,alpha=0.5)+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))



#GBIF occurrence data for plants
#1980-2024(March). >45degN
#GBIF.org (18 March 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.5zh6uk
#Direct download
download.file("https://ntnu.box.com/shared/static/f81im02rb32enak2qsuqk4w6e6waj6tb.csv","Figure3/data/PlantOccRecs.csv")
#fread to read... 
plantoccdat<-fread("Figure3/data/PlantOccRecs.csv",header=T)
dim(plantoccdat)#should be 5 002 413 rows
head(plantoccdat)

#Make a spatial dataframe using coordinates
plantocc_sp<-st_as_sf(plantoccdat,coords=c("decimalLongitude","decimalLatitude"),crs=crs(globalbiomes))
#Filter out coordinates outside of boreal and arctic biomes
plantocc_sp_bt<-st_filter(plantocc_sp,borealtundra)
dim(plantocc_sp_bt)

ggplot()+geom_sf(data=borealtundra,aes(fill=BIOME))+ scale_fill_manual(labels=c("Boreal forest","Arctic tundra"),"Biome",values=mycols)+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=plantocc_sp_bt[plantocc_sp_bt$species %in% c("Salix lanata","Salix polaris"),],aes(color=species),alpha=0.5)+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))
 
ggplot()+#geom_sf(data=borealtundra,fill=NA,color="black")+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=plantocc_sp_bt[plantocc_sp_bt$species %in% c("Vaccinium myrtillus","Cassiope tetragona"),],aes(color=species),alpha=0.5)+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))



# Temperature niches ------------------------------------------------------

#Download temperature data
#Worldclim, MAT, MST
bioclim2.5<-worldclim_global(var='bio',res=2.5,path="Figure3/data")
