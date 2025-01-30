#Figure3

#Paired boreal and Arctic species
#Red fox, arctic fox; Moose, muskox; salix lanata, Salix polaris; Vaccinium myrtillus, Cassiope tetragona

#a Range maps, #b temperature niches, #c trait distributions

rm(list=ls())
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
library(gridExtra)

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
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))+theme(legend.position='none')+
  scale_fill_manual(labels=c("Moose","Muskox"),"Species",values=mycols)

ggplot()+#geom_sf(data=borealtundra,aes(fill=BIOME),color=NA)+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=redfox,fill=mycols[1],color=NA,alpha=0.5)+geom_sf(data=arcticfox,fill=mycols[2],color=NA,alpha=0.5)+theme(legend.position='none')+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))



#GBIF occurrence data for plants
#1980-2024(March). >45degN
#GBIF.org (18 March 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.5zh6uk
#Direct download
#download.file("https://ntnu.box.com/shared/static/f81im02rb32enak2qsuqk4w6e6waj6tb.csv","Figure3/data/PlantOccRecs.csv")
#fread to read... 
plantoccdat<-fread("Figure3/data/PlantOccRecs.csv",header=T)
dim(plantoccdat)#should be 5 002 413 rows
head(plantoccdat)

#Make a spatial dataframe using coordinates
plantocc_sp<-st_as_sf(plantoccdat,coords=c("decimalLongitude","decimalLatitude"),crs=crs(globalbiomes))
#Filter out coordinates outside of boreal and arctic biomes
plantocc_sp_bt<-st_filter(plantocc_sp,borealtundra)
dim(plantocc_sp_bt)

SS_map<-ggplot()+#geom_sf(data=borealtundra,aes(fill=BIOME))+ scale_fill_manual(labels=c("Boreal forest","Arctic tundra"),"Biome",values=mycols)+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=plantocc_sp_bt[plantocc_sp_bt$species %in% c("Salix lanata","Salix polaris"),],aes(color=species),alpha=0.5,cex=0.5)+
  scale_color_manual(values=c("Salix polaris" =mycols[2],
                              "Salix lanata" = mycols[1]))+
  theme(legend.position = 'none')+
  labs(tag="b")+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))
 
CV_map<-ggplot()+#geom_sf(data=borealtundra,fill=NA,color="black")+
  geom_sf(data=world,fill=NA)+theme_bw()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),legend.position=c(0.2,0.9))+
  geom_sf(data=plantocc_sp_bt[plantocc_sp_bt$species %in% c("Vaccinium myrtillus","Cassiope tetragona"),],aes(color=species),alpha=0.5,cex=0.5)+
  scale_color_manual(values=c("Cassiope tetragona" =mycols[2],
                              "Vaccinium myrtillus" = mycols[1]))+
  theme(legend.position = 'none')+
  labs(tag="a")+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))



# Temperature niches ------------------------------------------------------

#Download temperature data
#Worldclim, MAT, MST
bioclim2.5<-worldclim_global(var='bio',res=2.5,path="Figure3/data")

#Mean Temperature of the Warmest Quarter
bio10 <- bioclim2.5$wc2.1_2.5m_bio_10
crs(bio10, proj = TRUE) #+proj=longlat +datum=WGS84 +no_defs
crs(plantocc_sp_bt, proj = TRUE) #+proj=longlat +datum=WGS84 +no_defs

# Crop and mask bioclims to biomes and reproject to polat projection
#reproject to polar
bio10_cropped_polar <- mask(crop(project(bio10, crs(borealtundra)),borealtundra),borealtundra)

# Plot Mean Temperature of the Warmest Quarter
ggplot() +
   theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.position=c(0.2,0.9))+
  geom_spatraster(data = bio10_cropped_polar) +
  scale_fill_continuous(na.value=NA)+
  geom_sf(data=world,fill=NA)+
  coord_sf(crs = projchoice,ylim=c(-703086, 7071423),xlim=c(-505347.4, 8526158))

## Extract temperature values for occurrences
#convert plant occurrences to SpatVector
plantocc_vect <- vect(plantocc_sp)

#create additional layer with unique ID cells
ID_raster <- bio10
values(ID_raster) <- 1:ncell(bio10)

#combine ID raster with bio10 raster
bio10_IDs <- c(bio10, ID_raster)

#change layer names
names(bio10_IDs) <- c("bio10", "cell_ID")

#check combined raster
print(bio10_IDs)

#extract raster values
temp_ID_occurrences <- terra::extract(bio10_IDs, plantocc_vect)

#add extracted values to spatial dataframe
plantocc_sp$bio10 <- temp_ID_occurrences[,2]
plantocc_sp$cell_ID <- temp_ID_occurrences[,3]

#keep unique rows for species and cell_ID
unique_plantocc_sp <- distinct(plantocc_sp, species, cell_ID,
                               .keep_all = TRUE)

# Plot frequency for C. tetragona & V. myrtillus
CV_temp<-unique_plantocc_sp |>
  filter(species %in% c("Vaccinium myrtillus","Cassiope tetragona")) |>
  ggplot(aes(x = bio10, group = species, fill = species))+
  geom_density(alpha = .4)+
  scale_fill_manual(breaks = c("Vaccinium myrtillus", "Cassiope tetragona"),
                    values = mycols, name = "Species")+
  xlab(expression("Mean temp. warmest quarter "~(degree*C)))+
  ylab("Density")+
  theme_classic()+
  theme(axis.title = element_text(size=14),axis.text=element_text(size=12))+
  labs(tag="c")+
  theme(legend.position = 'none')

# Plot frequency for S. lanata and S.polaris
SS_temp<-unique_plantocc_sp |>
  filter(species %in% c("Salix lanata","Salix polaris")) |>
  ggplot(aes(x = bio10, group = species, fill = species))+
  geom_density(alpha = .4)+
  scale_fill_manual(values = mycols, name = "Species")+
  xlab(expression("Mean temp. warmest quarter "~(degree*C)))+
  ylab("Density")+
  theme_classic()+
  theme(axis.title = element_text(size=14),axis.text=element_text(size=12))+
  labs(tag="d")+
  theme(legend.position = 'none')
  
# Traits ------------------------------------------------------


# Wrangling formatting problem data ---------------------------------------


# 
# # Read in data
# traits <- read.csv("Figure3/data/planttraitdata.csv",
#                    header = FALSE)
# 
# # Split columns
# traits_clean <- traits |>
#   separate(col = V1, 
#            into = c("number", "sp", "Dataset", "TraitValue", "Lat", "Lon",
#                     "DataContributor", "TraitName", "extra", "extra2"), 
#            sep = ",", convert = TRUE, extra = "warn")
# 
# # Remove first row - it is redundant
# traits_clean <- traits_clean |>
#   slice(-1)
# 
# # Inspect df
# str(traits_clean)
# glimpse(traits_clean) #TraitValue has both numbers and Names
# levels(as.factor(traits_clean$TraitValue)) #" Alaska\"", " Finland\"", " Leaf N and P", " Plant Height Database\"" - non-numeric values in the columns
# 
# # Copy Trait data column and reorder columns
# traits_clean_fix <- traits_clean |>
#   mutate(TraitValueClean = TraitValue) |>
#   select(1:4, 11, 5:10)
# 
# # Convert values to NA when they have the above value for TraitValue column
# traits_clean_fix <- traits_clean_fix |>
#   mutate(TraitValueClean = case_when(
#     str_detect(TraitValueClean, "a") ~ NA, 
#     TRUE ~ TraitValueClean
#   ))
# 
# # Check which rows have NA values for TraitsValueClean
# glimpse(traits_clean_fix)
# na_rows <- which(is.na(traits_clean_fix$TraitValueClean))
# 
# # Shift values in columns 6 to 11 one column to the left
# for(row in na_rows){
#   traits_clean_fix[row, 5:10] <- traits_clean_fix[row, 6:11]
# }
# 
# # Inspect df again
# glimpse(traits_clean_fix) #TraitValue has both numbers and Names
# levels(as.factor(traits_clean$TraitName)) #contributor names in the trait name column
# 
# # Copy contributor name column and reorder columns
# traits_clean_fix_2 <- traits_clean_fix |>
#   mutate(TraitNameClean = TraitName) |>
#   select(1:9, 12, 10:11)
# 
# # Convert values to NA when they have the above value for TraitName column
# traits_clean_fix_2 <- traits_clean_fix_2 |>
#   mutate(TraitNameClean = case_when(
#     str_detect(TraitNameClean, "SLA|PlantHeight|SeedMass") ~ TraitNameClean, 
#     TRUE ~ NA
#   ))
# 
# # Check which rows have NA values for TraitsValueClean
# glimpse(traits_clean_fix_2)
# na_rows_trait_name <- which(is.na(traits_clean_fix_2$TraitNameClean))
# 
# # Shift values in columns 6 to 10 one column to the left
# for(row in na_rows_trait_name){
#   traits_clean_fix_2[row, 10] <- traits_clean_fix_2[row, 11]
# }
# 
# # Check values in column "Traits Value Clean"
# levels(as.factor(traits_clean_fix_2$TraitNameClean)) #still some contributor names
# 
# # Convert values to NA when they have the above value for TraitName column
# traits_clean_fix_3 <- traits_clean_fix_2 |>
#   mutate(TraitNameClean = str_replace_all(TraitNameClean, "[\";]+", "")) |>
#   mutate(TraitNameClean = case_when(
#     str_detect(TraitNameClean, "SLA|PlantHeight|SeedMass") ~ TraitNameClean, 
#     TRUE ~ NA
#   ))
# 
# # Check which rows have NA values for TraitsValueClean
# glimpse(traits_clean_fix_3)
# na_rows_trait_name <- which(is.na(traits_clean_fix_3$TraitNameClean))
# 
# 
# # Shift values in columns 12 to column 10
# for(row in na_rows_trait_name){
#   traits_clean_fix_3[row, 10] <- traits_clean_fix_3[row, 12]
# }
# 
# # Remove "" from spm and trait name
# traits_clean_fix_4 <- traits_clean_fix_3 |>
#   mutate(sp = str_replace_all(sp, fixed("\""), ""),
#          TraitNameClean = str_replace_all(TraitNameClean, "[\";]+", ""))
# 
# # Save new plant trait df
# write.csv(traits_clean_fix_4,
#           "Figure3/data/cleaned_planttraitdata.csv")
# 


# #Figure -----------------------------------------------------------------


# Read in data
traits1 <- read.csv("Figure3/data/traits_for_james_fromxl.csv",
                   header = TRUE,sep=";",dec=',')
traits1$sp<-factor(traits1$sp,levels=c("Cassiope tetragona","Vaccinium myrtillus","Salix polaris","Salix lanata"))


SS_trait<-ggplot(data=traits1[traits1$TraitName=="PlantHeight" & traits1$sp %in% c("Salix polaris","Salix lanata"),], aes(x =TraitValue, fill = sp))+
  geom_density(alpha = .4)+
  theme_classic()+
  xlab("Height (m)")+ylab("Density")+
  theme(legend.position=c(0.7,0.9),legend.text=element_text(size=12),legend.title=element_text(size=12),
        axis.title = element_text(size=14),axis.text=element_text(size=12))+
  
  labs(tag="f")+
  scale_fill_manual("Species",values=c("Salix polaris" =mycols[2],
                                      "Salix lanata" = mycols[1]))+
  scale_x_continuous(trans='log10')
  
CV_trait<-ggplot(data=traits1[traits1$TraitName=="SLA" & traits1$sp %in% c("Cassiope tetragona","Vaccinium myrtillus"),], aes(x =TraitValue, fill = sp))+
  geom_density(alpha = .4)+
  theme_classic()+scale_y_continuous(limits=c(0,0.15))+
  xlab(expression("SLA"~(mm^2~mg^-1)))+ylab("Density")+
  theme(legend.position=c(0.7,0.9),legend.text=element_text(size=12),legend.title=element_text(size=12),
        axis.title = element_text(size=14),axis.text=element_text(size=12))+
  labs(tag="e")+
  scale_fill_manual("Species", values=c("Cassiope tetragona" =mycols[2],
                                      "Vaccinium myrtillus" = mycols[1]))
                  


#Plant species only fig
arrange1<-grid.arrange(CV_map,SS_map,CV_temp,SS_temp,CV_trait,SS_trait,ncol=2)
arrange1
ggsave("Figure3/BorealizationFig.png",arrange1,width=8,height=12,units="in")
