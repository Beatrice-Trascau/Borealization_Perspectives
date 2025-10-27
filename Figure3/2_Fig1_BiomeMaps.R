#Figure1

#Map of Boreal and Arctic Biomes

#Libraries
library(here)
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

#Load WWF biomes
#Citation: Olson, D. M., Dinerstein, E., Wikramanayake, E. D., Burgess, N. D., Powell, G. V. N., Underwood, E. C., D'Amico, J. A., Itoua, I., Strand, H. E., Morrison, J. C., Loucks, C. J., Allnutt, T. F., Ricketts, T. H., Kura, Y., Lamoreux, J. F., Wettengel, W. W., Hedao, P., Kassem, K. R. 2001. Terrestrial ecoregions of the world: a new map of life on Earth. Bioscience 51(11):933-938.
globalbiomes<-st_read("Figure3/data/BiomesWWF","wwf_terr_ecos")

#Biome 6 is boreal forest and 11 is tundra
borealforest<-st_union(globalbiomes[globalbiomes$BIOME==6,])
tundra<-st_union(globalbiomes[globalbiomes$BIOME==11 &(globalbiomes$REALM=="PA"|globalbiomes$REALM=="NA"), ])
borealtundra<-globalbiomes[(globalbiomes$BIOME==6 | globalbiomes$BIOME==11)& (globalbiomes$REALM=="PA"|globalbiomes$REALM=="NA"),]

# Make sure boreal_forest and tundra are valid geometries
boreal_forest <- st_make_valid(borealforest)
tundra <- st_make_valid(tundra)

# Check CRS
biome_crs <- st_crs(boreal_forest)

# Get world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define polar projection
proj_choice<-"+proj=laea +lat_0=90 +lon_0=0 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "

# Transform all spatial objects to the polar projection
world_polar <- st_transform(world, crs = proj_choice)
boreal_forest_polar <- st_transform(boreal_forest, crs = proj_choice)
tundra_polar <- st_transform(tundra, crs = proj_choice)

# Define custom colors for biomes
mycols <- c("darkolivegreen", "darkgoldenrod")

# Combine the boreal and tundra data for the legend
biome_data <- rbind(cbind(st_drop_geometry(data.frame(BIOME = 6)), 
                          st_geometry(boreal_forest_polar)) |> st_sf(),
                    cbind(st_drop_geometry(data.frame(BIOME = 11)), 
                          st_geometry(tundra_polar)) |> st_sf())



# Create map with polar projection
(biomes_polar_proj <- ggplot() +
    # add biomes
    geom_sf(data = biome_data, aes(fill = factor(BIOME)), color = NA) +
    # add country outlines
    geom_sf(data = world_polar, fill = NA, color = "darkgray", size = 0.2) +
    # set extent
    coord_sf(crs = proj_choice, 
             ylim = c(-703086, 7071423), 
             xlim = c(-505347.4, 8526158)) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = c(0.2, 0.1),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          plot.title = element_text(face = "bold", size = 16),
          panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
          panel.grid.minor = element_blank()) +
    # add custom colours and legend values
    scale_fill_manual(values = mycols,
                      labels = c("Boreal Forest Biome", "Tundra Biome"),
                      name = "Biome") +
    # add point legend
    guides(fill = guide_legend(title = "Biome")))

# Save figure to file
ggsave(filename = here("Figure3", "Figure1_biomes.png"),
       plot = biomes_polar_proj, width = 16, height = 12, dpi = 300)

ggsave(filename = here("Figure3", "Figure1_biomes.pdf"),
       plot = biomes_polar_proj, width = 16, height = 12, dpi = 300)

