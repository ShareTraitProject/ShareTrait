# ------------------------------------------------------------------------------
# Global map with studies included on ShareTrait
# ------------------------------------------------------------------------------
# Cleaning working space
rm(list = ls())

# Check directory
getwd()

# libraries
library(rgdal)
library(sp)
library(data.table)
library(ggplot2)
library(mapview)
library(dplyr)
library(cowplot)
library(tidyverse)
library(RColorBrewer)
library(colorRamps)

# load data
ShareTrait <- fread("../outputs/2_2_ShareTrait_DataBase.csv", 
               #colClasses = "character",
               na.strings = c("NA","N/A","null", ""),
               select = c("trait_name","species_reported","realm_general","lat_decimal","long_decimal"))

# check if the columns were selected correctly
colnames(ShareTrait)

# rename some columns
names(ShareTrait)[names(ShareTrait) == "long_decimal"] <- "lon"
names(ShareTrait)[names(ShareTrait) == "lat_decimal"]  <- "lat"

# check if the columns were renamed correctly
print(colnames(ShareTrait))

# exclude empty rows
df <- drop_na(ShareTrait)

# check the range of 'lon' column
print(range(df$lon) %between% c(-180, 180))

# check the range of 'lat' column
print(range(df$lat) %between% c(-90, 90))

#yes, they are within expected ranges

# Transform unprojected long-lat in Robinson coordinates
df[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]


# Check points with interactive map
points_WGS84 <- sp::SpatialPointsDataFrame(coords      = df[,.(lon, lat)], # order matters
                                           data        = df[,.(trait_name)], 
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
html_map <- mapview(points_WGS84)
# save as html
mapshot(html_map, url = "Global_map.html")

# ------------------------------------------------------------------------------
# Load & prepare NaturalEarth shapefiles
# ------------------------------------------------------------------------------
load("NaturalEarth.RData"); setDT(lbl.Y); setDT(lbl.X)
# Details about NaturalEarth shapefiles:
#   The files were already downloaded from http://www.naturalearthdata.com/
#   Graticules were adjusted to 10 dg for latitude lines and 20 dg for longitude lines 
#   (some editing was carried in ArcMap)

# Project from long-lat (unprojected) to Robinson projection
NE_countries_rob  <- spTransform(NE_countries, CRS("+proj=robin"))
NE_graticules_rob <- spTransform(NE_graticules, CRS("+proj=robin"))
NE_box_rob        <- spTransform(NE_box, CRS("+proj=robin"))

# Shift longitude of OX graticule labels. This was needed because, for example,
# the 160 dg label ended up on the 180 longitude line when projecting to
# Robinson. A shift is applied for each degree in the sequence 
# seq(from = 160, to = 0, by = -20)
shift <- c(10, 10, 9, 8, 8, 5, 2, 0, 0)
lbl.X[, shift := rep(c(shift, -rev(shift)[-1]), 2)]
lbl.X
lbl.X[, lon := lon - shift] # apply shift
lbl.X[, shift := NULL] # delete column

# Project the labales for graticules to Robinson
lbl.Y[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]
lbl.X[, c("X.prj","Y.prj") := data.table(rgdal::project(xy   = cbind(lon, lat),
                                                        proj = "+proj=robin"))]
# Create helper columns with nudged coordinates for plotting graticule labels.
# For lbl.Y nudge longitude and for lbl.X nudge latitude.
# Give nudge values in dg (if you change, re-run also the projection lines above)
my_nudge <- cbind(nudge_lon = 10, 
                  nudge_lat = 4) 
my_nudge <- rgdal::project(my_nudge, proj = "+proj=robin")
lbl.Y[, X.prj := ifelse(lon < 0, 
                        yes = X.prj - my_nudge[1,1], 
                        no = X.prj + my_nudge[1,1])]
lbl.X[, Y.prj := ifelse(lat < 0, 
                        yes = Y.prj - my_nudge[1,2], 
                        no = Y.prj + my_nudge[1,2])]


# ------------------------------------------------------------------------------
# Plot map 
# ------------------------------------------------------------------------------

# Prepare base map
base_map <- 
  ggplot() +
  # ___ add graticules projected to Robinson
  geom_path(data = NE_graticules_rob, 
            aes(x     = long, 
                y     = lat, 
                group = group), 
            linetype = "dotted", 
            color    = "grey50", 
            size     = 0.2) +
  # ___ add Natural Earth countries projected to Robinson
  geom_polygon(data = NE_countries_rob, 
               aes(x     = long,
                   y     = lat, 
                   group = group), 
               colour = "gray80", # country border color
               fill   = "gray80", # country fill color
               size   = 0.2) +
  # ___ add graticule labels - latitude and longitude
  geom_text(data = lbl.Y, 
            aes(x     = X.prj, 
                y     = Y.prj, 
                label = lbl), 
            color   = "black", 
            size    = 0.8) +
  geom_text(data = lbl.X, 
            aes(x     = X.prj, 
                y     = Y.prj, 
                label = lbl), 
            color   = "black", 
            size    = 0.8) +
  # ___ add Natural Earth box projected to Robinson
  geom_polygon(data = NE_box_rob, 
               aes(x = long, 
                   y = lat), 
               colour ="black", 
               fill   ="transparent", 
               size   = 0.2) +
  # "Regions defined for each Polygons" warning has to do with fortify
  # transformation. Might get deprecated in future.
  # ___ the default ratio = 1 in coord_fixed ensures that one unit on the x-axis 
  # is the same length as one unit on the y-axis
  coord_fixed(ratio = 1) +
  # # ___ remove the background and default gridlines
  theme_void()

base_map

# Add study locations (points)
ShareTrait_map <- base_map + 
  geom_point(data = df, 
             aes(x = X.prj, y = Y.prj, colour = trait_name), 
             size = 0.5, alpha = .6, shape = 16)

ShareTrait_map <- ShareTrait_map +
  guides(colour = guide_legend(override.aes = list(alpha = .8))) +
  theme(legend.position = "bottom", legend.justification = "center") +
  labs(color = "") +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(0.1, "cm"),
    legend.key.width = unit(0.1, "cm"),
    legend.text = element_text(size = 6, family = "Times"),
    legend.title = element_text(size = 8, family = "Times"))
ShareTrait_map
# ------------------------------------------------------------------------------
# Save as pdf and jpeg file 
# ------------------------------------------------------------------------------
# ggsave(plot = ShareTrait_map, 
#        filename = "Global_map_ShareTrait_DataBase_v1.0.0.pdf", 
#        width = 14, 
#        height = 7, 
#        units = "cm")

ggsave(plot = ShareTrait_map, 
       filename = "Global_map_ShareTrait_DataBase_v1.0.0.jpg", 
       width = 14, 
       height = 7, 
       units = "cm", 
       dpi = 1000)

# saving session information with all packages versions for reproducibility purposes
sink("../outputs/3_1_Global_map_ShareTrait_R_session.txt")
sessionInfo()
sink()
# END OF SCRIPT