# Load in any of the necessary packages
library(ctmm)
library(raster)
library(terra)

#-------------------------------------------------------------
# Jaguar meta-data
#-------------------------------------------------------------

meta_data <- read.csv("data/jaguar_metadata.csv")

#-------------------------------------------------------------
# Tracking data import and pre-processesing
#-------------------------------------------------------------

#Import Tracking data (while removing outliers)
data <- read.csv("data/gps/jaguar_data_locations_Maraca-Jipioca.csv")
DATA <- as.telemetry(data, mark.rm=T)
ctmm::projection(DATA) <- median(DATA)


#-------------------------------------------------------------
# Import the land class information
#-------------------------------------------------------------

# Import the MapBiomas 2023 land classification raster for the two Maraca Islands
maraca_land <- rast("data/environment/maraca.tif")

# Build individual rasters for each of the land class co-variates
# Needed for fitting the RSFs

#Forest
forest <- maraca_land
forest[forest %in% c(3,6,9)] <- 1
forest[forest != 1] <- 0
forest <- project(forest,
                       DATA[[1]]@info$projection,
                       method = "near")

#Mangrove
mangrove <- maraca_land
mangrove[mangrove %in% 5] <- 1
mangrove[mangrove != 1] <- 0
mangrove <- project(mangrove,
                  DATA[[1]]@info$projection,
                  method = "near")

#Wetland
wetland <- maraca_land
wetland[wetland %in% 11] <- 1
wetland[wetland != 1] <- 0
wetland <- project(wetland,
                  DATA[[1]]@info$projection,
                  method = "near")

#Grassland
grassland <- maraca_land
grassland[grassland %in% 12] <- 1
grassland[grassland != 1] <- 0
grassland <- project(grassland,
                  DATA[[1]]@info$projection,
                  method = "near")

#Water
water <- maraca_land
water[water %in% 33] <- 1
water[water != 1] <- 0
water <- project(water,
                  DATA[[1]]@info$projection,
                  method = "near")

#Agriculture
agriculture <- maraca_land
agriculture[agriculture %in% 41] <- 1
agriculture[agriculture != 1] <- 0
agriculture <- project(agriculture,
                  DATA[[1]]@info$projection,
                  method = "near")

#Convert the categories to labels (some comparable and rare land classes are merged)
# Details on the key are available here: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/10/Legenda-Colecao-9-LEGEND-CODE_v2.pdf 
#levels(maraca_land) <- data.frame(ID = c(3,5,6,9,11,12,33,41), category = c(3,5,6,9,11,12,33,41))
maraca_land <- ifel(maraca_land %in% c(3,6,9), 3, maraca_land)
levels(maraca_land) <- data.frame(ID = c(3,
                                         5,
                                         11,
                                         12,
                                         33,
                                         41),
                                  class = c("Forest",
                                            "Mangrove",
                                            "Wetland",
                                            "Grassland",
                                            "Water",
                                            "Agriculture"))

# Reproject to match the projection of the tracking data
maraca_land <- project(maraca_land,
                       DATA[[1]]@info$projection,
                       method = "near")

#Island boundary
maraca <- vect("data/environment/maraca/maraca.shp")
maraca <- project(maraca, DATA[[1]]@info$projection)

#Create a second "SpatialPolygonsDataFrame" object that is needed by ctmm
maraca_spdf <- as(maraca, "Spatial")

# Generate a distance to coast raster
r <- rast(maraca, res = res(maraca_land))
r <- rasterize(maraca, r)
r[is.na(r)] <- 2
r[r == 1] <- NA
dist_to_coast <- distance(r)
#dist_to_coast <- crop(dist_to_coast, maraca, mask = T)
rm(r)

#-------------------------------------------------------------
# Import the movement movement metrics
#-------------------------------------------------------------

#Movement models
if(file.exists("results/jaguar_fits.Rda")) {load("results/jaguar_fits.Rda")}

#Home ranges
if(file.exists("results/jaguar_akdes.Rda")) {load("results/jaguar_akdes.Rda")}

#Speed estimates
if(file.exists("results/jaguar_speeds.Rda")) {load("results/jaguar_speeds.Rda")}

#Instantaneous movement speeds
if(file.exists("data/jaguar_speeds.csv")) {speed_df <- read.csv("data/jaguar_speeds.csv")}
speed_df$ID <- as.factor(speed_df$ID)

#Separation distances
if(file.exists("results/distance_df.Rda")) {load("results/distance_df.Rda")}

#Overlap and encounter nestimates
if(file.exists("results/jaguar_overlap.csv")) {pairs <- read.csv("results/jaguar_overlap.csv")}

#-------------------------------------------------------------
# Import the tide data
#-------------------------------------------------------------

# #Import and process the tide data
tide <- rast("data/environment/cmems_mod_glo_phy_anfc_merged-sl_PT1H-i_1750382952624.nc")
tide <- data.frame(time = time(tide),
                   masl = global(tide, fun = "mean", na.rm = TRUE)[,1])
tide$timestamp <- as.POSIXct(tide$time, format = "%Y-%m-%d %H:%M:%OS", tz = "America/Sao_Paulo")
