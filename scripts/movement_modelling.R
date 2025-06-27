#import packages
library(ctmm)
library(raster)
library(terra)
library(tidyterra)
library(sf)
library(ggplot2)
library(lubridate)
library(mgcv)
library(gridExtra)

#-------------------------------------------------------------
# Data import and pre-processing
#-------------------------------------------------------------

source("scripts/data_import.R")
source("scripts/functions.R")

#-------------------------------------------------------------
# Movement modelling
#-------------------------------------------------------------

FITS <- list()
for(i in 1:length(DATA)){
  
  #extract the ith individual
  cilla <- DATA[[i]]
  #First fit the movement model using the usual workflow
  #Generate the variogram
  vg.cilla <- variogram(cilla)
  
  #fit the variogram
  GUESS <- ctmm.guess(cilla, interactive= FALSE)
  
  #Assign the file path
  ctmm.path <- file.path("results/movement_models", paste("fit_", cilla@info[1], ".rda", sep = ""))
  
  #Import the model if it has been fit already, otherwise fit it
  if(file.exists(ctmm.path)) {
    message("Importing an existing movement model for: ", unlist(cilla@info[1]))
    load(ctmm.path)
  } else {
    
    #Fit the movement models
    cilla.mods <- ctmm.select(cilla,
                              CTMM = GUESS,
                              cores = -1,
                              trace = TRUE)
    
    #Save the best fit model
    #First get the best fit model if more than 1 have been fit
    if(class(cilla.mods) == "list") {FIT <- cilla.mods[[1]]} else {
      
      FIT <- cilla.mods
    }
    
    
    #And save
    save(FIT, file = ctmm.path)
  }
  
  FITS[[i]] <- FIT
  
  #Assign the file path for the AKDE
  akde.path <- file.path("results/akdes", paste("AKDE_", cilla@info[1], ".rda", sep = ""))
  
  #Import the model if it has been fit already, otherwise fit it
  if(file.exists(akde.path)) {
    message("Importing an existing home range for: ", unlist(cilla@info[1]))
    load(akde.path)
  } else {
    # Estimate the home range
    message("Estimating a home range for: ", unlist(cilla@info[1]))
    
    AKDE <- akde(cilla,
                 FIT,
                 SP = maraca_spdf,
                 weights=T)
    
    #And save
    save(AKDE, file = akde.path)
  }
  
}#Closes loop over all animals


save(FITS, file = "results/jaguar_fits.Rda")



#Global AKDE for overlap analysis
AKDEs <- akde(DATA,
              FITS,
              SP = maraca_spdf,
              weights=T)

#Rename the elements of the list to match the tracking data IDs
names(AKDEs) <- names(DATA)

#Save
save(AKDEs, file = "results/jaguar_akdes.Rda")


#-------------------------------------------------------------
# Movement speed analyses
#-------------------------------------------------------------

#Import the model if it has been fit already, otherwise fit it
if(file.exists("results/jaguar_speeds.Rda")) {
  load("results/jaguar_speeds.Rda")
} else {
  # Estimate mean speed
  SPEEDS <- list()
  
  for(i in 1:length(DATA)){
    SPEEDS[[i]] <- speed(DATA[[i]], FITS[[i]],
                         cores = -1,
                         units = F)
  }
  
  #Rename the elements of the list to match the tracking data IDs
  names(SPEEDS) <- names(DATA)
  
  #Save
  save(SPEEDS, file = "results/jaguar_speeds.Rda")
}


#Instantaneous movement speed estimates
speed_df <- list()
for(i in 1:length(DATA)){
  
  #First get instantaneous movement speeds
  speeds_df_i <- detrend.speeds(DATA[[i]], FITS[[i]])
  speeds_df_i$ID <- DATA[[i]]@info$identity
  speeds_df_i$longitude <- DATA[[i]]$longitude
  speeds_df_i$latitude <- DATA[[i]]$latitude
  
  #Then get habitat information
  cilla_sf <- as.sf(DATA[[i]])
  speeds_df_i$class <- extract(maraca_land, cilla_sf)[,2]
  speeds_df_i$dist_to_coast <- extract(dist_to_coast, cilla_sf)[,2]
  
  speed_df[[i]] <- speeds_df_i
}

speed_df <- do.call(rbind, speed_df)


#Use a k-means clustering to identify periods where they are inactive 
test <- kmeans(speed_df[,c("low","est")], 3)
speed_df$active <- 0
speed_df[speed_df$low > min(test$centers[,"low"]),"active"] <- 1


#Save as a csv
write.csv(speed_df, "data/jaguar_speeds.csv", row.names = F)



#-------------------------------------------------------------
# Home-range overlap and encounter rates
#-------------------------------------------------------------
#NOTE: The proximity() analysis is slow

#Vector for subseting to the five animals sampled during the same time frame
IDs <- c("Iranildo", "Iemanja2", "ID696469B", "ID696490B", "ID717047B")

#Setup the pairwise analysis
pairs <- as.data.frame(t(combn(IDs,2)))
names(pairs) <- c("jag_1", "jag_2")
pairs$pair <- NA
pairs$overlap <- NA
pairs$encounters <- NA

pairs$proximity_low <- NA
pairs$proximity_est <- NA
pairs$proximity_high <- NA

names(FITS) <- names(DATA)

distances <- list()

for(i in 1:nrow(pairs)){
  
  #Get the pair's information
  pairs[i,"pair"] <- paste(meta_data[meta_data$ID == pairs[i,1],"sex"],meta_data[meta_data$ID == pairs[i,2],"sex"], sep = "_")
  if(pairs[i,"pair"] == "male_female"){pairs[i,"pair"] <- "female_male"}
  
  #Pairwise overlap
  pairs[i,"overlap"] <- overlap(AKDEs[c(pairs[i,1],pairs[i,2])])$CI[,,"est"][1,2]
  
  #Estimate pairwise separation distances
  dists_i <- ctmm::distances(DATA[c(pairs[i,1],pairs[i,2])],
                             FITS[c(pairs[i,1],pairs[i,2])])
  
  #Number of encounters 
  pairs[i,"encounters"] <- sum(dists_i$low < 100)
  
  #Add the pair's information into the distance dataset
  dists_i$jag_1 <- pairs[i,1]
  dists_i$jag_2 <- pairs[i,2]
  dists_i$pair <- pairs[i,"pair"]
  
  distances[[i]] <- dists_i
  
  #Proximity ratio
  prox_i <- proximity(DATA[c(pairs[i,1],pairs[i,2])],
                      FITS[c(pairs[i,1],pairs[i,2])],
                      cores = -1)
  
  pairs[i,"proximity_low"] <- prox_i["low"]
  pairs[i,"proximity_est"] <- prox_i["est"]
  pairs[i,"proximity_high"] <- prox_i["high"]
}

write.csv(pairs, file = "results/jaguar_overlap.csv", row.names = F)

distance_df <- do.call(rbind, distances)
save(distance_df, file = "results/distance_df.Rda")

