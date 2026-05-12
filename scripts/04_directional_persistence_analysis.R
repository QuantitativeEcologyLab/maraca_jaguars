# Load in any of the necessary packages
library(ggplot2)
library(metafor)

source("scripts/01_data_import.R")


#-------------------------------------------------------------
# Basic directional persistence descriptions
#-------------------------------------------------------------

#Rename the list of fitted models
names(FITS) <- names(AKDEs)

#Extract tau_v, and variance for for each animal
meta_data$tau_v_est <- NA
meta_data$tau_v_var <- NA
meta_data$dof_speed <- NA

for(i in 1:length(FITS)){
  meta_data[meta_data$ID == names(FITS)[i], "tau_v_est"] <- summary(FITS[[i]], units = F)$CI[3,2]
  meta_data[meta_data$ID == names(FITS)[i], "tau_v_var"] <- FITS[[i]]$COV["tau velocity","tau velocity"]
  meta_data[meta_data$ID == names(FITS)[i], "dof_speed"] <- summary(FITS[[i]])$DOF["speed"]
  
}

# Mean tau_v
ctmm::meta(FITS, variable = "tau velocity", sort = TRUE)

# Min tau_v
summary(FITS$Iara)

# Max tau_v
summary(FITS$ID696490B)


#-------------------------------------------------------------
# Differences between males and females
#-------------------------------------------------------------

test <- ctmm::meta(list(males = FITS[names(FITS) %in% meta_data[meta_data$sex == "male", "ID"]],
                        females = FITS[names(FITS) %in% meta_data[meta_data$sex == "female", "ID"]]),
                   verbose = T, variable = "tau velocity")


#-------------------------------------------------------------
# Correlation with body weight
#-------------------------------------------------------------

#Use meta analyses methods to run test
DAT <- escalc(measure = "MNLN",
              mi = tau_v_est,
              sdi = sqrt(tau_v_var),
              ni = dof_speed,
              data = meta_data)

res <- rma(yi ~ weight,
           vi,
           data=DAT,
           method="REML")
res


#-------------------------------------------------------------
# Correlation with age
#-------------------------------------------------------------

#Use meta regression to test for any difference in tau_velocity size between groups
res <- rma(yi ~ age,
           vi,
           data=DAT,
           method="REML")
res

