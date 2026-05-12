# Load in any of the necessary packages
library(ggplot2)
library(metafor)
library(lubridate)
library(mgcv)

source("scripts/01_data_import.R")


#-------------------------------------------------------------
# Basic home-range descriptions
#-------------------------------------------------------------

#Extract HR, and variance for each animal
meta_data$hr_est <- NA
meta_data$log_hr <- NA
meta_data$log_hr_var <- NA
meta_data$n_area <- NA

for(i in 1:length(AKDEs)){
  meta_data[meta_data$ID == names(AKDEs)[i], "hr_est"] <- summary(AKDEs[[i]], units = F)$CI[2]
  meta_data[meta_data$ID == names(AKDEs)[i], "log_hr"] <- Log(AKDEs[[i]])$log
  meta_data[meta_data$ID == names(AKDEs)[i], "log_hr_var"] <- Log(AKDEs[[i]])$VAR.log
  meta_data[meta_data$ID == names(AKDEs)[i], "n_area"] <- summary(AKDEs[[i]])$DOF["area"]
}

# Mean home-range size
ctmm::meta(AKDEs)

# Min HR size
summary(AKDEs$Iemanja2)

# Max HR size
summary(AKDEs$Iranildo)


#-------------------------------------------------------------
# Differences between males and females
#-------------------------------------------------------------

test <- ctmm::meta(list(males = AKDEs[names(AKDEs) %in% meta_data[meta_data$sex == "male", "ID"]],
                        females = AKDEs[names(AKDEs) %in% meta_data[meta_data$sex == "female", "ID"]]),
                   verbose = T)

test$males

test$females


#-------------------------------------------------------------
# Correlation with body weight
#-------------------------------------------------------------

#Use meta analyses methods to run test
DAT <- escalc(measure = "MNLN",
              mi = log_hr,
              sdi = sqrt(log_hr_var),
              ni = n_area,
              data = meta_data)

res <- rma(yi ~ weight,
           vi,
           data=DAT,
           method="REML")
res


#-------------------------------------------------------------
# Correlation with age
#-------------------------------------------------------------

#Use meta regression to test for any difference in HR size between groups
res <- rma(yi ~ age,
           vi,
           data=DAT,
           method="REML")
res




#-------------------------------------------------------------
# Home-range overlap and encounter rates
#-------------------------------------------------------------
#Subset to the five animals sampled during the same time frame
IDs <- c("Iranildo", "Iemanja2", "ID696469B", "ID696490B", "ID717047B")
OVER <- overlap(AKDEs[IDs])

# Mean home-range overlap
mean(OVER$CI[,,"est"][lower.tri(OVER$CI[,,"est"])])
range(OVER$CI[,,"est"][lower.tri(OVER$CI[,,"est"])])


#set threshold of 100m
distance_df$encounter <- ifelse(distance_df$low > 100, 0,1)
encounter_df <- distance_df[which(distance_df$encounter == 1),]
encounter_df$doy <- yday(encounter_df$timestamp) #day of the year
encounter_df$month <- month(encounter_df$timestamp, label = TRUE)
encs <- aggregate(encounter ~ pair + doy + month, data = encounter_df, FUN = "sum")



#-------------------------------------------------------------
# Home-range overlap and sexes
#-------------------------------------------------------------

#Model comparing homem-range overlap between sex dyads
overlap_sex_fit <- gam(overlap ~ pair,
                       family = betar(link = "logit"),
                       data = pairs,
                       method = "REML")

summary(overlap_sex_fit)

overlap_sex_null <- gam(overlap ~ 1,
                       family = betar(link = "logit"),
                       data = pairs,
                       method = "REML")

anova(overlap_sex_null,overlap_sex_fit, test = "Chisq")

