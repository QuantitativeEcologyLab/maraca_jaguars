#-------------------------------------------------------------
# Workspace preparation
#-------------------------------------------------------------

# Load in any of the necessary packages
library(metafor)
library(mgcv)
library(lubridate)
library(gratia)

source("scripts/01_data_import.R")

#Extract speed, for each animal
meta_data$speed_est <- NA
meta_data$speed_min <- NA
meta_data$speed_max <- NA

for(i in 1:length(SPEEDS)){
  meta_data[meta_data$ID == names(SPEEDS)[i], "speed_est"] <- "km/day" %#% SPEEDS[[i]]$CI[2]
  meta_data[meta_data$ID == names(SPEEDS)[i], "speed_min"] <- "km/day" %#% SPEEDS[[i]]$CI[1]
  meta_data[meta_data$ID == names(SPEEDS)[i], "speed_max"] <- "km/day" %#% SPEEDS[[i]]$CI[3]
}

#-------------------------------------------------------------
# Basic speed descriptions
#-------------------------------------------------------------

# Mean speed
ctmm::meta(SPEEDS)

# Min speed
"km/day" %#% SPEEDS$ID696490B$CI 

# Max speed
"km/day" %#% SPEEDS$Netuno$CI


#-------------------------------------------------------------
# Differences between males and females
#-------------------------------------------------------------

ctmm::meta(list(males = SPEEDS[names(SPEEDS) %in% meta_data[meta_data$sex == "male", "ID"]],
                females = SPEEDS[names(SPEEDS) %in% meta_data[meta_data$sex == "female", "ID"]]))


#-------------------------------------------------------------
# Correlation with body weight
#-------------------------------------------------------------

fit <- gam(speed_est ~ weight,
           family = tw(link = "log"),
           data = meta_data,
           method = "REML")

summary(fit)


#-------------------------------------------------------------
# Correlation with age
#-------------------------------------------------------------

fit <- gam(speed_est ~ age,
           family = tw(link = "log"),
           data = meta_data,
           method = "REML")

summary(fit)



#-------------------------------------------------------------
# Comparing active vs. stationary in the different habitats
#-------------------------------------------------------------

#Remove NAs where habitat wasn't estimable (e.g., locations in the ocean)
speed_df <- na.omit(speed_df)

#proportions active & stationary
round(table(speed_df$active)/nrow(speed_df)*100,1)

#Model comparing active vs. stationary in the different habitats
activity_fit <- gam(active ~ class + s(ID, bs = 're'),
                    family = binomial(link = "logit"),
                    data = speed_df,
                    method = "REML")

summary(activity_fit)

#Null model
activity_null <- gam(active ~ 1 + s(ID, bs = 're'),
                     family = binomial(link = "logit"),
                     data = speed_df,
                     method = "REML")

#Likelihood ratio test
anova(activity_null,activity_fit, test = "Chisq")



#Estimated probability of moving in the different habitats
prediction_df <- data.frame(class = c("Forest","Grassland","Mangrove","Water", "Wetland"),
                            ID = "population")
activity_pred <- predict(activity_fit, newdata = prediction_df, se = T)
names(activity_pred$fit) <- c("Forest","Grassland","Mangrove","Water", "Wetland")
names(activity_pred$se.fit) <- c("Forest","Grassland","Mangrove","Water", "Wetland")

round(exp(activity_pred$fit)/(exp(activity_pred$fit)+1),2)
round(exp(activity_pred$fit - activity_pred$se.fit*1.96)/(exp(activity_pred$fit - activity_pred$se.fit*1.96)+1),2)
round(exp(activity_pred$fit + activity_pred$se.fit*1.96)/(exp(activity_pred$fit + activity_pred$se.fit*1.96)+1),2)


#-------------------------------------------------------------
# Comparing movement speeds in the different habitats
#-------------------------------------------------------------

moving_speeds <- na.omit(speed_df[speed_df$active == 1,])

mean(moving_speeds$est)
range(moving_speeds$est)

#Model comparing non-zero movement speeds in the different habitats
speed_fit <- gam(est ~ class + s(ID, bs = 're'),
                 family = tw(link = "log"),
                 data = moving_speeds,
                 method = "REML")

summary(speed_fit)

speed_null <- gam(est ~ 1 + s(ID, bs = 're'),
                  family = tw(link = "log"),
                  data = moving_speeds,
                  method = "REML")

anova(speed_null,speed_fit, test = "Chisq")

#average movement speed in the different habitats
prediction_df <- data.frame(class = c("Forest","Grassland","Mangrove","Water", "Wetland"),
                            ID = "population")
speed_pred <- predict(speed_fit, newdata = prediction_df, se = T)
names(speed_pred$fit) <- c("Forest","Grassland","Mangrove","Water", "Wetland")
names(speed_pred$se.fit) <- c("Forest","Grassland","Mangrove","Water", "Wetland")

round(exp(speed_pred$fit),3)
round(exp(speed_pred$fit - speed_pred$se.fit*1.96),2)
round(exp(speed_pred$fit + speed_pred$se.fit*1.96),2)




#-------------------------------------------------------------
# Comparing diurnal activity patterns between males and females
#-------------------------------------------------------------

speed_df$timestamp <- as.POSIXct(speed_df$time, tz = "America/Sao_Paulo")

speed_df$hr_min  <- as.POSIXct(format(speed_df$timestamp, format = "%H:%M"), format = "%H:%M", tz = "America/Sao_Paulo")

# Scale the speed estimate
speed_df$est_scaled <- speed_df$est/max(speed_df$est)

speed_df <- merge(x = speed_df, y = meta_data, by.x = "ID", by.y = "ID")

speed_df$ID <- as.factor(speed_df$ID)
speed_df$sex <- as.factor(speed_df$sex)

speed_df$hr_min_numeric <- as.numeric(format(speed_df$timestamp, format = "%H")) + as.numeric(format(speed_df$timestamp, format = "%M"))/60 + as.numeric(format(speed_df$timestamp, format = "%S"))/3600


circadian_fit <- bam(active ~ s(hr_min_numeric, bs = "cc") + s(hr_min_numeric, sex, bs = "sz") + s(ID, bs = 're'),
                     family = binomial(link = "logit"),
                     data = speed_df,
                     method = "fREML",
                     discrete = T)
summary(circadian_fit)


circadian_fit_no_sex <- bam(active ~ s(hr_min_numeric, bs = "cc") + s(ID, bs = 're'),
                     family = binomial(link = "logit"),
                     data = speed_df,
                     method = "fREML",
                     discrete = T)

anova(circadian_fit_no_sex,circadian_fit, test = "Chisq")

