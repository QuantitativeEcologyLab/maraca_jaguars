#-------------------------------------------------------------
# Workspace preparation
#-------------------------------------------------------------

# Load in any of the necessary packages
library(ggplot2)
library(metafor)
library(mgcv)
library(lubridate)
library(gratia)

source("scripts/data_import.R")

#Extract speed, for each animal
meta_data$speed_est <- NA

for(i in 1:length(SPEEDS)){
  meta_data[meta_data$ID == names(SPEEDS)[i], "speed_est"] <- "km/day" %#% SPEEDS[[i]]$CI[2]
}

#-------------------------------------------------------------
# Basic speed descriptions
#-------------------------------------------------------------

# Mean home-range size
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

summary(fit)

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

summary(fit)

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


A <- 
  draw(circadian_fit,
       select = "s(hr_min_numeric)",
       rug = F,
       caption = F) +
  ggtitle("A") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        #strip.text.x = element_text(size=6, family = "sans", face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=5, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  xlab("Time of day (Hrs)")

B <- 
  draw(circadian_fit,
       select = "s(hr_min_numeric,sex)",
       rug = F,
       caption = F) +
  ggtitle("B") +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        #strip.text.x = element_text(size=6, family = "sans", face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=5, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  xlab("Time of day (Hrs)")
  

C <- 
  ggplot(speed_df, aes(x = hr_min, y = active, col = sex)) +
  ggtitle("C") +
  stat_smooth(aes(fill = sex), method = "gam", formula = y ~ s(x, bs = "cc"), linewidth = 0.4, se = F) +
  stat_smooth(aes(fill = sex), method = "gam", formula = y ~ s(x, bs = "cc"), linewidth = 0.4, show.legend = FALSE) +
  scale_x_datetime(date_breaks = "3 hours", date_labels = "%H:00") +
  coord_radial(expand = F) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male"), guide="none") +
  theme_bw() +
  theme(panel.grid.major.x = element_line(color = "black", linewidth = 0.2),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_blank(),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=8, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  labs(y = "",
       x = "Time of day")


FIG <-
  grid.arrange(A,B,C,
               ncol=3,
               nrow=1,
               widths = c(1,1,1.1))

ggsave(FIG, filename = "figures/jags_circadian.png",
       width = 6.86*1.5, height = 2*1.3, units = "in", dpi = 600)
