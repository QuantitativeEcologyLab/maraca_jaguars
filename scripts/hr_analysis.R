# Load in any of the necessary packages
library(ggplot2)
library(metafor)
library(lubridate)

source("scripts/data_import.R")


#-------------------------------------------------------------
# Basic home-range descriptions
#-------------------------------------------------------------

#Extract HR, and variance for for each animal
meta_data$hr_est <- NA
meta_data$log_hr <- NA
meta_data$log_hr_var <- NA
meta_data$n_area <- NA

for(i in 1:length(AKDEs)){
  meta_data[meta_data$ID == names(AKDEs)[i], "hr_est"] <- summary(AKDEs[[i]], units = F)$CI[2]
  meta_data[meta_data$ID == names(AKDEs)[i], "log_hr"] <- Log(AKDEs[[i]])[,1]
  meta_data[meta_data$ID == names(AKDEs)[i], "log_hr_var"] <- Log(AKDEs[[i]])[,2]
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

#Model comparing non-zero movement speeds in the different habitats
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


A <-
  ggplot(data = pairs, 
         mapping = aes(x = pair, y = overlap, fill = pair)) + 
  geom_boxplot(alpha = 0.5, size = 0.3, outliers = F) +
  geom_jitter(aes(col = pair), size = 0.5, shape = 16, position=position_jitter(height=0, width=0.1)) +
  ylab("Home-range overlap") +
  xlab("Sex") +
  ggtitle("A") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
        legend.position="none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#fca311", "#2d6a4f", "#14213d"),
                      breaks = c("female_female","female_male","male_male"),
                      labels = c("Female - Female", "Female - Male", "Male - Male")) +
  scale_fill_manual(values = c("#fca311", "#2d6a4f", "#14213d"),
                    breaks = c("female_female","female_male","male_male"),
                    labels = c("Female - Female", "Female - Male", "Male - Male")) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(breaks = c("female_female","female_male","male_male"),
                   labels = c("Female - Female", "Female - Male", "Male - Male"))



B <- 
  ggplot(data = pairs, 
         aes(y = proximity_est, x = overlap, col = pair),) +
  ggtitle("B") +
  geom_hline(yintercept = 1, col = "grey70", linetype = "dashed") +
  geom_point(size = 1.2, shape = 16) + #alpha = colour intensity
  geom_segment(aes(x = overlap, xend = overlap, y = proximity_low, yend = proximity_high, col = pair), 
               linewidth = 0.3) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0.02)) +
  scale_colour_manual(values = c("#fca311", "#2d6a4f", "#14213d"),
                      breaks = c("female_female","female_male","male_male"),
                      labels = c("Female - Female", "Female - Male", "Male - Male"),
                      name = "") +
  ylab("Proximity ratio") +
  xlab("Home-range overlap") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.005, size = 10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.9),
        legend.key.height = unit(0.3, "cm"),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))




C <-
  ggplot(data = encs,
         aes(y = encounter, x = doy)) +
  geom_bar(stat = "identity", position = "stack", aes(fill = pair)) + 
  scale_x_continuous(limits = c(-2, 340), expand = c(0,0),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  scale_y_continuous(limits = c(0,11), expand = c(0,0)) +
  scale_fill_manual(values = c("#fca311", "#2d6a4f", "#14213d"),
                    breaks = c("female_female","female_male","male_male"),
                    labels = c("Female - Female", "Female - Male", "Male - Male")) +
  xlab("Month") +
  ylab("Encounter count") +
  ggtitle("C") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.position="none",
        # legend.text = element_text(size=6, family = "sans", face = "bold"),
        # legend.position = c(0.84, 0.2), #horizontal, vertical
        # legend.key.height = unit(0.3, "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



TOP <-
  grid.arrange(A,B,
               ncol=2,
               nrow=1)


FIG <-
  ggarrange(TOP, C,
            ncol=1,
            nrow=2,
            heights = c(1.2,0.8))


ggsave(FIG, filename = "figures/overlap_encounters.png",
       width = 6.86, height = 4.5, units = "in", dpi = 600)

