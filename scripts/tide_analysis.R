
#-------------------------------------------------------------
# Workspace preparation
#-------------------------------------------------------------

# Load in any of the necessary packages
library(ggplot2)
library(ggpubr)
library(mgcv)
library(lubridate)

source("scripts/data_import.R")


#Colour pallet for the individual animals
COLS <- c("#001427",
          "#fca311",
          "#ffd60a",
          "#a34e97",
          "#7400b8",
          "#b85236",
          "#c32f27",
          "#f72585")

#-------------------------------------------------------------
# Distance to coast vs sea level
#-------------------------------------------------------------

DISTS <- list()
#Calculate distance to coast
for(i in 1:length(DATA)){
  
  jag_sf <- as.sf(DATA[[i]])[1]
  
  #Compile results
  dists <- data.frame(dist_to_coast = extract(dist_to_coast, jag_sf)[,2])
  dists$time <- DATA[[i]]$timestamp
  dists$longitude <- DATA[[i]]$longitude
  dists$latitude <- DATA[[i]]$latitude
  dists$ID <- DATA[[i]]@info$identity
  
  DISTS[[i]] <- dists
}

#Some data carpentry
DISTS <- do.call(rbind, DISTS)
DISTS$timestamp <- round_date(DISTS$time, unit = "hour")
DISTS$timestamp <- force_tz(DISTS$timestamp,tzone = "America/Sao_Paulo")

#Merge in the tide data
DISTS <- merge(x = DISTS, y = tide,
               by.x = "timestamp", by.y = "timestamp")


#Remove a few NAs
DISTS <- na.omit(DISTS)
DISTS$ID <- as.factor(DISTS$ID)

#Model comparing active vs. stationary in the different habitats
dist_fit <- gam(dist_to_coast ~ masl + s(ID, bs = 're'),
                family = tw(link = "log"),
                data = DISTS,
                method = "REML")

summary(dist_fit)

#Null model
dist_null <- gam(dist_to_coast ~ 1 + s(ID, bs = 're'),
                 family = tw(link = "log"),
                 data = DISTS,
                 method = "REML")

#Likelihood ratio test
anova(dist_null,dist_fit, test = "Chisq")

AIC(dist_fit) - AIC(dist_null)



#-------------------------------------------------------------
# Distance to coast vs sea level figure
#-------------------------------------------------------------


A <- 
  ggplot(data = tide, aes(x = timestamp, y = masl)) +
  ggtitle("A") +
  geom_line(size = 0.2, alpha = 0.4) +
  
  ylab("Meters Above Sea Level (m)") +
  xlab("Time") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold", vjust = -2),
        legend.text  = element_text(size=5, family = "sans", face = "bold"),
        axis.ticks.length=unit(0.08, "cm"),
        axis.ticks = element_line(size = 0.3),
        legend.position = "none",
        legend.position.inside  = c(0.7,0.95),
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,0.1), "cm"))


B <- 
  ggplot(data = DISTS, aes(x = timestamp, y = dist_to_coast)) +
  ggtitle("B") +
  geom_point(size = 0.2, alpha = 0.2, pch = 16) +
  #scale_colour_manual(values = COLS) +
  
  ylab("Distance to coast (m)") +
  xlab("Time") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold", vjust = -2),
        legend.text  = element_text(size=5, family = "sans", face = "bold"),
        axis.ticks.length=unit(0.08, "cm"),
        axis.ticks = element_line(size = 0.3),
        legend.position = "none",
        legend.position.inside  = c(0.7,0.95),
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,0.1), "cm")) +
  scale_y_continuous(expand = c(0,30)) #+ 
scale_y_log10() +
  annotation_logticks(sides="l",
                      outside = T,
                      linewidth = 0.2,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm"))


C <- 
  ggplot(data = DISTS, aes(x = masl, y = dist_to_coast)) +
  ggtitle("C") +
  geom_point(size = 0.2, alpha = 0.2, pch = 16) +
  geom_smooth(method = "gam",
              formula = y ~ x,
              method.args = list(family = tw(link = "log")),
              col = "#2a9d8f",
              fill = "#2a9d8f",
              linewidth = 0.5,
              alpha = 0.2) +

  #scale_colour_manual(values = COLS) +
  
  ylab("Distance to coast") +
  xlab("Meters Above Sea Level (m)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold", vjust = -2),
        legend.text  = element_text(size=5, family = "sans", face = "bold"),
        axis.ticks.length=unit(0.08, "cm"),
        axis.ticks = element_line(size = 0.3),
        legend.position = "none",
        legend.position.inside  = c(0.7,0.95),
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,0.1), "cm")) +
  scale_y_continuous(expand = c(0,30)) #+ 
scale_y_log10() +
  annotation_logticks(sides="l",
                      outside = T,
                      linewidth = 0.2,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm"))



FIG <-
  ggarrange(A,B,C,
            ncol=3,
            nrow=1)

ggsave(FIG,
       file="figures/sea_level.png",
       width = 6.46,
       height=2,
       units = "in",
       dpi = 600)

