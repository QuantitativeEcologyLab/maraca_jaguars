library(ggplot2)
library(ggspatial)
library(tidyterra)
library(terra)
library(sf)
library(ctmm)
library(ggpubr)

#-------------------------------------------------------------
# Data import and pre-processesing
#-------------------------------------------------------------
source("scripts/data_import.R")


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

#Colour pallet for the individual animals
COLS <- c("#001427",
          "#fca311",
          "#ffd60a",
          "#a34e97",
          "#7400b8",
          "#b85236",
          "#c32f27",
          "#f72585")


#Convert tracking data to sf format
Iara_sf <- as.sf(DATA$Iara, crs = crs(maraca_land))
Iara_sf <- st_transform(Iara_sf, crs = crs(maraca_land))

ID696469B_sf <- as.sf(DATA$ID696469B, crs = crs(maraca_land))
ID696469B_sf <- st_transform(ID696469B_sf, crs = crs(maraca_land))

ID696490B_sf <- as.sf(DATA$ID696490B, crs = crs(maraca_land))
ID696490B_sf <- st_transform(ID696490B_sf, crs = crs(maraca_land))

ID717047B_sf <- as.sf(DATA$ID717047B, crs = crs(maraca_land))
ID717047B_sf <- st_transform(ID717047B_sf, crs = crs(maraca_land))

Iemanja1_sf <- as.sf(DATA$Iemanja1, crs = crs(maraca_land))
Iemanja1_sf <- st_transform(Iemanja1_sf, crs = crs(maraca_land))

Iemanja2_sf <- as.sf(DATA$Iemanja2, crs = crs(maraca_land))
Iemanja2_sf <- st_transform(Iemanja2_sf, crs = crs(maraca_land))

Iranildo_sf <- as.sf(DATA$Iranildo, crs = crs(maraca_land))
Iranildo_sf <- st_transform(Iranildo_sf, crs = crs(maraca_land))

Netuno_sf <- as.sf(DATA$Netuno, crs = crs(maraca_land))
Netuno_sf <- st_transform(Netuno_sf, crs = crs(maraca_land))


#Convert AKDEs to spatVectors format
Iara_akde <- as.sf(AKDEs$Iara, crs = crs(maraca_land))
Iara_akde <- vect(st_transform(Iara_akde, crs = crs(maraca_land)))[2]

ID696469B_akde <- as.sf(AKDEs$ID696469B, crs = crs(maraca_land))
ID696469B_akde <- vect(st_transform(ID696469B_akde, crs = crs(maraca_land)))[2]

ID696490B_akde <- as.sf(AKDEs$ID696490B, crs = crs(maraca_land))
ID696490B_akde <- vect(st_transform(ID696490B_akde, crs = crs(maraca_land)))[2]

ID717047B_akde <- as.sf(AKDEs$ID717047B, crs = crs(maraca_land))
ID717047B_akde <- vect(st_transform(ID717047B_akde, crs = crs(maraca_land)))[2]

Iemanja1_akde <- as.sf(AKDEs$Iemanja1, crs = crs(maraca_land))
Iemanja1_akde <- vect(st_transform(Iemanja1_akde, crs = crs(maraca_land)))[2]

Iemanja2_akde <- as.sf(AKDEs$Iemanja2, crs = crs(maraca_land))
Iemanja2_akde <- vect(st_transform(Iemanja2_akde, crs = crs(maraca_land)))[2]

Iranildo_akde <- as.sf(AKDEs$Iranildo, crs = crs(maraca_land))
Iranildo_akde <- vect(st_transform(Iranildo_akde, crs = crs(maraca_land)))[2]

Netuno_akde <- as.sf(AKDEs$Netuno, crs = crs(maraca_land))
Netuno_akde <- vect(st_transform(Netuno_akde, crs = crs(maraca_land)))[2]

#Convert AKDEs to spatRasterformat
Iara_PDF <- project(rast(raster(AKDEs$Iara, DF = "PMF")), maraca_land)
Iara_PDF[Iara_PDF == 0] <- NA
ID696469B_PDF <- project(rast(raster(AKDEs$ID696469B, DF = "PMF")), maraca_land)
ID696490B_PDF <- project(rast(raster(AKDEs$ID696490B, DF = "PMF")), maraca_land)
ID717047B_PDF <- project(rast(raster(AKDEs$ID717047B, DF = "PMF")), maraca_land)
Iemanja1_PDF <- project(rast(raster(AKDEs$Iemanja1, DF = "PMF")), maraca_land)
Iemanja2_PDF <- project(rast(raster(AKDEs$Iemanja2, DF = "PMF")), maraca_land)
Iranildo_PDF <- project(rast(raster(AKDEs$Iranildo, DF = "PMF")), maraca_land)
Netuno_PDF <- project(rast(raster(AKDEs$Netuno, DF = "PMF")), maraca_land)


#-------------------------------------------------------------
# Panel A - Tracking data
#-------------------------------------------------------------


A <-
ggplot() +
  ggtitle("A") +
  geom_spatraster(data = maraca_land, maxcell = 5e+07,
                  alpha = 0.7, aes(fill = class)) +
  
  geom_spatvector(data = maraca, col = "black", size = 0.1, fill = "transparent") +
  
  #Add in the tracking data
  geom_sf(data = Iara_sf, size = 0.1, alpha = 0.9, col = COLS[1], shape = 16) +
  geom_sf(data = ID696469B_sf, size = 0.1, alpha = 0.9, col = COLS[2], shape = 16) +
  geom_sf(data = ID696490B_sf, size = 0.1, alpha = 0.9, col = COLS[3], shape = 16) +
  geom_sf(data = ID717047B_sf, size = 0.1, alpha = 0.9, col = COLS[4], shape = 16) +
  geom_sf(data = Iemanja1_sf, size = 0.1, alpha = 0.9, col = COLS[5], shape = 16) +
  geom_sf(data = Iemanja2_sf, size = 0.1, alpha = 0.9, col = COLS[6], shape = 16) +
  geom_sf(data = Iranildo_sf, size = 0.1, alpha = 0.9, col = COLS[7], shape = 16) +
  geom_sf(data = Netuno_sf, size = 0.1, alpha = 0.9, col = COLS[8], shape = 16) +
  
  scale_fill_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water","Agriculture"),
                    values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a","#e9c46a"), 
                    name = "Land Class",
                    na.value = NA) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        #legend.position.inside = c(0.68,0.82),
        legend.title = element_text(size=8, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.title = element_text(hjust = .01, vjust = -6, size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
annotation_scale(height = unit(0.010, "npc"),
                 width_hint = 0.4,
                 line_width = 0.2,
                 pad_x = unit(0.07, "npc"),
                 pad_y = unit(0.07, "npc"),
                 text_pad = unit(0.01, "npc"),
                 text_cex = .5,
                 text_family = "sans",
                 text_face = "bold",
                 location = "tr")



#-------------------------------------------------------------
# Panel B - Home ranges
#-------------------------------------------------------------

B <- 
ggplot() +
  ggtitle("B") +
  geom_spatraster(data = maraca_land, maxcell = 5e+07,
                  alpha = 0.7, aes(fill = class)) +
  scale_fill_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water","Agriculture"),
                    values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a","#e9c46a"),
                    name = "Land Class",
                    na.value = NA) +
  
  geom_spatvector(data = maraca, col = "black", size = 0.1, fill = "transparent") +

  #Add in the HR contours
  geom_spatvector(data = Iara_akde, linewidth = 0.5, alpha = 0, col = COLS[1], fill = COLS[1]) +
  geom_spatvector(data = ID696469B_akde, linewidth = 0.5, alpha = 0, col = COLS[2], fill = COLS[2]) +
  geom_spatvector(data = ID696490B_akde, linewidth = 0.5, alpha = 0, col = COLS[3], fill = COLS[3]) +
  geom_spatvector(data = ID717047B_akde, linewidth = 0.5, alpha = 0, col = COLS[4], fill = COLS[4]) +
  geom_spatvector(data = Iemanja1_akde, linewidth = 0.5, alpha = 0, col = COLS[5], fill = COLS[5]) +
  geom_spatvector(data = Iemanja2_akde, linewidth = 0.5, alpha = 0, col = COLS[6], fill = COLS[6]) +
  geom_spatvector(data = Iranildo_akde, linewidth = 0.5, alpha = 0, col = COLS[7], fill = COLS[7]) +
  geom_spatvector(data = Netuno_akde, linewidth = 0.5, alpha = 0, col = COLS[8], fill = COLS[8]) +
  
  #Add in the PDFs
  # geom_spatraster(data = Iara_PDF) +
  # geom_spatvector(data = ID696469B_PDF, linewidth = 0.5, alpha = 0, col = COLS[2], fill = COLS[2]) +
  # geom_spatvector(data = ID696490B_PDF, linewidth = 0.5, alpha = 0, col = COLS[3], fill = COLS[3]) +
  # geom_spatvector(data = ID717047B_PDF, linewidth = 0.5, alpha = 0, col = COLS[4], fill = COLS[4]) +
  # geom_spatvector(data = Iemanja1_PDF, linewidth = 0.5, alpha = 0, col = COLS[5], fill = COLS[5]) +
  # geom_spatvector(data = Iemanja2_PDF, linewidth = 0.5, alpha = 0, col = COLS[6], fill = COLS[6]) +
  # geom_spatvector(data = Iranildo_PDF, linewidth = 0.5, alpha = 0, col = COLS[7], fill = COLS[7]) +
  # geom_spatvector(data = Netuno_PDF, linewidth = 0.5, alpha = 0, col = COLS[8], fill = COLS[8]) +



  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        #legend.position.inside = c(0.68,0.82),
        legend.title = element_text(size=8, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.title = element_text(hjust = .01, vjust = -4, size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))


# TOP <-
#   grid.arrange(A,B,
#                ncol=2,
#                nrow=1)


TOP <- ggarrange(A,B,
                 ncol=2, nrow=1,
                 common.legend = TRUE,
                 legend="top")

# #Save the figures
# ggsave(TOP,
#        width = 6.86*1.5, height = 6*1.5, units = "in",
#        dpi = 600,
#        bg = "transparent",
#        file="figures/hr_figure.png")



#-------------------------------------------------------------
# Panel C - Boxplots of Home-range size vs sex
#-------------------------------------------------------------

#Generate the figure
C <-
  ggplot(data = meta_data, aes(x = sex,
                               y = hr_est*1e-6,
                               col = sex,
                               fill = sex,
                               alpha = 0.5)) +
  ggtitle("C") +
  geom_boxplot(size = 0.1, outlier.size = 0.2, outlier.shape = 16, outlier.alpha = 0) +
  geom_jitter(size = 0.5, shape = 16, position=position_jitter(height=0, width=0.1)) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  ylab(expression(bold(Home~range~size~(km^2))))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
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
  scale_x_discrete(breaks = c("female", "male"), labels = c("Female","Male"))


#-------------------------------------------------------------
# Panel D - Scatterplot of Home-range size vs weight
#-------------------------------------------------------------

#Generate the figure
D <-
  ggplot(data = meta_data, aes(x = weight,
                               y = hr_est*1e-6,
                               col = sex,
                               fill = sex)) +
  ggtitle("D") +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "gam",
              formula = y ~ x,
              method.args = list(family = tw(link = "log")),
              col = "black",
              fill = "grey80",
              linewidth = 0.2,
              linetype = "dashed",
              alpha = 0.3) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  xlab(expression(bold(Weight~(kg))))+
  ylab(expression(bold(Home~range~size~(km^2))))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans", face = "bold", color = "black"),
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#-------------------------------------------------------------
# Panel E - Scatterplot of Home-range size vs weight
#-------------------------------------------------------------

#Generate the figure
E <-
  ggplot(data = meta_data, aes(x = age,
                               y = hr_est*1e-6,
                               col = sex,
                               fill = sex)) +
  ggtitle("E") +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "gam",
              formula = y ~ x,
              method.args = list(family = tw(link = "log")),
              col = "black",
              fill = "grey80",
              linewidth = 0.2,
              linetype = "dashed",
              alpha = 0.3) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  xlab(expression(bold(Age~(years))))+
  ylab(expression(bold(Home~range~size~(km^2))))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans", face = "bold", color = "black"),
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



BOT <-
  grid.arrange(C,D,E,
               ncol=3,
               nrow=1)


FIG <-
  ggarrange(TOP, BOT,
               ncol=1,
               nrow=2,
               heights = c(1.25,0.75))


#Save the figures
ggsave(FIG,
       width = 6.86*1.5, height = 6*1.5, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/hr_figure.png")


