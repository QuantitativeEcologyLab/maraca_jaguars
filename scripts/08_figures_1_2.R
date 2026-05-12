#Import the packages
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(terra)
library(sf)
library(ctmm)
library(ggpubr)
library(mgcv)
library(rnaturalearth)
library(rnaturalearthdata)

#-------------------------------------------------------------
# Data import and pre-processesing
#-------------------------------------------------------------
source("scripts/01_data_import.R")


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


#-------------------------------------------------------------
# Panel A - Tracking data
#-------------------------------------------------------------

#Get the contours for Brazil
world <- ne_countries(scale = "medium", returnclass = "sv")
world <- project(world, "+proj=moll")
brasil <- world[world$name == "Brazil",]

bras_inset <-
  ggplot() +
  
  geom_spatvector(data = world) +
    geom_spatvector(data = brasil, col = "black", size = 0.1, fill = "#009440") +

  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        legend.title = element_text(size=8, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.title = element_text(hjust = .01, vjust = -6, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
    coord_sf(xlim = c(-8200000, -3300000), ylim = c(-4500000, 850000), expand = FALSE)



fig <-
ggplot() +
  annotation_map_tile(
    type = "cartolight", zoomin = 0
  ) +
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
        #panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        #legend.position.inside = c(0.68,0.82),
        legend.title = element_text(size=8, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.title = element_text(hjust = .01, vjust = -6, size = 14, family = "sans", face = "bold"),
        #axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.text.x  = element_blank(),
        #axis.ticks = element_blank(),
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
                 location = "tr") +
  scale_x_continuous(expand = expansion(mult = 0.25), n.breaks = 4) +
  scale_y_continuous(expand = expansion(mult = 0.2), n.breaks = 4) +
  coord_sf()



#Save the figures
ggsave(bras_inset,
       width = 6.86*1.5, height = 6*1.5, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/figure_1_inset.png")


#Save the figures
ggsave(fig,
       width = 6.86*1.5, height = 6*1.5, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/figure_1.png")


#-------------------------------------------------------------
# Panel A - Home ranges
#-------------------------------------------------------------

A <- 
ggplot() +
  ggtitle("A") +
  annotation_map_tile(
    type = "cartolight", zoomin = 0
  ) +
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
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        legend.title = element_text(size=8, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.title = element_text(hjust = .01, vjust = -4, size = 14, family = "sans", face = "bold"),
        #axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.text.x  = element_blank(),
        #axis.ticks = element_blank(),
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
# Panel B - Boxplots of Home-range size vs sex
#-------------------------------------------------------------

#Generate the figure
B <-
  ggplot(data = meta_data, aes(x = sex,
                               y = hr_est*1e-6,
                               col = sex,
                               fill = sex,
                               alpha = 0.5)) +
  ggtitle("B") +
  geom_boxplot(size = 0.1, outlier.size = 0.2, outlier.shape = 16, outlier.alpha = 0) +
  geom_jitter(size = 1, shape = 16, position=position_jitter(height=0, width=0.1)) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  ylab(expression(bold(Home-range~size~(km^2))))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
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
# Panel C - Scatterplot of Home-range size vs weight
#-------------------------------------------------------------

#Generate the figure
C <-
  ggplot(data = meta_data, aes(x = weight,
                               y = hr_est*1e-6,
                               col = sex,
                               fill = sex)) +
  ggtitle("C") +
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
  ylab(expression(bold(Home-range~size~(km^2))))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
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
# Panel D - Scatterplot of Home-range size vs age
#-------------------------------------------------------------

#Generate the figure
D <-
  ggplot(data = meta_data, aes(x = age,
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
  xlab(expression(bold(Age~(years))))+
  ylab(expression(bold(Home-range~size~(km^2))))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
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



#Combine and save
right <-
  ggarrange(B,C,D,
            ncol=1,
            nrow=3)


FIG <-
  ggarrange(A, right,
            ncol=2,
            nrow=1,
            widths = c(1.25,0.75))


#Save the figures
ggsave(FIG,
       width = 6.86*1.5, height = 5*1.5, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/figure_2.png")







#-------------------------------------------------------------
# Supplementary Figure S2 - Individual Home Ranges
#-------------------------------------------------------------


#Colour pallet for the individual animals
COLS <- c("#001427",
          "#fca311",
          "#ffd60a",
          "#a34e97",
          "#7400b8",
          "#b85236",
          "#c32f27",
          "#f72585")

names(COLS) <- c("Iara","ID696469B","ID696490B","ID717047B","Iemanja1","Iemanja2","Iranildo","Netuno")


png("figures/figure_S2.png", width = 6.86*1.5, height = 10*1.5, units = "in", res = 600)


par(mfrow = c(4,2))

for(i in 1:nrow(meta_data)){
  plot(DATA[meta_data$ID[i]],
       UD = AKDEs[meta_data$ID[i]],
       col = COLS[meta_data$ID[i]],
       col.DF = ifelse(meta_data[meta_data[,"ID"] == meta_data$ID[i],"sex"] == "male", "#fca311", "#14213d"),
       main = paste(meta_data$ID[i], sep = ""),
       col.bg="transparent",
       col.grid="transparent",
       level=NA,
       labels = NA)
}
dev.off()
