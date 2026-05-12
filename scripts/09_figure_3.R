# Load in any of the necessary packages
library(ggplot2)
library(metafor)
library(lubridate)
library(gridExtra)
library(ggpubr)

#-------------------------------------------------------------
# Data import and pre-processesing
#-------------------------------------------------------------

source("scripts/01_data_import.R")

#Identify encounters and aggregate by day of year
#set threshold of 100m
distance_df$encounter <- ifelse(distance_df$low > 100, 0,1)
encounter_df <- distance_df[which(distance_df$encounter == 1),]
encounter_df$doy <- yday(encounter_df$timestamp) #day of the year
encounter_df$month <- month(encounter_df$timestamp, label = TRUE)
encs <- aggregate(encounter ~ pair + doy + month, data = encounter_df, FUN = "sum")



#-------------------------------------------------------------
# Panel A Home-range overlap and sexes
#-------------------------------------------------------------

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



#-------------------------------------------------------------
# Panel B Proximity ratios
#-------------------------------------------------------------

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


#-------------------------------------------------------------
# Panel C Encounters over time
#-------------------------------------------------------------

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


ggsave(FIG, filename = "figures/figure_3.png",
       width = 6.86, height = 4.5, units = "in", dpi = 600)




#-------------------------------------------------------------
# Supplementary Figure S3 - Pairwise overlap
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
  

png("figures/figure_S3.png", width = 6.86*1.5, height = 10*1.5, units = "in", res = 600)


par(mfrow = c(5,2))

for(i in 1:nrow(pairs)){
plot(DATA[c(pairs$jag_1[i], pairs$jag_2[i])],
  UD = AKDEs[c(pairs$jag_1[i], pairs$jag_2[i])],
  col = c(COLS[pairs$jag_1[i]], COLS[pairs$jag_2[i]]),
     col.DF = c(ifelse(meta_data[meta_data[,"ID"] == pairs$jag_1[i],"sex"] == "male", "#fca311", "#14213d"),
                ifelse(meta_data[meta_data[,"ID"] == pairs$jag_2[i],"sex"] == "male", "#fca311", "#14213d")),
     main = paste(pairs$jag_1[i], " - ", pairs$jag_2[i], "; Overlap = ", round(pairs[i,"overlap"],3), sep = ""),
     col.bg="transparent",
     col.grid="transparent",
     level=NA,
  labels = NA)
}
dev.off()