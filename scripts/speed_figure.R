library(ggplot2)
library(ctmm)
library(ggpubr)

#-------------------------------------------------------------
# Data import and pre-processing
#-------------------------------------------------------------
source("scripts/data_import.R")

#Extract speed, for each animal
meta_data$speed_est <- NA

for(i in 1:length(SPEEDS)){
  meta_data[meta_data$ID == names(SPEEDS)[i], "speed_est"] <- "km/day" %#% SPEEDS[[i]]$CI[2]
}

#Convert activity to a factor
speed_df$active <- as.factor(speed_df$active)

#Create a secondary dataset with speeds only while the animals were moving
moving_speeds <- na.omit(speed_df[speed_df$active == 1,])


#-------------------------------------------------------------
# Panel A - Boxplot of speed size vs sex
#-------------------------------------------------------------

#Generate the figure
A <-
  ggplot(data = meta_data, aes(x = sex,
                               y = speed_est,
                               col = sex,
                               fill = sex)) +
  ggtitle("A") +
  geom_boxplot(size = 0.1, outlier.size = 0.2, outlier.shape = 16, outlier.alpha = 0, alpha = 0.5) +
  geom_jitter(size = 0.5, shape = 16, position=position_jitter(height=0, width=0.1)) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  ylab(expression(bold(Mean~movement~speed~(km/day)))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=6, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold"),
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
# Panel B - Scatterplot of speed size vs weight
#-------------------------------------------------------------

#Generate the figure
B <-
  ggplot(data = meta_data, aes(x = weight,
                               y = speed_est,
                               col = sex,
                               fill = sex)) +
  ggtitle("B") +
  geom_smooth(method = "gam",
              formula = y ~ x,
              method.args = list(family = tw(link = "log")),
              col = "black",
              fill = "grey80",
              linewidth = 0.2,
              linetype = "dashed",
              alpha = 0.3) +
  geom_point(size = 1, shape = 16) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  xlab(expression(bold(Weight~(kg))))+
  ylab(expression(bold(Mean~movement~speed~(km/day)))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=6, family = "sans", face = "bold"),
        axis.title.x = element_text(size=6, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=5, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold"),
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
# Panel C - Scatterplot of speed vs. age
#-------------------------------------------------------------

#Generate the figure
C <-
  ggplot(data = meta_data, aes(x = age,
                               y = speed_est,
                               col = sex,
                               fill = sex)) +
  ggtitle("C") +
  geom_smooth(method = "gam",
              formula = y ~ x,
              method.args = list(family = tw(link = "log")),
              col = "black",
              fill = "grey80",
              linewidth = 0.2,
              linetype = "dashed",
              alpha = 0.3) +
  geom_point(size = 1, shape = 16) +
  scale_fill_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male")) +
  xlab(expression(bold(Age~(years))))+
  ylab(expression(bold(Mean~movement~speed~(km/day)))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=6, family = "sans", face = "bold"),
        axis.title.x = element_text(size=6, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=5, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold"),
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
# Panel D - Instantaneous movement speeds in the different habitats
#-------------------------------------------------------------


#Generate the figure
D <-
  ggplot(data = na.omit(speed_df), aes(x = class,
                                       group = active,
                                   fill = active)) +
  ggtitle("D") +
  geom_bar(position="fill") +
    scale_fill_manual(breaks = c("0","1"),
                      labels = c("Resting","Moving"),
                      values = c("#81b29a", "#e07a5f"), 
                      name = "Activity",
                      na.value = NA) +
  ylab(expression(bold(Proportion~of~locations))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 10, vjust = -4, family = "sans", face = "bold"),
        #strip.text.x = element_text(size=6, family = "sans", face = "bold", color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=5, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
    scale_y_continuous(expand = c(0,0.01)) +
    scale_x_discrete(expand = c(0,.5))


#-------------------------------------------------------------
# Panel E - Instantaneous movement speeds in the different habitats
#-------------------------------------------------------------

#Generate the figure
E <-
  ggplot(data = moving_speeds, aes(x = class,
                                   y = est,
                                   col = class,
                                   fill = class,
                                   alpha = 0.5)) +
  ggtitle("E") +
  geom_boxplot(size = 0.1, outlier.size = 0.2, outlier.shape = 16, outlier.alpha = 0) +
  geom_jitter(size = 0.1, shape = 16, position=position_jitter(height=0, width=0.1)) +
  scale_fill_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water","Agriculture"),
                    values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a","#e9c46a"), 
                    name = "Land Class",
                    na.value = NA) +
  scale_colour_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water","Agriculture"),
                      values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a","#e9c46a"), 
                      name = "Land Class",
                      na.value = NA) +
  ylab(expression(bold(Instantaneous~movement~speed~(m/s)))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans", face = "bold"),
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) #+ 
  # scale_y_log10() +
  # annotation_logticks(sides="l",
  #                     outside = F,
  #                     linewidth = 0.2,
  #                     short = unit(0.05, "cm"),
  #                     mid = unit(0.05, "cm"),
  #                     long = unit(0.1, "cm"))



TOP <-
  ggarrange(A,B,C,
            ncol=3,
            nrow=1)



FIG <-
  ggarrange(TOP, D, E,
            ncol=1,
            nrow=3,
            heights = c(0.75, 1.25, 1.25))


#Save the figures
ggsave(FIG,
       width = 6.86, height = 7, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/speed_figure.png")


