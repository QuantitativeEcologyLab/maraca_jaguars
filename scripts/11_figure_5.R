#-------------------------------------------------------------
# Workspace and data preparation
#-------------------------------------------------------------

# Load in any of the necessary packages
library(ggplot2)
library(mgcv)
library(lubridate)
library(gratia)
library(gridExtra)

source("scripts/01_data_import.R")


#-------------------------------------------------------------
# Refit the diurnal activity model from script 05
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



#-------------------------------------------------------------
# Generate Figure 5 - Circadian Rhythms
#-------------------------------------------------------------


A <- 
  gratia::draw(circadian_fit,
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
        plot.title = element_text(vjust = 11, hjust = -0.05, size = 12, family = "sans", face = "bold"),
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
        plot.margin = unit(c(1.2,0.1,0.2,0.2), "cm")) +
  xlab("Time of day (Hrs)")


B <- 
  gratia::draw(circadian_fit,
               select = "s(hr_min_numeric,sex)",
               rug = F,
               caption = F) +
  ggtitle("B") +
  scale_colour_manual(values = c("#fca311", "#14213d"), labels = c("Female", "Male"), guide = "none") +
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
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=8, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  xlab("Time of day (Hrs)") +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))


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


ggsave(FIG, filename = "figures/figure_5.png",
       width = 6.86*1.5, height = 3.5, units = "in", dpi = 600)

