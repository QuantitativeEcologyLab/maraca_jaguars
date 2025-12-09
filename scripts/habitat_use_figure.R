library(ctmm)
library(terra)
library(weights)

#-------------------------------------------------------------
# Data import and pre-processing
#-------------------------------------------------------------

source("scripts/data_import.R")
source("scripts/functions.R")

#-------------------------------------------------------------
# Extract land use
#-------------------------------------------------------------

# Note: these rasters are not on GitHub

RES <- list()

# Then walk through each individuals for that species
for(i in 1:length(DATA)){
  
  # Generate a brief message to keep track of progress
  cat("Working on individual ", i, " of ", length(DATA), "\n")
  
  
  #Import the HR estimate for the ith animal
  PATH <- file.path("results/akdes",
                    paste("AKDE_",
                          DATA[[i]]@info[1],
                          ".rda",
                          sep = ""))
  load(PATH)
  
  
  #Data carpentry to get the home range PDF into the correct format for extracting values
  HR <- rast(raster(AKDE, DF = "PMF"))
  HR <- project(HR, crs(maraca_land), res = res(maraca_land))
  HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
  #Renormalize
  HR.df$layer <- HR.df$layer/sum(HR.df$layer)
  
  
  #Extract habitat values
  HR.df$land_class <- extract(maraca_land, HR.df[,1:2])[,2]
  
  # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
  PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
  PROPS2 <- data.frame(class = names(PROPS),
                       proportion = as.numeric(PROPS))
  PROPS <- data.frame(t(PROPS2))[2,]
  names(PROPS) <- PROPS2$class
  
  
  res <- data.frame(binomial = "Panthera_onca")
  res$ID <- AKDE@info$identity
  res <- cbind(res,PROPS)
  
  RES[[i]] <- res
  
} # Closes the loop that runs over the telemetry object (i.e., i)


res <- do.call(dplyr::bind_rows, RES)
res[is.na(res)] <- 0

# Save the land use data as a csv
write.table(res,
            file = "results/jaguar_habitat_use.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")





#-------------------------------------------------------------
# Figure S3 - Proportion of HR PDF in the different habitats
#-------------------------------------------------------------

res_long <- reshape(
  res[,2:7],
  direction = "long",
  varying = names(res[,2:7])[names(res[,2:7]) != "ID"],
  v.names = "Value",
  timevar = "Habitat",
  times = names(res[,2:7])[names(res[,2:7]) != "ID"]
)
row.names(res_long) <- NULL; res_long[,4] <- NULL
res_long$Value <- as.numeric(res_long$Value)

#Generate the figure
S3 <-
  ggplot(data = res_long, aes(x = Habitat,
                                   y = Value,
                                   col = Habitat,
                                   fill = Habitat,
                                   alpha = 0.5)) +
  geom_boxplot(size = 0.1, outlier.size = 0.2, outlier.shape = 16, outlier.alpha = 0) +
  geom_jitter(size = 1, shape = 16, position=position_jitter(height=0, width=0.1)) +
  scale_fill_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water"),
                    values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a"), 
                    name = "Land Class",
                    na.value = NA) +
  scale_colour_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water"),
                      values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a"), 
                      name = "Land Class",
                      na.value = NA) +
  ylab(expression(bold(Weighted~habitat~use))) +
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



#Save the figures
ggsave(S3,
       width = 6.86, height = 4, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/Figure_S3.png")

