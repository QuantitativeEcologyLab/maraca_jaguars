# Load in any of the necessary packages
library(ggplot2)
library(metafor)
library(scico)

source("scripts/data_import.R")

# First build dataset of observed locations

rsf_df <- list()
for(i in 1:length(DATA)){
  
  #First build a dataframe with coordinates and individual ID 
  rsf_df_i <- data.frame(ID = DATA[[i]]@info$identity,
                         x = DATA[[i]]$x,
                         y = DATA[[i]]$x)
  
  #Then get habitat information
  cilla_sf <- as.sf(DATA[[i]])
  rsf_df_i$forest <- extract(forest, cilla_sf)[,2]
  rsf_df_i$mangrove <- extract(mangrove, cilla_sf)[,2]
  rsf_df_i$wetland <- extract(wetland, cilla_sf)[,2]  
  rsf_df_i$grassland <- extract(wetland, cilla_sf)[,2]
  rsf_df_i$dist_to_coast <- extract(dist_to_coast, cilla_sf)[,2]
  rsf_df_i$weight <- AKDEs[[i]]$weights
  rsf_df_i$detection <- 1
  
  #Then build an individual specific dataset of quadrature points
  quadrature_i <- terra::as.data.frame(forest, xy = TRUE)[,c("x","y")]
  quadrature_i$forest <- extract(forest, quadrature_i[,c("x","y")])[,2]
  quadrature_i$mangrove <- extract(mangrove, quadrature_i[,c("x","y")])[,2]
  quadrature_i$wetland <- extract(wetland, quadrature_i[,c("x","y")])[,2]
  quadrature_i$grassland <- extract(grassland, quadrature_i[,c("x","y")])[,2]
  quadrature_i$dist_to_coast <- extract(dist_to_coast, quadrature_i[,c("x","y")])[,2]
  quadrature_i$weight <- 1
  quadrature_i$ID <- DATA[[i]]@info$identity
  quadrature_i$detection <- 0
  quadrature_i <- quadrature_i[,names(rsf_df_i)]
  
  #Combine and store in main list
  rsf_df_i <- rbind(rsf_df_i, quadrature_i)
  rsf_df[[i]] <- rsf_df_i
}

#Some final data carpentry
rsf_df <- do.call(rbind, rsf_df)
rsf_df$ID <- as.factor(rsf_df$ID)
rsf_df <- na.omit(rsf_df)
rsf_df$forest <- as.factor(rsf_df$forest)
rsf_df$mangrove <- as.factor(rsf_df$mangrove)
rsf_df$wetland <- as.factor(rsf_df$wetland)
rsf_df$grassland <- as.factor(rsf_df$grassland)

#Fit the RSF
RSF <-
  bam(detection ~
        s(ID, bs = 're') +
        s(log(dist_to_coast+1),k = 5) +
        s(log(dist_to_coast+1), forest, bs = "sz") +
        s(log(dist_to_coast+1), mangrove, bs = "sz") +
        s(log(dist_to_coast+1), wetland, bs = "sz") +
        s(log(dist_to_coast+1), grassland, bs = "sz"),
      family = poisson(link = 'log'), 
      data = rsf_df,  
      weights = weight,
      method = 'fREML',
      discrete = TRUE)

summary(RSF)
plot(RSF, pages = 1)


#Then build a dataset for generating the predictions
pred_df <- terra::as.data.frame(forest, xy = TRUE)[,c("x","y")]
pred_df$forest <- extract(forest, pred_df[,c("x","y")])[,2]
pred_df$mangrove <- extract(mangrove, pred_df[,c("x","y")])[,2]
pred_df$wetland <- extract(wetland, pred_df[,c("x","y")])[,2]
pred_df$grassland <- extract(grassland, pred_df[,c("x","y")])[,2]
pred_df$dist_to_coast <- extract(dist_to_coast, pred_df[,c("x","y")])[,2]
pred_df$ID <- as.factor("Iara")
pred_df$forest <- as.factor(pred_df$forest)
pred_df$mangrove <- as.factor(pred_df$mangrove)
pred_df$wetland <- as.factor(pred_df$wetland)
pred_df$grassland <- as.factor(pred_df$grassland)

pred_df$lambda <- pred <- predict(RSF, newdata = pred_df,
                                  exclude = c("(Intercept)","s(ID)"),
                                  type = "response")


pred_df$lambda[pred_df$lambda > quantile(pred_df$lambda, .99)] <- quantile(pred_df$lambda, .99)
pred_df$lambda[pred_df$lambda < quantile(pred_df$lambda, .01)] <- quantile(pred_df$lambda, .01)
pred_df$lambda <- pred_df$lambda/max(pred_df$lambda)

rsf_map <- rast(pred_df[c("x","y","lambda")], type="xyz")

plot(rsf_map)







#-------------------------------------------------------------
# Panel A - RSF Map
#-------------------------------------------------------------


A <-
  ggplot() +
  ggtitle("A") +
  geom_spatraster(data = rsf_map, maxcell = 5e+07,
                  alpha = 0.9, aes(fill = lambda)) +
  
  geom_spatvector(data = maraca, col = "black", size = 0.1, fill = "transparent") +
  
  scale_fill_scico(palette = 'batlow',
                   na.value = NA,
                   name = expression(bold(Scaled~lambda))) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        #legend.position.inside = c(0.68,0.82),
        legend.title = element_text(size=6, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=4, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        plot.title = element_text(hjust = .01, vjust = -6, size = 8, family = "sans", face = "bold"),
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
                   location = "tr") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 10,
                               barheight = 0.2, direction = "horizontal"))




#-------------------------------------------------------------
# Panel B - Habitat selection
#-------------------------------------------------------------


forest_df <- data.frame(class = "Forest",
                        mangrove = 0,
                        forest = 1,
                        wetland = 0,
                        grassland = 0,
                        dist_to_coast = seq(0.1, 4000, by = 0.01),
                        ID = as.factor("Iara"))

mangrove_df <- data.frame(class = "Mangrove",
                          mangrove = 1,
                          forest = 0,
                          wetland = 0,
                          grassland = 0,
                          dist_to_coast = seq(0.1, 4000, by = 0.01),
                          ID = as.factor("Iara"))

wetland_df <- data.frame(class = "Wetland",
                         mangrove = 0,
                         forest = 0,
                         wetland = 1,
                         grassland = 0,
                         dist_to_coast = seq(0.1, 4000, by = 0.01),
                         ID = as.factor("Iara"))

grassland_df <- data.frame(class = "Grassland",
                           mangrove = 0,
                           forest = 0,
                           wetland = 0,
                           grassland = 1,
                           dist_to_coast = seq(0.1, 4000, by = 0.01),
                           ID = as.factor("Iara"))

pred_df <- rbind(forest_df, mangrove_df, wetland_df, grassland_df)

pred_df$lambda <- predict(RSF, newdata = pred_df,
                          exclude = c("(Intercept)","s(ID)"),
                          type = "response")

pred_df$lambda <- pred_df$lambda/max(pred_df$lambda)

#Generate the figure
B <-
ggplot() +
  ggtitle("B") +
  geom_line(data = pred_df,
            aes(x = dist_to_coast,
                y = lambda,
                col = class),
            size = .6) +
  scale_colour_manual(breaks = c("Forest","Mangrove","Wetland","Grassland","Water","Agriculture"),
                      values = c("#004b23", "#001524", "#168aad", "#99d98c","#023e8a","#e9c46a"), 
                      name = "Land Class",
                      na.value = NA) +
  xlab(expression(bold(Distance~to~coast~(m))))+
  ylab(expression(bold(Scaled~lambda))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=6, family = "sans", face = "bold"),
        axis.title.x = element_text(size=6, family = "sans", face = "bold", color = "black"),
        axis.text.y = element_text(size=5, family = "sans"),
        axis.text.x  = element_text(size=5, family = "sans", face = "bold", color = "black"),
        plot.title = element_text(hjust = -0.05, size = 8, family = "sans", face = "bold"),
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
  scale_x_log10(breaks = c(0.001,0.01,0.1,1,10,100,1000,10000),
                labels = c(0,0.01,0.1,1,10,100,1000,10000),
                expand = c(0,0)) +
  annotation_logticks(sides="b",
                      outside = F,
                      linewidth = 0.2,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm"))




FIG <-
  ggarrange(A, B,
            ncol=1,
            nrow=2,
            heights = c(1.25,0.75))


#Save the figures
ggsave(FIG,
       width = 3.23, height = 6, units = "in",
       dpi = 600,
       bg = "transparent",
       file="figures/rsf_figure.png")
