####### make maps with migration speed and direction for species and green up ########
# input files:


# load libraries ---------------------
library(tidyverse)
library(ggplot2)
library(viridis)

# source 'base' maps ----------------------
# create map
source("~/Documents/GitHub/BirdMigrationSpeed/map.R")
rm(list= ls()[!(ls() %in% c('rr', 'rrb', 'pp'))])

# load data  ------------
velB <- read_rds("data/velocityB.rds")

cellcoor <- read_rds("data/cellcoor.rds") 

velB <- left_join(velB, cellcoor, by = "cell")

sps <- "Vireo_olivaceus"   
year <- 2015

plot_mapvel <- function(sps, year, maptype){ # maptype = map hex hexn
  
  if(year == "all") {preds2 <- velB %>% 
    filter(species == sps)}
  
  if(year != "all") {preds2 <- velB %>% 
    filter(species == sps,
           year == year)}
  
  cell_sps <- sort(unique(velB$cell))
  
  preds2 <- preds2 %>% 
    mutate(
      # x = (cell_lng + 10) * cos(ang * pi / 180),
      # y = (cell_lat + 10) * sin(ang * pi / 180)
      x = cell_lng + 2 * cos(angB * pi / 180),
      y = cell_lat + 2 * sin(angB * pi / 180)
    )
  
  
  preds3 <- as.data.frame(matrix(data = NA, ncol = 8, nrow = length(cell_sps)))
  colnames(preds3) <- c("species","year","cell","cell_lng","cell_lat","x","y","mag")
  
  for(i in 1:length(cell_sps)){
    celll <- cell_sps[i]
    pred_loop <- preds2 %>% filter(cell == celll)
    if(nrow(pred_loop) == 1) {
      preds3$year[i] <- pred_loop$year
      preds3$cell[i] <- pred_loop$cell
      preds3$cell_lng[i] <- pred_loop$cell_lng
      preds3$cell_lat[i] <- pred_loop$cell_lat
      preds3$x[i] <- pred_loop$x
      preds3$y[i] <- pred_loop$y
      preds3$mag[i] <- pred_loop$vArrMag
    } else {
      preds3$year[i] <- pred_loop$year[1]
      preds3$cell[i] <- pred_loop$cell[1]
      preds3$cell_lng[i] <- pred_loop$cell_lng[1]
      preds3$cell_lat[i] <- pred_loop$cell_lat[1]
      preds3$x[i] <- mean(x = pred_loop$x)
      preds3$y[i] <- mean(x = pred_loop$y)
      preds3$mag[i] <-  mean(x = pred_loop$vArrMag, na.rm = T)
    }
  }
  
  preds3$species <- sps
  
  preds4 <- preds3 %>% 
    filter(mag < 9999,
           !is.na(year))
  
  if(maptype == "map") {base_map <- rr}
    
  if(maptype == "hex") {base_map <- rrb}
    
  if(maptype == "hexn") {base_map <- pp}
      
  base_map +
    geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell, colour = log(mag)),
                 arrow = arrow(length = unit(0.1, "cm")), size = 1.2) +
    scale_colour_continuous(type = "viridis", trans = "log"
                            ) +
    #scale_fill_binned(type = "viridis") +
    #scale_colour_gradientn(colors=viridis(3)) +
    ggtitle(glue("{sps} {year}")) +
    theme_bw() +
    theme(#legend.position = "none",
          panel.grid.major = element_line(color = alpha('black', 0.2),
                                          size = 0.5),
          panel.ontop = TRUE,
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = NA),
          legend.title=element_text(size=13),
          legend.spacing.y = grid::unit(0.5, "cm"),
          legend.text=element_text(size=rel(0.8)),
          legend.key.height=grid::unit(0.9,"cm"),
          legend.title.align=0.5,
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.border = element_blank()) 
}

plot_mapvel("Tachycineta_bicolor", "all", "hex")
plot_mapvel("Vireo_olivaceus", "all", "hex")
plot_mapvel("Setophaga_americana", "all", "hex")
plot_mapvel("Setophaga_discolor", "all", "hex")

## plot hexagons within the distribution of a species

