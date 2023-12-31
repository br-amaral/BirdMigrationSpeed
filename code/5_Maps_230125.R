####### make maps with migration speed and direction for species and green up ########
# input files:


# load libraries ---------------------
library(tidyverse)
library(ggplot2)
library(viridis)
library("RColorBrewer")
library(glue)
library(viridis)
library(scales) # histogram

# load data and source 'base' maps ----------------------
source("code/map.R")
rm(list= ls()[!(ls() %in% c('rr', 'rrb', 'pp'))])

load("data/species_Grid.RData")
cell_grid_tab4 <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/MigratorySensitivity//Data/master_cell_grid.rds") ## load grid - package not on CRAN

final2 <- readRDS("~/OneDrive/BirdMigrationSpeed_copy/final.rds") %>% 
  mutate(species = as.factor(species), 
         cell = as.factor(cell),
         #mig_cell = abs(mig_cell - 1),
         mig_cell = as.factor(mig_cell),
         sps_cell = as.factor(glue("{species}_{cell}"))) %>% 
  filter(!is.na(vArrMag))

spskey <- read_csv("data/source/spskey.csv")

cell_grid_master <- read_rds(file = "data/cell_grid_master.rds")

# individual species velocity maps --------------------------------
# no arrow
plot_mapvel2 <- function(sps, year, rang, pllot) {
  
  ## plot one ---------------
  
    preds <- final2
    
    if(sps == "all") {preds2 <- preds}
    if(sps != "all") {preds2 <- preds %>% filter(species == sps)}
    
    if(year != "all") {preds2 <- preds2 %>% filter(year == year)}
    
    if(rang == "bre"){ preds2 <- preds2[which(preds2$breed_cell==TRUE),]}
    if(rang == "mig"){ preds2 <- preds2[which(preds2$mig_cell==TRUE),]}
    
    mean_cell_speed <- preds2 %>% 
      group_by(cell) %>% 
      summarise(mean_cell_speed = median(vArrMag)) 
    
    if(sps %in% spskey$sci_name) {
    cell_grid <- get(paste('cell_grid', 
                           as.character(spskey[which(spskey$sci_name == sps),1]), 
                           sep="_")) %>% 
      mutate(cell = as.factor(cell)) } else {
        cell_grid <- cell_grid_master %>% 
          mutate(cell = as.factor(cell))
      }
    
    cell_grid2 <- left_join(cell_grid, mean_cell_speed, by = c("cell"), relationship = "many-to-many")
    
    #MIN <- round((floor((min(cell_grid2$mag_s, na.rm = TRUE))*10)/10), 1)
    #MAX <- round((ceiling((max(cell_grid2$mag_s, na.rm = TRUE))*10)/10), 1)
    
    # if(sps == "all") {
    p_scale <- c(40, 50, 60, 70); p_limits <- c(32,71) #} else {
    
      #p_scale <- c(10, 25, 100,1000); p_limits <- c(1.96,7.27)}
    if(pllot == 1) {
      return_plot <- 
      pp +
      geom_polygon(data = cell_grid2 %>% filter(mean_cell_speed > 0), 
                   aes(x = long, 
                                          y = lat, group = group, 
                                          fill = mean_cell_speed), alpha = 0.7) +
      geom_path(data = cell_grid2 %>% filter(mean_cell_speed > 0), aes(x = long, 
                                       y = lat, group = group), 
              alpha = 0.4, color = 'black') +
      scale_fill_viridis(option="magma",
                         #trans='log10', 
                         limits = p_limits,
                         breaks = p_scale,
                         labels = p_scale) +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
            panel.grid.major = element_line(color = alpha('black', 0.2), size = 0.5),
            panel.ontop = TRUE,
            legend.title=element_text(size=10.5),
            legend.text=element_text(size=10),
            panel.background = element_rect(fill = NA),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      xlab('') +
      ylab('') +
      labs(fill = 'Median Bird\nMigratory Speed \n(km/day)') +
        coord_map("ortho", orientation = c(35, -80, 0),
                  xlim = c(-95, -67), ylim = c(25, 50)) 
  }
  
  ## plot two ---------------
  if(pllot == 2) {
    
    preds2 <- final2 
    
    cell_sps <- sort(unique(preds2$cell))
    
    preds2 <- preds2 %>% 
      filter(!is.na(cell_lat2),
             !is.na(cell_lng)) %>% 
      mutate(
        # x = (cell_lng + 10) * cos(ang * pi / 180),
        # y = (cell_lat + 10) * sin(ang * pi / 180)
        x = cell_lng + 2 * cos(angB * pi / 180),
        y = cell_lat2 + 2 * sin(angB * pi / 180)
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
        preds3$cell_lat[i] <- pred_loop$cell_lat2
        preds3$x[i] <- pred_loop$x
        preds3$y[i] <- pred_loop$y
        preds3$mag[i] <- pred_loop$vArrMag
      } else {
        preds3$year[i] <- pred_loop$year[1]
        preds3$cell[i] <- pred_loop$cell[1]
        preds3$cell_lng[i] <- pred_loop$cell_lng[1]
        preds3$cell_lat[i] <- pred_loop$cell_lat2[1]
        preds3$x[i] <- median(x = pred_loop$x)
        preds3$y[i] <- median(x = pred_loop$y)
        preds3$mag[i] <-  median(x = pred_loop$vArrMag, na.rm = T)
      }
    }
    
    preds4 <- preds3 %>% 
      filter(mag < 9999,
             !is.na(year))
    
    cell_grid3 <- cell_grid2 %>% filter(mean_cell_speed > 0)
    cell_grid3$mean_cell_speed2 <- NA

    return_plot <- 
    pp +
      geom_polygon(data = cell_grid3, 
                   aes(x = long, 
                      y = lat, group = group), alpha = 0.3) +
      geom_path(data = cell_grid3,
                aes(x = long, 
                                       y = lat, group = group), 
                alpha = 0.3, color = 'black') +
      # scale_fill_viridis(option="magma",
      #                    trans='log10', 
      #                    limits = log(p_limits),
      #                    breaks = log(p_scale),
      #                    labels = p_scale) +
      theme_bw() +
      theme(
        panel.grid.major = element_line(color = alpha('black', 0.2),
                                        size = 0.5),
        panel.ontop = TRUE,
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank()) +
      geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
                                      xend = x, yend = y,
                                      group = cell),
                   arrow = arrow(length = unit(0.175, "cm")), size = 0.8) +
      coord_map("ortho", orientation = c(35, -80, 0),
                xlim = c(-95, -67), ylim = c(25, 50))  

    
  }
  return(return_plot)
}

svglite::svglite(glue("figures/Fig1/fig1a_map.svg"), 
    width = 6, height = 4)
plot_mapvel2(sps = "all", year = "all", rang = "all", pllot = 1)
dev.off()

svglite::svglite(glue("figures/Fig1/fig1b_map.svg"), 
    width = 6, height = 4)
plot_mapvel2(sps = "all", year = "all", rang = "all", pllot = 2)
dev.off()

plot_mapvel2(sps = "Setophaga_magnolia", year = "all", rang = "all", pllot = 1)
plot_mapvel2(sps = "Setophaga_americana", year = "all", rang = "all", pllot = 1)
plot_mapvel2(sps = "Setophaga_petechia", year = "all", rang = "all", pllot = 1)

# develop specie specific values for scale?