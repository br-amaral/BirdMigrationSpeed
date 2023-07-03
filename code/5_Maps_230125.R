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
plot_mapvel2 <- function(sps, year, rang){
  
  preds <- final2
  
  if(sps == "all") {preds2 <- preds}
  if(sps != "all") {preds2 <- preds %>% filter(species == sps)}
  
  if(year != "all") {preds2 <- preds2 %>% filter(year == year)}
  
  if(rang == "bre"){ preds2 <- preds2[which(preds2$breed_cell==TRUE),]}
  if(rang == "mig"){ preds2 <- preds2[which(preds2$mig_cell==TRUE),]}
  
  mean_cell_speed <- preds2 %>% 
    group_by(cell) %>% 
    summarise(mean_cell_speed = mean(vArrMag)) 
  
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
    p_scale <- c(40, 50, 60, 70); p_limits <- c(30,90) #} else {
  
    #p_scale <- c(10, 25, 100,1000); p_limits <- c(1.96,7.27)}
  
  pp +
    geom_polygon(data = cell_grid2, aes(x = long, 
                                        y = lat, group = group, 
                                        fill = log(mean_cell_speed)), alpha = 0.7) +
    geom_path(data = cell_grid2, aes(x = long, 
                                     y = lat, group = group), 
              alpha = 0.4, color = 'black') +
    scale_fill_viridis(option="magma",
                       trans='log10', 
                       limits = log(p_limits),
                       breaks = log(p_scale),
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
    labs(fill = 'Bird Migratory Speed \n(km/day (log scale))')
}

svg(glue("figures/Fig1/fig1_map.svg"), 
    width = 6, height = 4)
plot_mapvel2(sps = "all", year = "all", rang = "all")
dev.off()

plot_mapvel2(sps = "Setophaga_magnolia", year = "all", rang = "all")
plot_mapvel2(sps = "Setophaga_americana", year = "all", rang = "all")
plot_mapvel2(sps = "Setophaga_petechia", year = "all", rang = "all")

# develop specie specific values for scale?