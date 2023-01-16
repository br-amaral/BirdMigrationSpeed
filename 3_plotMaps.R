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
source("~/Documents/GitHub/BirdMigrationSpeed/map.R")
rm(list= ls()[!(ls() %in% c('rr', 'rrb', 'pp'))])
cellcoor <- read_rds("data/cellcoor.rds") 
final <- read_rds(file = "data/final.rds")
annual <- read_rds(file = "data/annual.rds")
velocityB <- read_rds(file = "data/velocityB.rds")


velocityB <- read_rds("data/velocityB.rds")
predsG <- read_rds("data/cellaveG.rds")
load("~/Library/Mobile Documents/com~apple~CloudDocs/MigratorySensitivity/Data/species_Grid.RData")
spskey <- read_csv("data/source/spskey.csv")
cells <- read_rds("data/cellcoor.rds")

velocityB <-left_join(velocityB, cells, by = "cell")
colnmaes <- colnames
# individual species velocity maps --------------------------------

plot_mapvel2 <- function(sps, year){
  
  preds <- velocityB
  
  if(year == "all") {preds2 <- preds %>% 
    filter(species == sps)}
  
  if(year != "all") {preds2 <- sps %>% 
    filter(species == sps,
           year == year)}
  
  cell_sps <- sort(unique(preds2$cell))
  
  preds2 <- preds2 %>% 
    filter(vArrMag < 10000) %>% 
    mutate(
      # x = (cell_lng + 10) * cos(ang * pi / 180),
      # y = (cell_lat + 10) * sin(ang * pi / 180)
      x = cell_lng + 3 * cos(angB * pi / 180),
      y = cell_lat + 3 * sin(angB * pi / 180)
    ) %>% 
    mutate(vArrMag_s = scale(vArrMag))
  
  preds3 <- as.data.frame(matrix(data = NA, ncol = 8, nrow = length(cell_sps)))
  colnames(preds3) <- c("species","year","cell","cell_lng","cell_lat","x","y","mag_s")
  
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
      preds3$mag_s[i] <- pred_loop$vArrMag_s
      
    } else {
      preds3$year[i] <- pred_loop$year[1]
      preds3$cell[i] <- pred_loop$cell[1]
      preds3$cell_lng[i] <- pred_loop$cell_lng[1]
      preds3$cell_lat[i] <- pred_loop$cell_lat[1]
      preds3$x[i] <- mean(x = pred_loop$x)
      preds3$y[i] <- mean(x = pred_loop$y)
      preds3$mag_s[i] <- mean(pred_loop$vArrMag_s)
    }
  }
  
  preds3$species <- sps
  
  cell_grid <- get(paste('cell_grid', 
                         as.character(spskey[which(spskey$sci_name == sps),1]), 
                         sep="_")) 
  
  cell_grid2 <- left_join(cell_grid, preds3, by = "cell")
  
  MIN <- round((floor((min(cell_grid2$mag_s, na.rm = TRUE))*10)/10), 1)
  MAX <- round((ceiling((max(cell_grid2$mag_s, na.rm = TRUE))*10)/10), 1)
  
  
  #rr +
  #rrb + 
  pp +
    geom_polygon(data = cell_grid2, aes(x = long, 
                                        y = lat, group = group, 
                                        fill = mag_s), alpha = 0.8
                 ) +
    geom_path(data = cell_grid2, aes(x = long, 
                                     y = lat, group = group), 
              #alpha = 0.4, 
              color = 'darkgrey') +
    geom_segment(data = preds3, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell), colour = "black",
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
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
      legend.text=element_text(size=rel(1.2)),
      legend.key.height=grid::unit(0.9,"cm"),
      legend.title.align=0.5,
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.border = element_blank()) +
    scale_fill_viridis(option="magma",limits = c(MIN, MAX), na.value="white") 
  
  
  pp +
    geom_polygon(data = cell_grid2, aes(x = long, 
                                        y = lat, group = group, 
                                        fill = mag_s), alpha = 0.4
    ) +
    geom_path(data = cell_grid2, aes(x = long, 
                                     y = lat, group = group), 
              #alpha = 0.4, 
              color = 'black') +
    geom_segment(data = preds3, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell), colour = "black",
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
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
      legend.text=element_text(size=rel(1.2)),
      legend.key.height=grid::unit(0.9,"cm"),
      legend.title.align=0.5,
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.border = element_blank()) +
    scale_fill_viridis(option="magma",limits = c(MIN, MAX)#, na.value="white"
                       ) 

  pp +
    geom_polygon(data = cell_grid2, aes(x = long, 
                                    y = lat, group = group, 
                                    fill = mag_s), alpha = 0.6) +
    geom_path(data = cell_grid2, aes(x = long, 
                                 y = lat, group = group), 
              alpha = 0.4, color = 'black') +
    scale_fill_gradientn(trans='exp',
                         colors = c('darkorange2','darkorchid4',"blue"),
                         #limits = c(MIN, MAX),
                         breaks = log(c(1,5,10,15,20)), labels = c(1,5,10,15,20)
                         ) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
}

plot_mapvel2("Tachycineta_bicolor", "all")
plot_mapvel2("Vireo_olivaceus", "all")
plot_mapvel2("Setophaga_americana", "all")
plot_mapvel2("Setophaga_discolor", "all")



## PANEL 1 ---------------------------------------



# sps <- "Vireo_olivaceus"   
# yearx <- "all"
# maptype = "hex"

# color cell
plot_mapvel_c <- function(sps, yearx, maptype){ # maptype = map hex hexn
  
  if(sps == "all") {
    if(yearx == "all") {preds2 <- velocityB}
    
    if(yearx != "all") {preds2 <- velocityB %>% 
      filter(year == yearx)}
  } else { 
    if(yearx == "all") {preds2 <- velocityB %>% 
      filter(species == sps)}
    
    if(yearx != "all") {preds2 <- velocityB %>% 
      filter(species == sps,
             year == yearx)}
  }
  
  cell_sps <- sort(unique(velocityB$cell))
  
  preds2 <- preds2 %>% 
    mutate(
      x = cell_lng + 2 * cos(angB * pi / 180),
      y = cell_lat + 2 * sin(angB * pi / 180)
    ) %>% 
    filter(vArrMag < 3000)
  
  cell_use <- sort(unique(preds2$cell))
  
  for(i in 1:length(cell_use)){
    cell_lo <- cell_use[i] 
    
    taba1 <- preds2 %>% 
      filter(cell == cell_lo) 
    meansam <- mean(taba1$vArrMag, na.rm = T)
    taba2 <- taba1 %>% 
      dplyr::select(species, cell, vArrMag) %>% 
      group_by(species) %>% 
      mutate(cell_sp_var = mean(vArrMag, na.rm= T),
             count = n(),
             dif = count * ((cell_sp_var - meansam)^2)) 
    sd1 <- #(sum(taba2$dif))/length(unique(taba2$species)) # 
      sd(taba2$cell_sp_var)
    
    tabb1 <- preds2 %>% 
      filter(cell == cell_lo) 
    tabb2 <- taba1 %>% 
      dplyr::select(year, cell, vArrMag) %>% 
      group_by(year) %>% 
      mutate(cell_yr_var = mean(vArrMag, na.rm= T),
             count = n(),
             dif = count * ((cell_yr_var - meansam)^2)) 
    sd2 <- #(sum(tabb2$dif))/length(unique(tabb2$year)) # 
      sd(tabb2$cell_yr_var)
    
    tab <- t(as.matrix(c(cell_lo, sd1, sd2)))
    if(i == 1) { tab2 <- tab} else {tab2 <- rbind(tab2,tab)}
  }
  
  colnames(tab2) <- c("cell", "cell_spe_var", "cell_yr_var")
  vars <- tab2 %>% 
    as_tibble() %>% 
    filter(!is.na(cell_spe_var) | !is.na(cell_yr_var))
  
  preds3 <- as.data.frame(matrix(data = NA, ncol = 8, nrow = length(cell_sps)))
  colnames(preds3) <- c("species","year","cell","cell_lng","cell_lat","x","y","mag")
  
  for(i in 1:length(cell_sps)){
    celll <- cell_sps[i]
    pred_loop <- preds2 %>% filter(cell == celll)
    if(nrow(pred_loop) == 1) {
      preds3$species[i] <- pred_loop$species
      preds3$year[i] <- pred_loop$year
      preds3$cell[i] <- pred_loop$cell
      preds3$cell_lng[i] <- pred_loop$cell_lng
      preds3$cell_lat[i] <- pred_loop$cell_lat
      preds3$x[i] <- pred_loop$x
      preds3$y[i] <- pred_loop$y
      preds3$mag[i] <- pred_loop$vArrMag
    } else {
      preds3$species[i] <- pred_loop$species[1]
      preds3$year[i] <- pred_loop$year[1]
      preds3$cell[i] <- pred_loop$cell[1]
      preds3$cell_lng[i] <- pred_loop$cell_lng[1]
      preds3$cell_lat[i] <- pred_loop$cell_lat[1]
      preds3$x[i] <- mean(x = pred_loop$x)
      preds3$y[i] <- mean(x = pred_loop$y)
      preds3$mag[i] <-  mean(x = pred_loop$vArrMag, na.rm = T)
    }
  }
  
  preds4 <- preds3 %>% 
    filter(mag < 3000,
           !is.na(year))
  
  cell_grid_tab4 <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/MigratorySensitivity//Data/master_cell_grid.rds")   %>% ## load grid - package not on CRAN
    left_join(., vars, by = "cell") #%>% 
  #filter(!is.na(cell_spe_var) | !is.na(cell_yr_var))
  
  mean_spe_cell_m <- lme4::lmer(data = preds2,
                                log(vArrMag) ~ as.factor(cell) - 1 + (1|species) + (1|year))
  
  mean_spe_cell <- cbind(substring(names(lme4::getME(mean_spe_cell_m, name = "fixef")), 16),
                         as.numeric(lme4::getME(mean_spe_cell_m, name = "fixef")))
  
  colnames(mean_spe_cell) <- c("cell", "mean_cell_spd")
  mean_spe_cell <- as.data.frame(mean_spe_cell)
  mean_spe_cell$cell <- as.numeric(mean_spe_cell$cell)
  mean_spe_cell$mean_cell_spd <- as.numeric(mean_spe_cell$mean_cell_spd)
  
  cell_grid_tab4 <- cell_grid_tab4 %>% 
    left_join(., mean_spe_cell, by = "cell")
  # merge hex spatial data with bird  data
  preds5 <- left_join(preds4, cell_grid_tab4, by = 'cell') 
  
  pp +
    geom_polygon(data = cell_grid_tab4, aes(x = long, y = lat,
                                            fill= cell_spe_var), alpha = 0.8) +
    scale_fill_viridis_c(#name = "Variation in speedn\(standart deviation)",
      limits = c(0, 80),
      na.value = "lightgray") +
    geom_path(data = cell_grid_tab4,
              aes(x = long,y = lat, group = cell),
              inherit.aes = FALSE,
              color = 'black', alpha = 0.2) +
    #geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
    #                                xend = x + (mag/75), yend = y + (mag/75),
    #                                group = cell),
    #             arrow = arrow(length = unit(0.1, "cm")), size = 0.7)  +
    ggtitle(glue("Between species speed variation")) +
    theme_bw() +
    theme(legend.position = "none",
      panel.grid.major = element_line(color = alpha('black', 0.2),
                                      size = 0.5),
      panel.ontop = TRUE,
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = NA),
      legend.title=element_text(size=9),
      legend.spacing.y = grid::unit(0.5, "cm"),
      legend.text=element_text(size=rel(0.8)),   
      legend.key.height=grid::unit(0.9,"cm") ,
      legend.title.align=0.5,
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.border = element_blank()) #+ sc
  
  pp +
    geom_polygon(data = cell_grid_tab4, aes(x = long, y = lat,
                                            fill= AZD ), alpha = 0.8) +
    scale_fill_viridis_c(name = "Variation in speed \n (standart deviation)",
                         limits = c(0, 80),
                         na.value = "lightgray") +
    geom_path(data = cell_grid_tab4,
              aes(x = long,y = lat, group = cell),
              inherit.aes = FALSE,
              color = 'black', alpha = 0.2) +
    #geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
    #                                xend = x + (mag/75), yend = y + (mag/75),
    #                                group = cell),
    #             arrow = arrow(length = unit(0.1, "cm")), size = 0.7)  +
    ggtitle(glue("Between year bird speed variation")) +
    theme_bw() +
    theme(
      #legend.position = "none",
      panel.grid.major = element_line(color = alpha('black', 0.2),
                                      size = 0.5),
      panel.ontop = TRUE,
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = NA),
      legend.title=element_text(size=9),
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
  
  (my_breaks1 <- c(3.5,3.75,4,4.25,4.5))
  
  (my_breaks2 <- round(exp(my_breaks1)))
  
  my_breaks2 <- c(30,40,50,60,70,80,90)
  
  my_breaks1 <- log(my_breaks2)
  
  pp +
    geom_polygon(data = cell_grid_tab4, aes(x = long, y = lat,
                                            fill= mean_cell_spd), alpha = 0.8) +
    scale_fill_viridis_c(name = "Speed \n (km/day, log scale)",
                         #limits = c(0, 80), 
                         option = "plasma", trans = "log",
                         breaks = my_breaks1, 
                         labels = my_breaks2,
                         na.value = "lightgray") +
    guides()+
    geom_path(data = cell_grid_tab4,
              aes(x = long,y = lat, group = cell),
              inherit.aes = FALSE,
              color = 'black', alpha = 0.2) +
    geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
                                    xend = x + (mag/75), yend = y + (mag/75),
                                    group = cell),
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.7)  +
    ggtitle(glue("Bird mean speed")) +
    theme_bw() +
    theme(panel.grid.major = element_line(color = alpha('black', 0.2),
                                          size = 0.5),
          panel.ontop = TRUE,
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = NA),
          legend.title=element_text(size=9),
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

# color arrow
plot_mapvel_a <- function(sps, yearx, maptype){ # maptype = map hex hexn
  
  if(sps == "all") {
    if(yearx == "all") {preds2 <- velocityB}
    
    if(yearx != "all") {preds2 <- velocityB %>% 
      filter(year == yearx)}
  } else { 
    if(yearx == "all") {preds2 <- velocityB %>% 
      filter(species == sps)}
    
    if(yearx != "all") {preds2 <- velocityB %>% 
      filter(species == sps,
             year == yearx)}
  }
  
  
  cell_sps <- sort(unique(velocityB$cell))
  
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
  
  if(maptype == "map") {base_map <- pp}
  
  if(maptype == "hex") {base_map <- rrb}
  
  if(maptype == "hexn") {base_map <- rr}
  
  myPalette <- colorRampPalette(brewer.pal(11, "PuRd"))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 9))
  
  my_breaks2 <- c(200, 600, 1200, 1800, 3000)
  
  my_breaks1 <- log(my_breaks2)
  
  base_map +
    geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell, colour = log(mag)),
                 arrow = arrow(length = unit(0.1, "cm")), size = 1.2) +
    scale_colour_gradient(trans = "log",breaks = my_breaks1, labels = my_breaks2,
                          low = "yellow", high = "red"
    ) +
    ggtitle(glue("{sps} {yearx}")) +
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
      panel.border = element_blank()) #+ sc
}

plot_mapvel_a("Tachycineta_bicolor", "all", "hex")
plot_mapvel_c("all", "all", "hex")







# PANEL 2 ------------------------------------------------------------------------------------
spse <- table(final$species) %>% sort(decreasing = T) 
spse <- names(spse[1:10])

my_breaks2 <- c(25, 50, 75, 100)
my_breaks1 <- log(my_breaks2)



dev.off()

svg(glue("figures/mod_lat_lag.svg"), 
    width = 4, height = 4)
#draw(mod_lag#, residuals = T
#     ) 
plot_smooth(mod_lag, view = "cell_lat") 

dev.off()

svg(glue("figures/mod_lat_bs.svg"), 
    width = 4, height = 4)
#draw(mod_gu#, residuals = T
#     )
plot_smooth(mod_gu, view = "cell_lat") 

dev.off()

# transformed axis:
a <- c(12.18249,15,
       20.08554,25,
       33.11545,
       50,54.59815)
b <- log(c(12.18249,15,
           20.08554,25,
           33.11545,
           50,54.59815))
tab <- as.data.frame(cbind(a,b))

svg(glue("figures/mod_lat_bs_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
    limits = c(2.3,4.2), 
    breaks = log(c(12.18249,15,
                   20.08554,25,
                   33.11545,50,
                   54.59815)), 
    labels = c(".",15,
               ".", 25,
               ".", 50, 
               ".")) 
dev.off()


svg(glue("figures/mod_lat_gs.svg"), 
    width = 4, height = 4)
#draw(mod_gre#, residuals = T
#     ) 
plot_smooth(mod_gre, view = "cell_lat") 

dev.off()

# transformed axis:
exp(c(3,3.5,4,4.5,5,5.5))
a <- c(25,20.08554,
       33.11545, 50,
       54.59815, 75,
       90.01713, 100,
       148.41316, 150,
       244.69193, 250)
b <- log(c(25,20.08554,
           33.11545, 50,
           54.59815, 75,
           90.01713, 100,
           148.41316, 150,
           244.69193, 250))
tab <- as.data.frame(cbind(a,b))
svg(glue("figures/mod_lat_gs_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
    limits = c(2.95,5.6), 
    breaks = log(c(25,20.08554,
                   33.11545, 50,
                   54.59815, 75,
                   90.01713, 100,
                   148.41316, 150,
                   244.69193, 250)), 
    labels = c(25,".",
               ".", 50,
               ".", 75,
               ".", 100,
               ".", 150,
               ".", 250)) 
dev.off()







# PANEL 3 -------------------------
ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) + # no estimates, no neighbors
  geom_boxplot(aes(x = reorder(species2,
                               #xi_mean, 
                               log(vArrMag),
                               FUN = median, na.rm = TRUE), 
                   y = log(vArrMag)#, fill = ea_lat
  ),
  width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Species first \narrival date\n") +
  labs(y = "Log(Speed)") +
  scale_y_continuous(trans='log10', breaks = log(c(10,50,250,1000)), labels = c(10,50,250,1000))

final %>% filter(species != "Pipilo_erythrophthalmus") %>% 
  group_by(species) %>% 
  summarise(median = median(vArrMag, na.rm = T)) %>% 
  view()

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = winlat), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Wintering\nlatitude") +
  labs(y = "Log(Speed)\n") 

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = ea_lat), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "First\narrival date", option = "B") +
  labs(y = "Log(Speed)\n") 

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = xi_mean), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Sensitivity", option = "G") +
  labs(y = "Log(Speed)\n") 


ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = Body_mass_g), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Body mass (g)", option = "B") +
  labs(y = "Log(Speed)\n") 

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = Diet), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(y = "Log(Speed)\n") 


ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = Time), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  #scale_fill_viridis(name = "Diet") +
  labs(y = "Log(Speed)\n") 

ggplot(final, aes(x = log(vArrMag))) +
  geom_histogram(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  theme_bw() +
  scale_x_continuous(trans='log10', 
                     breaks = log(c(10,50,250,1000)), 
                     labels = c(10,50,250,1000)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# PANEL 5 -----------------------------------------------
lag_tab <- final %>% dplyr::select(cell_lat, lag, cell_lat2) %>% 
  mutate(cell_cat = case_when(cell_lat < -10 ~ -12.5,
                              -10 <= cell_lat & cell_lat < -5 ~ -7.5,
                              -5 <= cell_lat & cell_lat < 0 ~ -2.5,
                              0 <= cell_lat & cell_lat < 5 ~ 2.5,
                              5 <= cell_lat ~ 7.5),
         cell_cat = as.factor(cell_cat))

sort(unique(as.numeric(lag_tab %>% 
                  filter(cell_cat == -12.5) %>% 
                  dplyr::select(cell_lat2) %>%
                  pull())))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == -7.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))


sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == -2.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == 2.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == 7.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

lag_tab %>% dplyr::select(cell_cat, lag) %>% filter(cell_cat == 7.5) %>% 
  mutate(Var = var(lag, na.rm = T))

lag_tab %>% dplyr::select(cell_cat, lag) %>% filter(cell_cat == -12.5) %>% 
  mutate(Var = var(lag, na.rm = T))

(lag1 <- ggplot(data = lag_tab %>% filter(!is.na(cell_cat))) +
  geom_jitter(aes(cell_cat, lag), alpha = 0.08, col = "orange") +
    geom_hline(yintercept = 0, 
               color = "black", size=0.7) +
  #geom_violin(aes(cell_cat, AnomLag), alpha = 0.6, width = 1) +
  geom_boxplot(aes(cell_cat, lag), alpha = 0.6, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Lag (days)", x = "Latitude (degrees)\n") +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
  coord_flip() #+ ylim(-30,30)
)

lagA_tab <- final %>% dplyr::select(cell_lat, AnomLag, cell_lat2) %>% 
  mutate(cell_cat = case_when(cell_lat < -10 ~ -12.5,
                              -10 <= cell_lat & cell_lat < -5 ~ -7.5,
                              -5 <= cell_lat & cell_lat < 0 ~ -2.5,
                              0 <= cell_lat & cell_lat < 5 ~ 2.5,
                              5 <= cell_lat ~ 7.5),
         cell_cat = as.factor(cell_cat))

lagA_tab %>% dplyr::select(cell_cat, AnomLag) %>% filter(cell_cat == 7.5) %>% 
  mutate(Var = var(AnomLag, na.rm = T))

lagA_tab %>% dplyr::select(cell_cat, AnomLag) %>% filter(cell_cat == -12.5) %>% 
  mutate(Var = var(AnomLag, na.rm = T))

lag2 <- ggplot(data = lagA_tab %>% filter(!is.na(cell_cat))) +
  geom_jitter(aes(cell_cat, AnomLag), alpha = 0.08, col = "orange") +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  #geom_violin(aes(cell_cat, AnomLag), alpha = 0.6, width = 1) +
  geom_boxplot(aes(cell_cat, AnomLag), alpha = 0.6, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Anomaly on lag (days)"#, x = "Latitude (degrees)"
  ) +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
  coord_flip() #+ ylim(-30,30)

svg(glue("figures/fig6.svg"), 
    width = 4, height = 5)
egg::ggarrange(lag1, lag2, ncol = 1)
dev.off()

vars3 <- lagA_tab %>% 
  group_by(cell_cat) %>% 
  summarise(sdano = sd(AnomLag, na.rm = T))

vars4 <- lag_tab %>% 
  group_by(cell_cat) %>% 
  summarise(sdlag = sd(lag, na.rm = T))

vars5 <- left_join(vars3, vars4, by = "cell_cat")

bird_name <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/Table_S1.csv") %>%
  dplyr::select(Species) %>% 
  mutate(species2 = Species) %>% 
  rename(species = Species) %>% 
  mutate(species = sub(" ", "_", species))

final %>% 
  dplyr::select(species2, Family, vArrMag, xi_mean, ea_lat, Diet, Body_mass_g, winlat , Time) %>% 
  group_by(species2) %>% mutate(mean_spe = mean(vArrMag, na.rm = T), 
                                med_spe = median(vArrMag, na.rm = T),
                                var_spe = var(vArrMag, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::select(-vArrMag) %>%
  distinct() %>% 
  write_csv(., file = "figures/sps_table.csv")



