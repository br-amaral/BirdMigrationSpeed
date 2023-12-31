

library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(maps)
library(mapproj)
library(robustHD)
library(tidyverse)
library(geosphere)


##  Bird arrival date map - TAB 2 ----------------------------
## load data
arr_master <- readRDS("data/data_arr.RDS")

tc <- sort(unique(arr_master$cell))
cellnumbs <- data.frame(cell = tc, 
                        cell2 = seq(1,length(tc)))

arr_master <- dplyr::left_join(arr_master, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2)

arr_master3 <- arr_master2 <- arr_master
arr_master2 <- arr_master2 %>% 
  rename(year2 = year)

# load maps and plot formatting
worldmap <- ggplot2::map_data("world")
pp <- ggplot(data = worldmap, aes(x = long, y = lat, 
                                  group = group)) +
  geom_polygon(fill = alpha('black', 0.1), color = NA) +
  coord_map("ortho", orientation = c(35, -80, 0),
            xlim = c(-110, -50), ylim = c(21, 66)) +
  #theme_bw() +
  theme(panel.grid.major = element_line(color = alpha('black', 0.2),
                                        size = 0.5),
        panel.ontop = TRUE,
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
        axis.ticks.y=element_blank()) +
  xlab('Longitude') +
  ylab('Latitude') +
  geom_path(aes(x = long, y = lat, group = group),
            alpha = 0.4, color = 'black')

rm(worldmap)

TAB <- readRDS("data/data_sensi.RDS")
TAB <- left_join(TAB, cellnumbs, by="cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2)

load("data/species_Grid.RData")

# Line plot
# all species
sensim_df <- readRDS('data/fit_df_tab5.rds')
qq <- ggplot(sensim_df, aes(lat, sensim, group = species)) +
  geom_line(size = 1, col = "gray") +
  theme_classic() +
  xlab("Latitude (Degrees)") +
  ylab("Sensitivity (Days / Day)") +
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90, margin = margin(r = 10)),
        axis.title.x = element_text(size = rel(0.9), angle = 00),
        axis.text=element_text(size=8, colour = "black")
        #axis.text.y = element_text(angle=90)
  ) 
# add species of interest on top 
doline <- function(species){
  sensim_df_f <- sensim_df[which(sensim_df$species == species),]
  qq + geom_line(data = sensim_df_f, 
                 #alpha = 0.8,
                 size = 1.1,
                 color = 'black')
}

##  Interannual variation - TAB 4 ----------------------------
f1a_green <- 'indianred'
f1a_bird <- '#2686A0'

## Range map
ran_sp <- arr_master3 

#create hex grid
cell_grid_tab4 <- readRDS("data/master_cell_grid.rds") ## load grid - package not on CRAN
for_gr <- readRDS('data/for_green-up_dl.rds')

for_gr2 <- left_join(for_gr, cellnumbs, by = "cell") %>% 
  select(-cell) %>% 
  rename(cell = cell2)

#merge hex spatial data with HM data
ran_sp <- left_join(ran_sp, cell_grid_tab4, by = 'cell', relationship = "many-to-many") %>%  
  transmute(species,
            cell,
            cell_lat,cell_lng,
            lat,long)

ran_sp3 <- distinct(ran_sp)
ran_sp3$species <- NA
ran_sp3 <- distinct(ran_sp3)

rr <- pp +
  geom_polygon(data = cell_grid_tab4, aes(x = long, y = lat),
               fill="white",
               inherit.aes = FALSE, alpha = 1) +
  geom_path(data = cell_grid_tab4,
            aes(x = long,y = lat, group = cell),
            inherit.aes = FALSE,
            color = 'black', alpha = 0.2) +
  annotate('text', x = ran_sp3$cell_lng, y = ran_sp3$cell_lat,
           label = ran_sp3$cell, col = 'black', alpha = 0.9,
           size = 3) #+

rrb <- pp +
  geom_polygon(data = cell_grid_tab4, aes(x = long, y = lat),
               fill="white",
               inherit.aes = FALSE, alpha = 1) +
  geom_path(data = cell_grid_tab4,
            aes(x = long,y = lat, group = cell),
            inherit.aes = FALSE,
            color = 'black', alpha = 0.2) 

