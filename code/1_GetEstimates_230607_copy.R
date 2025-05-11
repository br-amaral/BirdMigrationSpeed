## Code to import phenological mismatch data (results from Youngflesh et al 2021) and create tibbles 
##   with bird and green-up velocity to run models in 3_ModelPlots.R
#
## Input:  data/source/arrival_master_2020-07-21.csv : bird arrival dates data
#          data/source/MidGreenup_2020-08-06-forest.csv : green-up arrival dates data
#
## Output: data/velocityB.rds : tibble with calculated bird speeds
#          data/velocityG.rds : tibble with calculated green-up speeds
#
#          data/cellaveB.rds : tibble with the coordinates for the velocity vectors for map for the birds
#          data/cellaveG.rds : tibble with the coordinates for the velocity vectors for map for the green-up
#
#          data/birdgreen.rds : bird and green-up speed files merged (so there is some green-up here missing probs)
#
#          data/cellcoor.rds : x and y coordinates for all cells
#          data/cellnumbs.rds : 
#          data/cellnei.rds : 

freshr::freshr()

# load packages --------------------------
library(glue)
library(tidyverse)
library(spdep)
library(SpatialEpi)
library(REdaS)
library(dplyr)

# import data ---------------------------
## file paths
BIRDDATA_PATH <- glue("data/source/arrival_master_2020-07-21.csv")
GREENDATA_PATH <- glue("data/source/MidGreenup_2020-08-06-forest.csv")

## read files
bird1 <- read_csv(BIRDDATA_PATH) %>% 
  filter(VALID_GAM == TRUE) 
bird1 <- bird1[,-1]  ## remove weird first column

greenup1 <- read_csv(GREENDATA_PATH) %>% 
  filter(gr_ncell > 10000)
greenup1 <- greenup1[,-1]

# define maximum speed threshold
spe_thres <- 3000

# cell number in a sequence that starts at 1
tc <- sort(unique(greenup1$cell))
cellnumbs <- data.frame(cell = tc, 
                        cell2 = seq(1,length(tc)))

bird <- dplyr::left_join(bird1, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2) 

bird <- as.data.frame(bird)

greenup <- dplyr::left_join(greenup1, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2) 
greenup <- as.data.frame(greenup)

# species list
spec <- unique(bird$species) 

# get the neighbor set for each cell within a set buffer --------------------------
minNeigh <- 5   # threshold to calculate only velocities that have at least 5 number of neighbors
cells <- unique(greenup[ ,c("cell","cell_lat","cell_lng")])    # unique data points - cells and their locations 

adj <- dnearneigh(as.matrix(cells[ ,3:2]), 0, 360, longlat= TRUE)   # list of adjacent cells for each cell
num <- sapply(adj, length)         # number of neighbors for each cell
adj <- adj[num >= 0] 

cells <- as.tibble(cells)
cells <- cells %>% 
  mutate(adj = adj,
         numn = num)

# calculate velocity for birds --------------------------
velocityB <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

for (yr in min(bird$year):max(bird$year)){
  for (spp in 1:length(spec)){
    dat.temp <- subset(bird, year == yr & species == spec[spp])
    for (b in 1:nrow(cells)){ # look at the neighbors of a cell   c(adj[[b]], cells[b,1])
      # print(cells[b,])
      vecnei <- c(pull(cells[unlist(cells$adj[b]),1]), pull(cells[b,1]))
      dat.lm <- dat.temp[dat.temp$cell %in% vecnei, c("cell_lat","cell_lng","arr_GAM_mean","cell")]  
      rm(vecnei)
      if(nrow(dat.lm) >= minNeigh) {  # validate that minimum data points available
        if(sum(!is.na(dat.lm$arr_GAM_mean)) >= minNeigh + 1){
          #print(c(yr, spp, b))
          xy <- latlong2grid(dat.lm[,2:1])  # cell positions
          colnames(xy) <- c('x','y')
          coef <- lm(data = dat.lm, formula = arr_GAM_mean ~ xy$x + xy$y)$coef[2:3]
          angle <- rad2deg(atan2(coef[1],coef[2]))
          if(angle < 0) {angle = angle + 360}
          velocityB <- rbind(velocityB,
                             c(spec[spp],
                               yr,
                               as.numeric(cells[b,1]),
                               sum(!is.na(dat.lm$arr_GAM_mean)),
                               #as.numeric(sqrt((1/coef[1])^2+(1/coef[2])^2)),  # magnitude
                               as.numeric(1/sqrt(sum(coef^2))),
                               as.numeric(angle))                              # angle
            )
          }
        }
    }
  }
}

# fill bird speed tibble with values
velocityB <- data.frame(species = velocityB[,1], 
                        year = as.numeric(velocityB[,2]),
                        cell = velocityB[,3],
                        NArr = as.numeric(velocityB[,4]),
                        vArrMag = as.numeric(velocityB[,5]),  # magnitude
                        vArrAng = as.numeric(velocityB[,6]))  # angle

velocityB$cell <- as.numeric(velocityB$cell)

# add coordinates for the angles of each cell for migration
preds <- left_join(velocityB, cells[,1:3], by= "cell")
preds$ang <- NA
for(i in 1:nrow(preds)){
  if(!is.na(preds$vArrAng[i]) & (360 - preds$vArrAng[i] + 90)>360) 
  {preds$ang[i] = (360 - preds$vArrAng[i] + 90 - 360)}
  if(!is.na(preds$vArrAng[i]) & (360 - preds$vArrAng[i] + 90)<360) 
  {preds$ang[i] = (360 - preds$vArrAng[i] + 90)}
}

preds2 <- preds %>% 
  mutate(
    x = cell_lng + log((vArrMag)) * cos(ang * pi / 180),
    y = cell_lat + log((vArrMag)) * sin(ang * pi / 180)
  )

cell_sps <- preds2 %>% 
  dplyr::select(species, cell) %>% 
  unique()

preds3 <- as.data.frame(matrix(data = NA, ncol = 6, nrow = nrow(cell_sps)))
colnames(preds3) <- c("species","cell","cell_lng","cell_lat","x","y")
preds3$species <- cell_sps$species
preds3$cell <- cell_sps$cell

for(i in 1:nrow(preds3)){
  pred_loop <- preds2 %>% 
    filter(species == preds3[i,1],
           cell == preds3[i,2])
  
  if(nrow(pred_loop) == 1) {
    preds3$species[i] <- pred_loop$species
    preds3$cell[i] <- pred_loop$cell
    preds3$cell_lng[i] <- pred_loop$cell_lng
    preds3$cell_lat[i ] <- pred_loop$cell_lat
    preds3$x[i] <- pred_loop$x
    preds3$y[i] <- pred_loop$y
  } else {
    preds3$species[i] <- pred_loop$species[1]
    preds3$cell[i] <- pred_loop$cell[1]
    preds3$cell_lng[i] <- pred_loop$cell_lng[1]
    preds3$cell_lat[i] <- pred_loop$cell_lat[1]
    preds3$x[i] <- mean(x = pred_loop$x)
    preds3$y[i] <- mean(x = pred_loop$y)
  }
} 

predsB <- preds3
velocityB <- preds2
colnames(velocityB) <- c("species", "year", "cell", "NArr", "vArrMag", "vArrAng", "cell_lat", "cell_lng","angB", "xB", "yB")      

# calculate velocity for green up -------------
velocityG <- data.frame(year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

for(a in min(bird$year):max(bird$year)){
  dat.temp <- subset(greenup, year == a)
  for(b in 1:nrow(cells)){
    vecnei <- c(pull(cells[unlist(cells$adj[b]),1]), pull(cells[b,1]))
    dat.lm <- dat.temp[dat.temp$cell %in% vecnei, c("cell_lat","cell_lng","gr_mn","cell")]  
    rm(vecnei)
    if(nrow(dat.lm) >= minNeigh) {       # validate that minimum data points available
      if(sum(!is.na(dat.lm$gr_mn)) >= minNeigh + 1){
        #print(c(a,b))
        xy <- latlong2grid(dat.lm[ ,2:1])
        colnames(xy) <- c('x','y')
        coef <- lm(data = dat.lm, formula = gr_mn ~ xy$x + xy$y)$coef[2:3]
        angle <- rad2deg(atan2(coef[1],coef[2]))
        if(angle < 0) {angle = angle + 360}
        velocityG <- rbind(velocityG,
                           c(a,
                             as.numeric(cells[b,1]),
                             as.numeric(sum(!is.na(dat.lm$gr_mn))),
                             #as.numeric(sqrt((1/coef[1])^2+(1/coef[2])^2)),
                             as.numeric(1/sqrt(sum(coef^2))),
                             as.numeric(angle)))
      }
    }
  }
}

velocityG <- data.frame(year = as.numeric(velocityG[,1]),
                        cell = velocityG[,2],
                        NGr = as.numeric(velocityG[,3]),
                        vGrMag = as.numeric(velocityG[,4]),
                        vGrAng = as.numeric(velocityG[,5]))

velG <- left_join(velocityG, cells[,1:3], by= "cell")
velG$ang <- NA

for(i in 1:nrow(velG)){
  if(!is.na(velG$vGrAng[i]) & (360 - velG$vGrAng[i] + 90)>360) 
  {velG$ang[i] = (360 - velG$vGrAng[i] + 90 - 360)} 
  if(!is.na(velG$vGrAng[i]) & (360 - velG$vGrAng[i] + 90)<360) 
  {velG$ang[i] = (360 - velG$vGrAng[i] + 90)}
}

velG <- velG %>% 
  mutate(
    x = cell_lng + log((vGrMag)) * cos(ang * pi / 180),
    y = cell_lat + log((vGrMag)) * sin(ang * pi / 180)
  )

velocityG <- velG
colnames(velocityG) <- c("year", "cell", "NGr", "vGrMag", "vGrAng", "cell_lat", "cell_lng", "angG", "xG", "yG")

# average of all years
cell_g <- sort(unique(velocityG$cell))
predsG <- as.data.frame(matrix(data = NA, ncol = 5, nrow = length(cell_g)))
colnames(predsG) <- c("cell","cell_lng","cell_lat","x","y")
predsG$cell <- cell_g

for(i in 1:nrow(predsG)){
  pred_loop <- velG %>% 
    filter(cell == predsG[i,1])
  
  if(nrow(pred_loop) == 1) {
    predsG$cell[i] <- pred_loop$cell
    predsG$cell_lng[i] <- pred_loop$cell_lng
    predsG$cell_lat[i] <- pred_loop$cell_lat
    predsG$x[i] <- pred_loop$x
    predsG$y[i] <- pred_loop$y
  } else {
    predsG$cell[i] <- pred_loop$cell[1]
    predsG$cell_lng[i] <- pred_loop$cell_lng[1]
    predsG$cell_lat[i] <- pred_loop$cell_lat[1]
    predsG$x[i] <- mean(x = pred_loop$x)
    predsG$y[i] <- mean(x = pred_loop$y)
  }
} 

# join green up and bird velocity data -----------------
all <- base::merge(bird, greenup[ ,c("year","cell","gr_mn","gr_ncell")], all.x = TRUE)
velocityB <- velocityB %>% 
  mutate(cell = as.integer(cell)) %>% 
  dplyr::select(!cell_lat) %>% 
  dplyr::select(!cell_lng)
all <- full_join(all, velocityB, by = c("year", "cell", "species"))
velocityG <- velocityG %>% 
  mutate(cell = as.integer(cell)) %>% 
  dplyr::select(!cell_lat) %>% 
  dplyr::select(!cell_lng)
all <- full_join(all, velocityG, by = c("year", "cell"))
all$lag <- all$gr_mn - all$arr_GAM_mean

all <- all %>% 
  mutate(NArr = as.numeric(NArr),
         vArrMag = as.numeric(vArrMag),
         vArrAng = as.numeric(vArrAng),
         angB = as.numeric(angB),
         angG = as.numeric(angG)) %>% 
  dplyr::select(-c(VALID_GAM, arr_IAR_mean, arr_IAR_sd))

for(i in 1:nrow(all)){
  if(!is.na(all$vArrMag[i])) {
    if(all$vArrMag[i] > spe_thres) {all$vArrMag[i] <- NA}
  }
}

cellspec <- unique(all[ ,c("cell","species")])

# subset of all data ----------------------------------------
# get cells species with at least 8 years of data

final <- matrix(NA, 0, 27)

for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(all, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c("arr_GAM_mean","gr_mn","vArrMag","vGrMag","lag")], 2, function(x) scale(log(x), scale = FALSE))
    # divide by mean but not dividing by sd
    colnames(Anom) <- c("AnomDArr", "AnomDGr", "AnomVArr", "AnomVGr", "AnomLag")
    dat.temp <- cbind(dat.temp, Anom)
    final <- rbind(final, dat.temp)
  }
}

final2 <- final %>% 
  as_tibble() %>% 
  mutate(
    cell_lat2 = cell_lat,
    cell_lat = as.numeric(scale(cell_lat, scale = FALSE))  # centering lat for regression analysis
  )
# save output tibbles to run models
## velocities of bird and green up
write_rds(velocityB, file = glue("data/velocityB_st{spe_thres}.rds"))
write_rds(velocityG, file = glue("data/velocityG_st{spe_thres}.rds"))
## files with the coordinates for the velocity vectors for the map (all years together)
write_rds(predsB, file = glue("data/cellaveB_st{spe_thres}.rds"))
write_rds(predsG, file = glue("data/cellaveG_st{spe_thres}.rds"))
## bird and green up velocity merged
write_rds(final2, file = glue("data/birdgreen_st{spe_thres}.rds"))
write_rds(cells %>% dplyr::select(cell, cell_lat, cell_lng), 
          file = glue("data/cellcoor_st{spe_thres}.rds"))
write_rds(cellnumbs, file = glue("data/cellnumbs_st{spe_thres}.rds"))
write_rds(cells, file = glue("data/cellnei_st{spe_thres}.rds"))

