## Script to import phenological mismatch data (results from Youngflesh et al 2021) and create tibbles 
##   with bird and green-up velocity to run models in 2_RunMods
## Input:  arrival_master_2020-07-21.csv
#          MidGreenup_2020-08-06-forest.csv
## Output: velocityB.rds 
#          cellaveB.rds
#          velocityG.rds
#          cellaveG.rds
#          birdgreen.rds
#          cellcoor.rds

library(glue)
library(tidyverse)
library(spdep)
library(SpatialEpi)
library(REdaS)
library(dplyr)

# import data ---------------------------
BIRDDATA_PATH <- glue("data/source/arrival_master_2020-07-21.csv") # may not be the most current data file - but what i first found
GREENDATA_PATH <- glue("data/source/MidGreenup_2020-08-06-forest.csv")

bird1 <- read_csv(BIRDDATA_PATH) %>% 
  filter(VALID_GAM == TRUE) ##%>% filter(species == "Setophaga_discolor")
bird1 <- bird1[,-1]

greenup1 <- read_csv(GREENDATA_PATH) %>% 
  filter(gr_ncell > 10000)
greenup1 <- greenup1[,-1]

# cell number in a sequence that starts at 1
tc <- sort(unique(greenup1$cell))
cellnumbs <- data.frame(cell = tc, 
                        cell2 = seq(1,length(tc)))

bird <- dplyr::left_join(bird1, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2) #%>% # ONLY late years
#  filter(year > 2008)

bird <- as.data.frame(bird)

greenup <- dplyr::left_join(greenup1, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2) #%>% # ONLY late years
#  filter(year > 2008)

greenup <- as.data.frame(greenup)

# species list
spec <- unique(bird$species) 

# get the neighbor set for each cell within a set buffer --------------------------
minNeigh <- 5   # threshold to calculate only velocities that had at least 5 number of neighbors
cells <- unique(greenup[ ,c("cell","cell_lat","cell_lng")])    # unique data points - cells and their locations 

adj <- dnearneigh(as.matrix(cells[ ,3:2]), 0, 360, longlat= TRUE)   # list of adjacent cells for each cell
num <- sapply(adj, length)         # number of neighbors for each cell
adj <- adj[num >= 0] 

#cells <- cells[num >= minNeigh,]   # keep only cells with enough neighbors
#   a later step will reassess whether minimum neighbors for individual species and years 
#adj <- adj[num >= minNeigh]        # eliminate cells with too few neighbors
#num <- sapply(adj,length)          # recalculate num (number of neighbors) for the reduced data set

cells <- as_tibble(cells)
cells <- cells %>% 
  mutate(adj = adj,
         numn = num)

plot(bird$cell_lat, bird$arr_GAM_mean)
bird %>% filter(arr_GAM_mean < 50)
# create weights for the regression analysis - STILL WORKING ON THAT!!!
#bird <- bird %>% 
#  mutate(weight = exp(-(2008 - year)/9))

# calculate velocity for birds --------------------------
velocityB <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

# velocities are specific to year, species and cell
#    yr <- 2015   ;  spp <- 55  ;  b <- 48
#    yr <- 2017   ;  spp <- 14  ;  b <- 58
for (yr in 2002:2017){
  for (spp in 1:length(spec)){
    dat.temp <- subset(bird, year == yr & species == spec[spp])
    for (b in 1:nrow(cells)){ # look at the neighbours of a cell   c(adj[[b]], cells[b,1])
      # print(cells[b,])
      vecnei <- c(pull(cells[unlist(cells$adj[b]),1]), pull(cells[b,1]))
      dat.lm <- dat.temp[dat.temp$cell %in% vecnei, c("cell_lat","cell_lng","arr_GAM_mean","cell")]  
      #plot(dat.lm$cell_lng, dat.lm$cell_lat, cex = 4)
      #text(cell_lat ~ cell_lng, labels=cell, data=dat.lm, cex=0.9, font=2, col = "blue")
      #dat.lm$arr_GAM_mean <- c(6,7,3,4,5,1,2)
      #text(cell_lat ~ cell_lng, labels=arr_GAM_mean, data=dat.lm, cex=0.9, font=2, col = "red")
      rm(vecnei)
      #dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]],b),1], c("cell_lat","cell_lng","arr_GAM_mean","cell")]   # get lat, long and Gam arrival mean
      if(nrow(dat.lm) >= minNeigh) {  # validate that minimum data points available
        if(sum(!is.na(dat.lm$arr_GAM_mean)) >= minNeigh + 1){
          #print(c(yr, spp, b))
          #dat.lm2$b <- cells[b,1]
          #dat.lm1 <- rbind(dat.lm1,dat.lm2)
          xy <- latlong2grid(dat.lm[,2:1])  # cel positions
          colnames(xy) <- c('x','y')
          coef <- lm(data = dat.lm, formula = arr_GAM_mean ~ xy$x + xy$y)$coef[2:3]
          angle <- rad2deg(atan2(coef[1],coef[2]))
          if(angle < 0) {angle = angle + 360}
          velocityB <- rbind(velocityB,
                             c(spec[spp],
                               yr,
                               as.numeric(cells[b,1]),
                               sum(!is.na(dat.lm$arr_GAM_mean)),
                               #as.numeric(sqrt((1/coef[1])^2+(1/coef[2])^2)), #c
                               as.numeric(1/sqrt(sum(coef^2))), #B
                               as.numeric(angle))
            )
          }
        }
    }
  }
}

velocityB <- data.frame(species = velocityB[,1], 
                        year = as.numeric(velocityB[,2]),
                        cell = velocityB[,3],
                        NArr = as.numeric(velocityB[,4]),
                        vArrMag = as.numeric(velocityB[,5]),  # magnitude
                        vArrAng = as.numeric(velocityB[,6]))  # angle

velocityB$cell <- as.numeric(velocityB$cell)


preds <- left_join(velocityB, cells[,1:3], by= "cell")
preds$ang <- NA
for(i in 1:nrow(preds)){
  if(!is.na(preds$vArrAng[i]) & (preds$vArrAng[i] + 90)>360) 
  {preds$ang[i] = (preds$vArrAng[i] + 90 - 360)}
  if(!is.na(preds$vArrAng[i]) & (preds$vArrAng[i] + 90)<360) 
  {preds$ang[i] = (preds$vArrAng[i] + 90)}
}

preds2 <- preds %>% 
  #  filter(vArrMag < 10000) %>% 
  mutate(
    # x = (cell_lng + 10) * cos(ang * pi / 180),
    # y = (cell_lat + 10) * sin(ang * pi / 180)
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

for(a in 2002:2017){
  dat.temp <- subset(greenup, year == a)
  for(b in 1:nrow(cells)){
    vecnei <- c(pull(cells[unlist(cells$adj[b]),1]), pull(cells[b,1]))
    dat.lm <- dat.temp[dat.temp$cell %in% vecnei, c("cell_lat","cell_lng","gr_mn","cell")]  
    rm(vecnei)
    #dat.lm <- dat.temp[dat.temp$cell %in% pull(cells[c(adj[[b]],b),1]), c("cell_lat","cell_lng","gr_mn","cell")]
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
                             as.numeric(1/sqrt(sum(coef^2))),
                             #as.numeric(sqrt((1/coef[1])^2+(1/coef[2])^2)),
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
    # x = (cell_lng + 10) * cos(ang * pi / 180),
    # y = (cell_lat + 10) * sin(ang * pi / 180)
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
# merge green up and bird velocity data
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
    if(all$vArrMag[i] > 10000) {all$vArrMag[i] <- NA}
  }
}

for(i in 1:nrow(all)){
  if(!is.na(all$vGrMag[i])) {
    if(all$vGrMag[i] > 10000) {all$vGrMag[i] <- NA}
  }
}
cellspec <- unique(all[ ,c("cell","species")])

final <- matrix(NA, 0, 27)

# subset of all data - get cells species with at least 8 years of data
for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(all, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c("arr_GAM_mean","gr_mn","lag")], 2, 
                  function(x) scale(x, scale = FALSE))
    # divide by mean but not dividing by sd
    colnames(Anom) <- c("AnomDArr", "AnomDGr", "AnomLag")
    dat.temp <- cbind(dat.temp, Anom)
    final <- rbind(final, dat.temp)
  }
}

final2 <- matrix(NA, 0, 2)

# subset of all data - get cells species with at least 8 years of data
for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(all, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c("vArrMag","vGrMag")], 2, 
                  function(x) scale(log(x), scale = FALSE))
    # divide by mean but not dividing by sd
    colnames(Anom) <- c("AnomVArr", "AnomVGr")
    dat.temp <- cbind(dat.temp, Anom)
    final2 <- rbind(final2, dat.temp)
  }
}

final3 <- left_join(final,
                   final2 %>% dplyr::select(species, cell, year, AnomVArr, AnomVGr),
                   by = c('species', 'cell', 'year'))

final4 <- final3 %>% 
  mutate(cell_lat = scale(cell_lat, scale = FALSE))  #scale lat for regression analysis

# save output tibbles to run models
## velocities of rbird and green up
saveRDS(velocityB, file = "data/velocityB.rds")
saveRDS(velocityG, file = "data/velocityG.rds")
## files with the coordinates for the velocity vectors for the map (all years together)
saveRDS(predsB, file = "data/cellaveB.rds")
saveRDS(predsG, file = "data/cellaveG.rds")
## bird and green up velocity merged
saveRDS(final4, file = "data/birdgreen.rds")
saveRDS(cells %>% dplyr::select(cell, cell_lat, cell_lng), file = "data/cellcoor.rds")
saveRDS(cellnumbs, file = "data/cellnumbs.rds")
saveRDS(cells, file = "data/cellnei.rds")


