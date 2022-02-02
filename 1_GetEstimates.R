## Script to import phenological mismatch data (results from Youngflesh et al 2021) and create tibbles 
##   with bird and green-up velocity to run models in 2_RunMods
## Input:  arrival_master_2020-07-21.csv
#          MidGreenup_2020-08-06-forest.csv
## Output: velocityB.rds
#          predsB.rds
#          velocityG.rds
#          predsG.rds

library(glue)
library(tidyverse)
library(spdep)
library(SpatialEpi)
library(REdaS)

# import data ---------------------------
BIRDDATA_PATH <- glue("data/arrival_master_2020-07-21.csv") # may not be the most current data file - but what i first found
GREENDATA_PATH <- glue("data/MidGreenup_2020-08-06-forest.csv")

bird1 <- read_csv(BIRDDATA_PATH) %>% 
  filter(VALID_GAM == TRUE) ##%>% filter(species == "Setophaga_discolor")
bird1 <- bird1[,-1]

greenup1 <- read_csv(GREENDATA_PATH) %>% 
  filter(gr_ncell > 10000)
greenup1 <- greenup1[,-1]

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

spec <- unique(bird$species)  # species list

# get the neighbor set for each cell within a set buffer --------------------------
minNeigh <- 5   # threshold to calculate only velocities that had at least 5 number of neighbors
cells <- unique(greenup[ ,c("cell","cell_lat","cell_lng")])    # unique data points - cells and their locations 

adj <- dnearneigh(as.matrix(cells[ ,3:2]), 0, 360, longlat= TRUE)   # list of adjacent cells for each cell
num <- sapply(adj, length)         # number of neighbors for each cell
cells <- cells[num >= minNeigh,]   # keep only cells with enough neighbors
#   a later step will reassess whether minimum neighbors for individual species and years 
adj <- adj[num >= minNeigh]        # eliminate cells with too few neighbors
num <- sapply(adj,length)          # recalculate num (number of neighbors) for the reduced data set

# calculate velocity for birds --------------------------
velocityB <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

# velocities are specific to year, species and cell
#    yr <- 2004   ;  spp <- 40  ;  b <- 38
#dat.lm1 <- as.data.frame(matrix(NA, ncol = 5))
#colnames(dat.lm1) <- c("cell_lat","cell_lng","arr_GAM_mean","cell","b")
for (yr in 2002:2017){
  for (spp in 1:length(spec)){
    dat.temp <- subset(bird, year == yr & species == spec[spp])
    for (b in 1:nrow(cells)){ # look at the neighbours of a cell
      dat.lm <- dat.lm2 <- dat.temp[dat.temp$cell %in% c(adj[[b]], cells[b,1]), c("cell_lat","cell_lng","arr_GAM_mean","cell")]   # get lat, long and Gam arrival mean
      if(nrow(dat.lm) >= minNeigh) {  # validate that minimum data points available
        if(sum(!is.na(dat.lm$arr_GAM_mean)) >= minNeigh){
          #print(c(yr, spp, b))
          #dat.lm2$b <- cells[b,1]
          #dat.lm1 <- rbind(dat.lm1,dat.lm2)
          xy <- latlong2grid(dat.lm[,2:1])  # cel positions
          coef <- lm(data = dat.lm, formula = arr_GAM_mean ~ xy$x + xy$y)$coef[2:3]
          angle <- rad2deg(atan2(coef[1],coef[2]))
          if(angle < 0) {angle = angle + 360}
          velocityB <- rbind(velocityB,
                            c(spec[spp],
                              yr,
                              cells[b,1],
                              sum(!is.na(dat.lm$arr_GAM_mean)),
                              sqrt((1/coef[1])^2+(1/coef[2])^2),
                              #1/sqrt(sum(coef^2)),
                              angle)
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

preds <- left_join(velocityB, cells, by= "cell")
preds$ang <- NA
for(i in 1:nrow(preds)){
  if((360 - preds$vArrAng[i] + 90)>360) {preds$ang[i] = (360 - preds$vArrAng[i] + 90 - 360)}
  if((360 - preds$vArrAng[i] + 90)<360) {preds$ang[i] = (360 - preds$vArrAng[i] + 90)}
}

preds2 <- preds %>% 
  filter(vArrMag < 10000) %>% 
  mutate(
    # x = (cell_lng + 10) * cos(ang * pi / 180),
    # y = (cell_lat + 10) * sin(ang * pi / 180)
    x = cell_lng + log((vArrMag)) * cos(ang * pi / 180),
    y = cell_lat + log((vArrMag)) * sin(ang * pi / 180)
  )

cell_sps <- preds2 %>% 
  select(species, cell) %>% 
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

# calculate velocity for green up -------------
velocityG <- data.frame(year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

for(a in 2002:2017){
  dat.temp <- subset(greenup, year == a)
  for(b in 1:nrow(cells)){
    dat.lm <- dat.temp[dat.temp$cell %in% c(adj[[b]], cells[b,1]), c("cell_lat","cell_lng","gr_mn","cell")]
    if(nrow(dat.lm) >= minNeigh) {       # validate that minimum data points available
      if(sum(!is.na(dat.lm$gr_mn)) >= minNeigh){
        #print(c(a,b))
        xy <- latlong2grid(dat.lm[ ,2:1])
        coef <- lm(data = dat.lm, formula = gr_mn ~ xy$x + xy$y)$coef[2:3]
        angle <- rad2deg(atan2(coef[1],coef[2]))
        if(angle < 0) {angle = angle + 360}
        velocityG <- rbind(velocityG,
                           c(a,
                             cells[b,1],
                             sum(!is.na(dat.lm$gr_mn)),
                             #1/sqrt(sum(coef^2)),
                             sqrt((1/coef[1])^2+(1/coef[2])^2),
                             angle))
      }
    }
  }
}

velocityG <- data.frame(year = as.numeric(velocityG[,1]),
                        cell = velocityG[,2],
                        NGr = as.numeric(velocityG[,3]),
                        vGrMag = as.numeric(velocityG[,4]),
                        vGrAng = as.numeric(velocityG[,5]))

velG <- left_join(velocityG, cells, by= "cell")
velG$ang <- NA

for(i in 1:nrow(preds)){
  if((360 - velG$vGrAng[i] + 90)>360) {velG$ang[i] = (360 - velG$vGrAng[i] + 90 - 360)}
  if((360 - velG$vGrAng[i] + 90)<360) {velG$ang[i] = (360 - velG$vGrAng[i] + 90)}
}

velG <- velG %>% 
  filter(vGrMag < 10000) %>% 
  mutate(
    # x = (cell_lng + 10) * cos(ang * pi / 180),
    # y = (cell_lat + 10) * sin(ang * pi / 180)
    x = cell_lng + log((vGrMag)) * cos(ang * pi / 180),
    y = cell_lat + log((vGrMag)) * sin(ang * pi / 180)
  )

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

# correlation between green up and bird velocity  -----------------
# merge green up and bird velocity data
all <- merge(bird, greenup[ ,c("year","cell","gr_mn","gr_ncell")], all.x = TRUE)
all <- merge(all, velocityB, all.x = TRUE)
all <- merge(all, velocityG, all.x = TRUE)
all$lag <- all$gr_mn - all$arr_GAM_mean

all$angB <- all$angG <- NA

for(i in 1:nrow(all)){
  ifelse(((360 - all$vArrAng[i] + 90)>360),all$angB[i] <- (360 - all$vArrAng[i] + 90 - 360),all$angB[i] <- (360 - all$vArrAng[i] + 90))
  ifelse(((360 - all$vGrAng[i] + 90)>360), all$angG[i] <- (360 - all$vGrAng[i] + 90 - 360), all$angG[i] <- (360 - all$vGrAng[i] + 90))
}

cellspec <- unique(all[ ,c("cell","species")])

final <- matrix(NA, 0, 27)

for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(all, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c("arr_GAM_mean","gr_mn","vArrMag","vGrMag","lag")], 2, function(x) scale(log(x), scale = FALSE))
    colnames(Anom) <- c("AnomDArr", "AnomDGr", "AnomVArr", "AnomVGr", "AnomLag")
    dat.temp <- cbind(dat.temp, Anom)
    final <- rbind(final, dat.temp)
  }
}

# merge lat and lon data for cells back in
vel <- merge(velocity, cells, by.x = c("cell"), by.y = c("cell"))
vel$cell_lat2 <- scale(vel$cell_lat, scale = FALSE)  #scale lat for regression analysis

final$cell_lat2 <- scale(final$cell_lat, scale = FALSE)  #scale lat for regression analysis

final2 <- final %>% 
  drop_na(vArrMag) %>% 
  drop_na(angB) %>% 
  drop_na(vGrMag) %>% 
  drop_na(angG) %>% 
  filter(vGrMag < 10000,
         vArrMag < 10000) %>% 
  mutate(vArrMag_s = scale(vArrMag),
         vGrMag_s = scale(vGrMag))



# save output tibbles to run models
saveRDS(velocityB, file = "velocityB.rds")
saveRDS(velocityG, file = "velocityG.rds")
saveRDS(predsB, file = "predsB.rds")
saveRDS(predsG, file = "predsG.rds")


