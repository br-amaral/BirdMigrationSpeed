library(sp)
library(spdep)
library(raster)
library(SpatialEpi)
library(REdaS)
library(glue)
library(tidyverse)
library(lme4)

# create map
source("~/Documents/GitHub/BirdMigrationSpeed/map.R")
rm(list= ls()[!(ls() %in% c('rr'))])

# import data ---------------------------
BIRDDATA_PATH <- glue("data/arrival_master_2020-07-21.csv") # may not be the most current data file - but what i first found
GREENDATA_PATH <- glue("data/MidGreenup_2020-08-06-forest.csv")

bird1 <- read_csv(BIRDDATA_PATH) %>% 
  filter(VALID_GAM == TRUE)
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
velocity <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

# velocities are specific to year, species and cell
#    yr <- 2004   ;  spp <- 40  ;  b <- 38
#dat.lm1 <- as.data.frame(matrix(NA, ncol = 5))
#colnames(dat.lm1) <- c("cell_lat","cell_lng","arr_GAM_mean","cell","b")
for (yr in 2002:2017){
  for (spp in 1:length(spec)){
    dat.temp <- subset(bird, year == yr & species == spec[spp])
    for (b in 1:nrow(cells)){ # look at the neighbours of a cell
      dat.lm <- dat.lm2 <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]], b),1], c("cell_lat","cell_lng","arr_GAM_mean","cell")]   # get lat, long and Gam arrival mean
      if(nrow(dat.lm) >= minNeigh) {  # validate that minimum data points available
        if(sum(!is.na(dat.lm$arr_GAM_mean)) >= minNeigh){
          #print(c(yr, spp, b))
          #dat.lm2$b <- cells[b,1]
          #dat.lm1 <- rbind(dat.lm1,dat.lm2)
          xy <- latlong2grid(dat.lm[,2:1])  # cel positions
          coef <- lm(data = dat.lm, formula = arr_GAM_mean ~ xy$x + xy$y)$coef[2:3]
          angle <- rad2deg(atan2(coef[1],coef[2]))
          if(angle < 0) {angle = angle + 360}
          velocity <- rbind(velocity,
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

velocity <- data.frame(species = velocity[,1], 
                       year = as.numeric(velocity[,2]),
                       cell = velocity[,3],
                       NArr = as.numeric(velocity[,4]),
                       vArrMag = as.numeric(velocity[,5]),  # magnitude
                       vArrAng = as.numeric(velocity[,6]))  # angle

# calculate velocity for green up -------------
velocityG <- data.frame(year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

for(a in 2002:2017){
  dat.temp <- subset(greenup, year == a)
  for(b in 1:nrow(cells)){
    dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]],b),1], c("cell_lat","cell_lng","gr_mn","cell")]
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

# merge green up and bird velocity data
all <- merge(bird, greenup[ ,c("year","cell","gr_mn","gr_ncell")], all.x = TRUE)
all <- merge(all, velocity, all.x = TRUE)
all <- merge(all, velocityG, all.x = TRUE)
all$lag <- all$gr_mn - all$arr_GAM_mean

cellspec <- unique(all[ ,c("cell","species")])

final <- matrix(NA, 0, 27)

for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(all, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c(9,14,17,20,22)], 2, function(x) scale(log(x), scale = FALSE))
    colnames(Anom) <- c("AnomDArr", "AnomDGr", "AnomVArr", "AnomVGr", "AnomLag")
    dat.temp <- cbind(dat.temp, Anom)
    final <- rbind(final, dat.temp)
  }
}

# merge lat and lon data for cells back in
#vel <- merge(velocity, cells, by.x = c("cell"), by.y = c("cell"))
#vel$cell_lat <- scale(vel$cell_lat, scale = FALSE)  #scale lat for regression analysis

final$cell_lat <- scale(final$cell_lat, scale = FALSE)  #scale lat for regression analysis

final <- final %>% 
  drop_na(vArrMag) %>% 
  drop_na(vArrAng) %>% 
  drop_na(vGrMag) %>% 
  drop_na(vGrAng)

lmer(data = final, vArrMag ~ vGrMag + (1|cell))
lmer(data = final, vArrMag ~ vGrMag + (1|cell) + (1|species))
lm(data = final, vGrMag ~ cell_lat)

# plot velocity for species   --------------------------------
velocity$cell <- as.numeric(velocity$cell)
preds <- left_join(velocity, cells, by= "cell")
preds$ang <- NA
for(i in 1:nrow(preds)){
  if((360 - preds$vArrAng[i] + 90)>360) {preds$ang[i] = (360 - preds$vArrAng[i] + 90 - 360)}
  if((360 - preds$vArrAng[i] + 90)<360) {preds$ang[i] = (360 - preds$vArrAng[i] + 90)}
}

mag_mean <- function(magnitudes){
  magv <- magnitudes
  magm <- sum(magv)/length(magv)
  return(magm)
}

ang_mean <- function(angles){
  angv <- angles
  angm <- sum(angv)/length(angv)
  return(angm)
}

plot_mapvel <- function(sps,year){

  if(year == "all") {preds2 <- preds %>% 
    filter(species == sps)}
  
  if(year != "all") {preds2 <- preds %>% 
    filter(species == sps,
           year == year)}
  
  cell_sps <- sort(unique(preds2$cell))
  
  preds3 <- as.data.frame(matrix(data = NA, ncol = 7, nrow = length(cell_sps)))
  colnames(preds3) <- c("species","year","cell","cell_lng","cell_lat","mag","ang")
  
  for(i in 1:length(cell_sps)){
    celll <- cell_sps[i]
    pred_loop <- preds2 %>% filter(cell == celll)
    if(nrow(pred_loop) == 1) {
      preds3$year[i] <- pred_loop$year
      preds3$cell[i] <- pred_loop$cell
      preds3$mag[i] <- pred_loop$vArrMag
      preds3$ang[i] <- pred_loop$vArrAng
      preds3$cell_lng[i] <- pred_loop$cell_lng
      preds3$cell_lat[i] <- pred_loop$cell_lat
    } else {
      preds3$year[i] <- pred_loop$year[1]
      preds3$cell[i] <- pred_loop$cell[1]
      preds3$mag[i] <- mag_mean(pred_loop$vArrMag)
      preds3$ang[i] <- ang_mean(pred_loop$vArrAng)
      preds3$cell_lng[i] <- pred_loop$cell_lng[1]
      preds3$cell_lat[i] <- pred_loop$cell_lat[1]
    }
  }
  
  preds3$species <- sps
  
  preds3 <- preds3[which(preds3$mag < 10000),]
  preds3 <- preds3 %>% 
    mutate(
      #x = cell_lng + (cos(ang) * log(mag)),
      #y = cell_lat + (sin(ang) * log(mag))
      x = cell_lng + log((mag)) * cos(ang * pi / 180),
      y = cell_lat + log((mag)) * sin(ang * pi / 180)
    )
  
  rr +
    geom_segment(data = preds3, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell, colour = "red"),
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
    ggtitle(glue("{sps} {year}"))
}

sps <- "Hirundo_rustica" # "Tachycineta_bicolor"
year <- 2012 # "all"

plot_mapvel(sps,year)

# plot(rbind(preds3$cell_lng,preds3$x), rbind(preds3$y,preds3$cell_lat), col = "white") #, xlim = c(-120,-40), ylim = c(15,70))
# arrows(x0=preds3$cell_lng, y0=preds3$cell_lat, x1=preds3$x, y1=preds3$y ,col= 'red', length = 0.075)
# arrows(x0=preds3$cell_lng, y0=preds3$cell_lat, x1=preds3$x2, y1=preds3$y2 ,col= 'blue', length = 0.075)

# plot velocity for green up   --------------------------------
velocityG$cell <- as.numeric(velocityG$cell)
velG <- left_join(velocityG, cells, by= "cell")
velG$ang <- NA
for(i in 1:nrow(velG)){
  if((360 - velG$vGrAng[i] + 90)>360) {velG$ang[i] = (360 - velG$vGrAng[i] + 90 - 360)}
  if((360 - velG$vGrAng[i] + 90)<360) {velG$ang[i] = (360 - velG$vGrAng[i] + 90)}
}

mag_mean <- function(magnitudes){
  magv <- magnitudes
  magm <- sum(magv)/length(magv)
  return(magm)
}

ang_mean <- function(angles){
  angv <- angles
  angm <- sum(angv)/length(angv)
  return(angm)
}

plot_mapvel <- function(year){
  
  if(year == "all") {velG2 <- velG}
  
  if(year != "all") {velG2 <- velG %>% 
    filter(year == year)}
  
  cell_gr <- sort(unique(velG2$cell))
  
  velG3 <- as.data.frame(matrix(data = NA, ncol = 6, nrow = length(cell_gr)))
  colnames(velG3) <- c("year","cell","cell_lng","cell_lat","mag","ang")
  
  for(i in 1:length(cell_gr)){
    celll <- cell_gr[i]
    pred_loop <- velG2 %>% filter(cell == celll)
    if(nrow(pred_loop) == 1) {
      velG3$year[i] <- pred_loop$year
      velG3$cell[i] <- pred_loop$cell
      velG3$mag[i] <- pred_loop$vGrMag
      velG3$ang[i] <- pred_loop$vGrAng
      velG3$cell_lng[i] <- pred_loop$cell_lng
      velG3$cell_lat[i] <- pred_loop$cell_lat
    } else {
      velG3$year[i] <- pred_loop$year[1]
      velG3$cell[i] <- pred_loop$cell[1]
      velG3$mag[i] <- mag_mean(pred_loop$vGrMag)
      velG3$ang[i] <- ang_mean(pred_loop$vGrAng)
      velG3$cell_lng[i] <- pred_loop$cell_lng[1]
      velG3$cell_lat[i] <- pred_loop$cell_lat[1]
    }
  }
  
  velG3 <- velG3[which(velG3$mag < 10000),]
  velG3 <- velG3 %>% 
    mutate(
      #x = cell_lng + (cos(ang) * log(mag)),
      #y = cell_lat + (sin(ang) * log(mag))
      x = cell_lng + log((mag)) * cos(ang * pi / 180),
      y = cell_lat + log((mag)) * sin(ang * pi / 180)
    )
  
  rr +
    geom_segment(data = velG3, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell), colour = "darkgreen",
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
    ggtitle(glue("Green up {year}"))
}

plot_mapvel(2002)


# correlation between green up and bird velocity  -----------------
(gm1 <- lmer(data = final, vArrMag ~ vGrMag + (1|cell) + (1|species) + (1|year)))
newdata1 <- unique(final[,c("cell","vGrMag")])
newdata1 <- newdata1 %>% 
  group_by(cell) %>% 
  summarise(mean(vGrMag)) 
colnames(newdata1)[2] <- "vGrMag"
mag <- predict(gm1,newdata1,re.form=NA) # new data, level-0
newdata1 <- cbind(newdata1, mag)

(gm2 <- lmer(data = final, vArrAng ~ vGrAng + (1|cell) + (1|species) + (1|year)))
newdata2 <- unique(final[,c("cell","vGrAng")])
newdata2 <- newdata2 %>% 
  group_by(cell) %>% 
  summarise(mean(vGrAng))
colnames(newdata2)[2] <- "vGrAng"
ang <- predict(gm2,newdata2,re.form=NA) # new data, level-0
newdata2 <- cbind(newdata2, ang)

cells2 <- unique(sort(rbind(newdata1$cell,newdata2$cell)))
preds <- left_join(newdata1,newdata2, by = "cell")

