library(sp)
library(spdep)
library(raster)
library(SpatialEpi)
library(REdaS)
library(glue)

# import data ---------------------------
BIRDDATA_PATH <- glue("data/arrival_master_2020-07-21.csv") # may not be the most current data file - but what i first found
GREENDATA_PATH <- glue("data/MidGreenup_2020-08-06-forest.csv")

bird <- read.csv(BIRDDATA_PATH)
bird <- subset(bird, VALID_GAM == TRUE)

greenup <- read.csv(GREENDATA_PATH)
greenup <- subset(greenup, gr_ncell > 10000)

spec <- unique(bird$species)  # list of species

# get the neighbor set for each cell within a set buffer --------------------------
minNeigh <- 5   # threshold to only calculating velocities that had at least X number of neighbors
cells <- unique(greenup[ ,c("cell","cell_lat","cell_lng")])    # data points - cells and their locations 

adj <- dnearneigh(as.matrix(cells[,3:2]), 0, 360, longlat= TRUE)     # list of adjacent cells for each cell
num <- sapply(adj, length)         # number of neighbors for each cell
cells <- cells[num >= minNeigh,]   # remove cells with too few neighbors
                                   #   a later step will reassess whether minimum neighbors for individual species and years 
adj <- adj[num >= minNeigh]        # eliminate cells with too few neighbors
num <- sapply(adj,length)          # recalculate num (number of neighbors) for the reduced data set

# calculate velocity for birds --------------------------
velocity <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

# velocities are specific to cell, year, and species 
for (yr in 2002:2017){
  for (spp in 1:length(spec)){
    dat.temp <- subset(bird, year == yr & species == spec[spp])
    for (b in 1:nrow(cells)){
      dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]], b),1], c(6,7,9)]
      if(nrow(dat.lm) >= minNeigh) {  # validate that minimum data points available
        if(sum(!is.na(dat.lm$arr_GAM_mean)) >= minNeigh){
          xy <- latlong2grid(dat.lm[,2:1])
          coef <- lm(data = dat.lm, formula = arr_GAM_mean ~ xy$x + xy$y)$coef[2:3]
          angle <- rad2deg(atan2(coef[1],coef[2]))
          if(angle < 0) {angle = angle + 360}
          velocity <- rbind(velocity,
                            c(spec[spp],
                              yr,
                              cells[b,1],
                              sum(!is.na(dat.lm$arr_GAM_mean)),
                              1/sqrt(sum(coef^2)), angle)
                            )
        }
      }
    }
  }
}

# update the data.frame ------------------------
velocity <- data.frame(species = velocity[,1], 
                       year = as.numeric(velocity[,2]),
                       cell = velocity[,3],
                       NArr = as.numeric(velocity[,4]),
                       vArrMag = as.numeric(velocity[,5]),
                       vArrAng = as.numeric(velocity[,6]))

velocityG <- data.frame(year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

for(a in 2002:2017){
    dat.temp <- subset(greenup, year == a)
    for(b in 1:nrow(cells)){
      dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]],b),1],4:6]
      if(nrow(dat.lm) >= minNeigh) {       # validate that minimum data points available
        if(sum(!is.na(dat.lm$gr_mn)) >= minNeigh){
          xy <- latlong2grid(dat.lm[ ,2:1])
          coef <- lm(data = dat.lm, formula = gr_mn ~ xy$x + xy$y)$coef[2:3]
          angle <- rad2deg(atan2(coef[1],coef[2]))
          if(angle < 0) {angle = angle + 360}
          velocityG <- rbind(velocityG, c(a, cells[b,1], sum(!is.na(dat.lm$gr_mn)), 1/sqrt(sum(coef^2)), angle))
        }
      }
    }
  }

# calculate velocity for green up
velocityG <- data.frame(year = as.numeric(velocityG[,1]),
                        cell = velocityG[,2],
                        NGr = as.numeric(velocityG[,3]),
                        vGrMag = as.numeric(velocityG[,4]),
                        vGrAng = as.numeric(velocityG[,5]))

all <- merge(bird, greenup[,c(2,3,6,7)], all.x = TRUE)
all <- merge(all, velocity, all.x = TRUE)
all <- merge(all, velocityG, all.x = TRUE)
all$lag <- all$gr_mn - all$arr_GAM_mean

cellspec <- unique(all[ ,c("cell","species")])

final <- matrix(NA, 0, 27)

for (a in 1:nrow(cellspec)){
  dat.temp <- subset(all, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){
    Anom <- apply(dat.temp[,c(9,14,17,20,22)], 2, function(x) scale(log(x), scale = FALSE))
    colnames(Anom) <- c("AnomDArr", "AnomDGr", "AnomVArr", "AnomVGr", "AnomLag")
    dat.temp <- cbind(dat.temp, Anom)
    final <- rbind(final, dat.temp)
  }
}







### merge lat and lon data for cells back in
vel <- merge(velocity, cells, by.x = c("cell"), by.y = c("cell"))
vel$cell_lat <- scale(vel$cell_lat, scale = FALSE)  ### scale lat for regression analysis
library(lme4)


lmer(data = vel, vArrMag ~ vGrMag + (1|Cell))
lm(data = vel, vGrMag ~ cell_lat)
