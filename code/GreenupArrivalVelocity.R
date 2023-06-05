

library(sp)
library(spdep)
library(raster)

#### may not be the most current data file - but what i first found
dat <- read.csv("arrival_master_subset_2020-06-08_.csv")

spec <- unique(dat$species)  ## list of species



####  This section figures out the neighbor set for each cell within a set buffer 

minNeigh <- 5  ### I set a threshold of only calculating velocities that had at least this many neighbors

cells <- unique(dat[,c("cell","cell_lat","cell_lng")])  ### DATA PTs  
adj = dnearneigh(as.matrix(cells[,3:2]),0,500, longlat = TRUE)  ### a list of adjacent cells for each cell
num <- sapply(adj,length)  ### the number of neighbors for each cell
cells <- cells[num >= minNeigh,]  ### eliminate cells with too few neighbors
### a later step will reassess whether minimum neighbors for individual species and years 
adj <- adj[num >= minNeigh] ### eliminate cells with too few neighbors
num <- sapply(adj,length)  ### recalculate num for the reduced data set



velocity <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())


#####  velocities are specific to cell, year, and species 
for (a in 2002:2017){
  for (spp in 1:length(spec)){
    dat.temp <- subset(dat,year == a & species == spec[spp])
    for (b in 1:nrow(cells)){
      dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]],b),1],c(6,7,9,14)]
      if(nrow(dat.lm) >= minNeigh) {  ### validate that minimum data points available
        if(sum(!is.na(dat.lm$arr_GAM_mean))>=minNeigh){

          ### velocities are calculating by regressing mean arrival and mean green by lat and lon
          ### should change to be on a spatial grid with km as units
          ### the regressions calculate days per degree lat/lon
          ### what we actual want is km/day - switch to km grid and inverse of the regression estimate
          coef1 <- lm(data = dat.lm, formula = arr_GAM_mean ~ cell_lng + cell_lat)$coef[2:3]
          coef2 <- lm(data = dat.lm, formula = gr_mn ~ cell_lng + cell_lat)$coef[2:3]
          ### velocity is speed and direction - I only calculate speed so far - need a trig refresher to get direction
          ### use pythagorean theorem to get magnitude from the lat and lon regression coefs
          velocity <- rbind(velocity, c(spec[spp],a,cells[b,1],sum(!is.na(dat.lm$arr_GAM_mean)),sqrt(sum(coef1^2)),sqrt(sum(coef2^2))))
        }
      }
    }
  }
}


#### update the data.frame 
velocity = data.frame(Species = velocity[,1],Year = as.numeric(velocity[,2]),
                      Cell = velocity[,3],N = as.numeric(velocity[,4]),
                      vArrMag = 1/as.numeric(velocity[,5]),vGrMag = 1/as.numeric(velocity[,6]))

### merge lat and lon data for cells back in
vel <- merge(velocity, cells, by.x = c("Cell"), by.y = c("cell"))
vel$cell_lat <- scale(vel$cell_lat, scale = FALSE)  ### scale lat for regression analysis
library(lme4)


lmer(data = vel, vArrMag ~ vGrMag + (1|Cell))
lm(data = vel, vGrMag ~ cell_lat)

# library(SpatialEpi)
# 
# 
# 
# velocity <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())
# 
# 
# #####  velocities are specific to cell, year, and species 
# for (a in 2002:2017){
#   for (spp in 1:length(spec)){
#     dat.temp <- subset(dat,year == a & species == spec[spp])
#     for (b in 1:nrow(cells)){
#       dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]],b),1],c(6,7,9,14)]
#       if(nrow(dat.lm) >= minNeigh) {  ### validate that minimum data points available
#         if(sum(!is.na(dat.lm$arr_GAM_mean))>=minNeigh){
#           
#           ### velocities are calculating by regressing mean arrival and mean green by lat and lon
#           ### should change to be on a spatial grid with km as units
#           ### the regressions calculate days per degree lat/lon
#           ### what we actual want is km/day - switch to km grid and inverse of the regression estimate
#           coef1 <- lm(data = dat.lm, formula = arr_GAM_mean ~ cell_lng + cell_lat)$coef[2:3]
#           coef2 <- lm(data = dat.lm, formula = gr_mn ~ cell_lng + cell_lat)$coef[2:3]
#           ### velocity is speed and direction - I only calculate speed so far - need a trig refresher to get direction
#           ### use pythagorean theorem to get magnitude from the lat and lon regression coefs
#           velocity <- rbind(velocity, c(spec[spp],a,cells[b,1],sum(!is.na(dat.lm$arr_GAM_mean)),sqrt(sum(coef1^2)),sqrt(sum(coef2^2))))
#         }
#       }
#     }
#   }
# }