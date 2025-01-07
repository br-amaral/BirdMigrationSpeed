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

# define number of simulations
nsims <- 10

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

for(jj in 1:nsims){
    # calculate velocity for birds --------------------------
    for (yr in 2002:2017){
        for (spp in 1:length(spec)){
            dat.temp <- subset(bird, year == yr & species == spec[spp])
            for (b in 1:nrow(cells)){ # look at the neighbours of a cell   c(adj[[b]], cells[b,1])
            # print(cells[b,])
            vecnei <- c(pull(cells[unlist(cells$adj[b]),1]), pull(cells[b,1]))
            dat.lm <- dat.temp[dat.temp$cell %in% vecnei, c("cell_lat","cell_lng","arr_GAM_mean","cell")]  
            rm(vecnei)
            #dat.lm <- dat.temp[dat.temp$cell %in% cells[c(adj[[b]],b),1], c("cell_lat","cell_lng","arr_GAM_mean","cell")]   # get lat, long and Gam arrival mean
            if(nrow(dat.lm) >= minNeigh) {  # validate that minimum data points available
                if(sum(!is.na(dat.lm$arr_GAM_mean)) >= minNeigh + 1){
                #print(c(yr, spp, b))
                #dat.lm2$b <- cells[b,1]
                #dat.lm1 <- rbind(dat.lm1,dat.lm2)

                # randomly remove one data point
                dat.lm <- dat.lm[runif(1,1,nrow(dat.lm)),]

                xy <- latlong2grid(dat.lm[,2:1])  # cel positions
                colnames(xy) <- c('x','y')
                coef <- lm(data = dat.lm, formula = arr_GAM_mean ~ xy$x + xy$y)$coef[2:3]
                angle <- rad2deg(atan2(coef[1],coef[2]))
                if(angle < 0) {angle = angle + 360}
                if(exists("velocityB")) {
                    velocityB <- rbind(velocityB,
                                       c(as.numeric(jj),
                                         as.character(spec[spp]), 
                                         as.numeric(yr),
                                         as.numeric(cells[b,1]),
                                         as.numeric(sum(!is.na(dat.lm$arr_GAM_mean))),
                                         as.numeric(
                                         #as.numeric(sqrt((1/coef[1])^2+(1/coef[2])^2))
                                         1/sqrt(sum(coef^2))
                                         ),  # magnitude
                                         as.numeric(as.numeric(angle))))
                    } else {
                        velocityB <- data.frame(
                                    simu = as.numeric(jj),
                                    species = as.character(spec[spp]), 
                                    year = as.numeric(yr),
                                    cell = as.numeric(cells[b,1]),
                                    NArr = as.numeric(sum(!is.na(dat.lm$arr_GAM_mean))),
                                    vArrMag = as.numeric(
                                        #as.numeric(sqrt((1/coef[1])^2+(1/coef[2])^2))
                                        1/sqrt(sum(coef^2))
                                        ),  # magnitude
                                    vArrAng = as.numeric(as.numeric(angle))) %>% as_tibble()
                            }
                    }
                }
            }
        }
    }
}
velocityB <- velocityB %>% 
                mutate(year = as.numeric(year),
                       cell = as.numeric(cell),
                       NArr = as.numeric(NArr),
                       vArrMag = as.numeric(vArrMag),  # magnitude
                       vArrAng = as.numeric(vArrAng))  # angle

