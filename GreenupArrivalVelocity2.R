library(sp)
library(spdep)
library(raster)
library(SpatialEpi)
library(REdaS)
library(glue)
library(tidyverse)
library(lme4)
library(gganimate)

# create map
source("~/Documents/GitHub/BirdMigrationSpeed/map.R")
rm(list= ls()[!(ls() %in% c('rr', 'rrb', 'pp'))])

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
velocity <- data.frame(spec = c(), year = c(), cell = c(), N = c(), vArrMag = c(),vGrMag = c())

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

velocity$cell <- as.numeric(velocity$cell)

preds <- left_join(velocity, cells, by= "cell")
preds$ang <- NA
for(i in 1:nrow(preds)){
  if((360 - preds$vArrAng[i] + 90)>360) {preds$ang[i] = (360 - preds$vArrAng[i] + 90 - 360)}
  if((360 - preds$vArrAng[i] + 90)<360) {preds$ang[i] = (360 - preds$vArrAng[i] + 90)}
}


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

# plot velocity for species   --------------------------------

plot_mapvel <- function(sps,year){

  if(year == "all") {preds2 <- preds %>% 
    filter(species == sps)}
  
  if(year != "all") {preds2 <- preds %>% 
    filter(species == sps,
           year == year)}
  
  cell_sps <- sort(unique(preds2$cell))
  
  preds2 <- preds2 %>% 
    filter(vArrMag < 10000) %>% 
    mutate(
      #x = cell_lng + (cos(ang) * log(mag)),
      #y = cell_lat + (sin(ang) * log(mag))
      x = cell_lng + log((vArrMag)) * cos(ang * pi / 180),
      y = cell_lat + log((vArrMag)) * sin(ang * pi / 180)
    )
  
  preds3 <- as.data.frame(matrix(data = NA, ncol = 7, nrow = length(cell_sps)))
  colnames(preds3) <- c("species","year","cell","cell_lng","cell_lat","x","y")
  
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
    } else {
      preds3$year[i] <- pred_loop$year[1]
      preds3$cell[i] <- pred_loop$cell[1]
      preds3$cell_lng[i] <- pred_loop$cell_lng[1]
      preds3$cell_lat[i] <- pred_loop$cell_lat[1]
      preds3$x[i] <- mean(x = pred_loop$x)
      preds3$y[i] <- mean(x = pred_loop$y)
    }
  }
  
  preds3$species <- sps
  
  #rr +
  #rrb + 
  pp +
    geom_segment(data = preds3, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell, colour = "red"),
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
    ggtitle(glue("{sps} {year}")) +
    theme_bw() +
    theme(legend.position = "none",
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
          panel.border = element_blank()) 
}

sps <- "Setophaga_discolor" # "Hirundo_rustica" # "Tachycineta_bicolor"
year <-  "all"

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

plot_mapvelG <- function(year){
  
  if(year == "all") {velG2 <- velG}
  
  if(year != "all") {velG2 <- velG %>% 
    filter(year == year)}
 
  cell_G <- sort(unique(velG2$cell))
  
  velG2 <- velG2 %>% 
    filter(vGrMag < 10000) %>% 
    mutate(
      #x = cell_lng + (cos(ang) * log(mag)),
      #y = cell_lat + (sin(ang) * log(mag))
      x = cell_lng + log((vGrMag)) * cos(ang * pi / 180),
      y = cell_lat + log((vGrMag)) * sin(ang * pi / 180)
    )
  
  velG3 <- as.data.frame(matrix(data = NA, ncol = 5, nrow = length(cell_G)))
  colnames(velG3) <- c("cell","cell_lng","cell_lat","x","y")
  
  for(i in 1:length(cell_G)){
    celll <- cell_G[i]
    pred_loop <- velG2 %>% filter(cell == celll)
    if(nrow(pred_loop) == 1) {
      velG3$cell[i] <- pred_loop$cell
      velG3$cell_lng[i] <- pred_loop$cell_lng
      velG3$cell_lat[i] <- pred_loop$cell_lat
      velG3$x[i] <- pred_loop$x
      velG3$y[i] <- pred_loop$y
    } else {
      velG3$cell[i] <- pred_loop$cell[1]
      velG3$cell_lng[i] <- pred_loop$cell_lng[1]
      velG3$cell_lat[i] <- pred_loop$cell_lat[1]
      velG3$x[i] <- mean(x = pred_loop$x)
      velG3$y[i] <- mean(x = pred_loop$y)
    }
  }  
  
  rr +
    geom_segment(data = velG3, aes(x = cell_lng, y = cell_lat, 
                                    xend = x, yend = y,
                                    group = cell), colour = "darkgreen",
                 arrow = arrow(length = unit(0.1, "cm")), size = 0.5) +
    ggtitle(glue("Green up {year}"))
}

plot_mapvelG("all")


# correlation between green up and bird velocity  -----------------
# merge green up and bird velocity data
all <- merge(bird, greenup[ ,c("year","cell","gr_mn","gr_ncell")], all.x = TRUE)
all <- merge(all, velocity, all.x = TRUE)
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

#a <- lmer(data = final, vArrMag ~ vGrMag + (1|cell))
#b <- lmer(data = final, vArrMag ~ vGrMag + (1|cell) + (1|species))
#lm(data = final, vGrMag ~ cell_lat)

(mm1 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|cell) + (1|species) + (1|year)))
(mm2 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|cell) + (1|species)))
(mm3 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|cell) + (1|year)))
(mm4 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|species) + (1|year)))
(mm5 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|cell) ))
(mm6 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|species)))
(mm7 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + (1|year)))
(mm8 <- lm(data = final2, vArrMag_s ~ vGrMag_s))
(mm9 <- lmer(data = final2, vArrMag_s ~ vGrMag_s + cell_lat2 + (1|cell)+ (1|species)))

AIC(mm1,mm2,mm3,mm4,mm5,mm6,mm7,mm8,mm9) %>% arrange(AIC)  # mm1 best, but mm2 is close enough and simpler
sjPlot:: tab_model(mm1)

(ma1 <- lmer(data = final2, angB ~ angG + (1|cell) + (1|species) + (1|year)))
(ma2 <- lmer(data = final2, angB ~ angG + (1|cell) + (1|species)))
(ma3 <- lmer(data = final2, angB ~ angG + (1|cell) + (1|year)))
(ma4 <- lmer(data = final2, angB ~ angG + (1|species) + (1|year)))
(ma5 <- lmer(data = final2, angB ~ angG + (1|cell) ))
(ma6 <- lmer(data = final2, angB ~ angG + (1|species)))
(ma7 <- lmer(data = final2, angB ~ angG + (1|year)))
(ma8 <- lm(data = final2, angB ~ angG))

AIC(ma1,ma2,ma3,ma4,ma5,ma6,ma7,ma8) %>% arrange(AIC)  # ma1 is the best
sjPlot:: tab_model(ma1)

(vellat1 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|cell) + (1|species) + (1|year)))
(vellat2 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|cell) + (1|species)))
(vellat3 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|cell) + (1|year)))
(vellat4 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|species) + (1|year)))
(vellat5 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|cell) ))
(vellat6 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|species)))
(vellat7 <- lmer(data = final2, vArrMag_s ~ cell_lat2 + (1|year)))
(vellat8 <- lm(data = final2, vArrMag_s ~ cell_lat2))

AIC(vellat1,vellat2,vellat3,vellat4,vellat5,vellat6,vellat7,vellat8) %>% arrange(AIC)
sjPlot::tab_model(vellat1, 
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept", "Latitude"),
                  dv.labels= "Bird Migration Velocity")

(time1 <- lmer(data = final2, vArrMag_s ~ year + (1|cell) + (1|species) + (1|cell_lat2)))
(time2 <- lmer(data = final2, vArrMag_s ~ year + (1|cell) + (1|species)))
(time3 <- lmer(data = final2, vArrMag_s ~ year + (1|cell) + (1|cell_lat2)))
(time4 <- lmer(data = final2, vArrMag_s ~ year + (1|species) + (1|cell_lat2)))
(time5 <- lmer(data = final2, vArrMag_s ~ year + (1|cell) ))
(time6 <- lmer(data = final2, vArrMag_s ~ year + (1|species)))
(time7 <- lmer(data = final2, vArrMag_s ~ year + (1|cell_lat2)))
(time8 <- lm(data = final2, vArrMag_s ~ year))

AIC(time1,time2,time3,time4,time5,time6,time7,time8) %>% arrange(AIC)
sjPlot::tab_model(time2, 
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept", "Year"),
                  dv.labels= "Bird Migration Velocity")


## plots
#obvious?
ggplot(all, aes(y = vArrMag, x = lag)) +
#  geom_point() +
  geom_smooth() +
  theme_bw()

ggplot(final2, aes(y = vArrMag, x = cell_lat)) +
  #  geom_point() +
  geom_smooth() +
  theme_bw() #+  facet_wrap(~year,nrow=3)

### 
effects_vGr <- as.data.frame(effects::effect(term= "vGrMag_s", mod= mm1))
effects_lat <- as.data.frame(effects::effect(term= "cell_lat2", mod= vellat1))
effects_yr <- as.data.frame(effects::effect(term= "year", mod= time2))

ggplot() + 
  geom_point(data=final2, aes(y = vArrMag_s, x = vGrMag_s)) + 
  geom_point(data=effects_vGr, aes(x=vGrMag_s, y=fit), color="blue") +
  geom_ribbon(data= effects_vGr, aes(x=vGrMag_s, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Vegetation velocity", y="Bird velocity")

ggplot() + 
  geom_point(data=final2, aes(y = vArrMag_s, x = cell_lat2)) + 
  geom_line(data=effects_lat, aes(x=cell_lat2, y=fit), color="blue") +
  geom_ribbon(data= effects_lat, aes(x=cell_lat2, ymin=lower, ymax=upper), alpha= 0.5, fill="blue") +
  labs(x="Latitude", y="Bird velocity") +
  theme_bw()

ggplot() + 
  geom_point(data=final2, aes(y = vArrMag_s, x = year)) + 
  geom_line(data=effects_yr, aes(x=year, y=fit), color="blue") +
  geom_ribbon(data= effects_yr, aes(x=year, ymin=lower, ymax=upper), alpha= 0.5, fill="blue") +
  labs(x="Year", y="Bird velocity") +
  theme_bw()


## animation - wave going up
## have the arrows with different colors according to its size

final2 %>% 
  group_by(species) %>% 
  summarise(mean_speed = mean())

plot_mapvel("Tachycineta_bicolor", "all")
plot_mapvel("Vireo_olivaceus", "all")
plot_mapvel("Setophaga_americana", "all")
plot_mapvel("Setophaga_discolor", "all")


birdx <- final2 %>% filter(species == "Setophaga_americana")
findcel <- velocity %>% filter(species == "Setophaga_discolor")
cellfil <- sort(unique(findcel$cell))

ggplot() +
  geom_point(data = final2 %>% filter(species == "Setophaga_discolor"), 
             aes(x = cell_lat2, y = vArrMag_s, colour = "red"), size = 5) +
  ggtitle(glue("{sps} {year}")) +
  theme_bw() +
  theme(legend.position = "none",
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
        panel.border = element_blank()) 
  


