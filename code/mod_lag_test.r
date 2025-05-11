if(!require(freshr)){install.packages('freshr')}
freshr::freshr()
#
# Load packages ---------------------------------------
library(conflicted)
library(tidyverse)
library(glue)
library(mgcv)
library(gratia)
library(ggplot2)
library(dplyr)
library(tidymv)
library(itsadug)
library(viridis)
library(scales)
library(here)
library(readr)
library(janitor)
library(ggrepel)
library(zoo)
library(geosphere)
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(scales::alpha)
# conflicts_prefer(scales::alpha)

# Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)
#
# Color-blind friendly colors:
# Green-up: #009E73 (green)
# Bird: #CC79A7 (pink)
# Migratory range: #E69F00 (orange)
# Breeding range: #F0E442 (yellow)
# Source code -----------------------------------------
#
# Import data -----------------------------------------
# define maximum speed threshold
spe_thres <- 3000
## file paths
CELL_NUMB_PATH <- glue("data/cellnumbs_st{spe_thres}.rds")
CELL_COOR_PATH <- glue("data/cellcoor_st{spe_thres}.rds")
FINAL_DATA_PATH <- glue("data/final_st{spe_thres}.rds")

# FINAL_DATA_PATH <- glue("data/final copy.rds")
spe_thres <- "old"

## read files and load data
cellnumbs <- read_rds(file = CELL_NUMB_PATH)
cells <- read_rds(file = CELL_COOR_PATH)

#final2 <- readRDS("data/final2_2.rds") %>% 
final2 <- read_rds(FINAL_DATA_PATH) %>% 
   mutate(species = as.factor(species), 
          cell = as.factor(cell),
          #mig_cell = abs(mig_cell - 1),
          mig_cell = as.factor(mig_cell),# %>% as.numeric(),
          sps_cell = as.factor(glue("{species}_{cell}"))
  )

# Model 4 - Anomaly on lag varying according to z-scores --------------------------------------------------------
final_lag1 <- final2 %>% 
  dplyr::select(species,
                year,
                cell, cell_lat2, cell_lng,
                sps_cell,
                ea_lat, ea_lat_ano, ea_lat_yr, #ea_lat_yr_ano,
                arr_GAM_mean, 
                vArrMag, AnomVArr,
                gr_mn, AnomDGr,
                vGrMag, AnomVGr,
                lag, AnomLag, breed_cell) %>% 
  filter(!is.na(arr_GAM_mean))%>% 
  filter(!is.na(ea_lat)) %>% 
  filter(breed_cell == T) %>% 
  mutate(cell = as.numeric(cell))

spslist <- sort(unique(final_lag1$species))

final_lag5 <- as.data.frame(matrix(NA, ncol = 4, nrow = 0))
## get species past speed
for(s in 1:length(spslist)){
  final_lag2 <- final_lag1 %>% filter(species == spslist[s])
  years <- final_lag2 %>% dplyr::select(year) %>% arrange() %>% distinct() %>% pull
  for(y in 1:length(years)){
    final_lag3 <- final_lag2 %>% filter(year == years[y])
    cellsLoop <- final_lag3 %>% dplyr::select(cell) %>% arrange() %>% distinct() %>% pull
    # get minimum cell and date for that year
    date <- final_lag3 %>% dplyr::select(ea_lat) %>% pull() %>% first()
    place <- final_lag3 %>% dplyr::select(cell_lat2, cell_lng) %>% arrange(cell_lat2) %>% filter(row_number()==1)
    for(d in 1:length(cellsLoop)){
      final_lag4 <- final_lag3 %>% filter(cell == cellsLoop[d])
      date2 <- final_lag4 %>% dplyr::select(arr_GAM_mean) %>% pull()
      
      place2 <- final_lag4 %>% dplyr::select(cell_lat2, cell_lng) %>% filter(row_number()==1)
      colnames(place2) <- colnames(place) <- c('lat', 'lon')
      dist_a <- c(place2$lon[1], place2$lat[1])
      dist_b <- c(place$lon[1], place$lat[1])
       names(dist_a) <- names(dist_b) <- c('lon', 'lat') # RIGHT
      # names(dist_a) <- names(dist_b) <- c('lat', 'lon') # WRONG

    #   spe_bef <- (geodist::geodist(dist_a, dist_b, 
    #               measure = 'geodesic' )/1000)/(date2 - date)
      spe_bef <- distm(c(place2$lon[1], place2$lat[1]), c(place$lon[1], place$lat[1]), fun = distHaversine)

      if(!is.na(spe_bef) & spe_bef > 0) {final_lag5 <- rbind(final_lag5,
                                            cbind(as.data.frame(spslist[s]), years[y], cellsLoop[d], spe_bef))
                                            } else {print(glue("{spe_bef} s <- {s}; y <-{y}; d <- {d}"))}
    }
  }
}

colnames(final_lag5) <- c("species", "year", "cell", "past_spe")
final_lag5$year <- as.numeric(final_lag5$year)
final_lag5$cell <- as.numeric(final_lag5$cell)
final_lag5$past_spe <- as.numeric(final_lag5$past_spe)

final_lag <- left_join(final_lag1, final_lag5, by = c("species", "year", "cell"))
cellspec <- unique(final_lag[ ,c("cell","species")]) %>% as_tibble()

final3 <- matrix(NA, 0, (ncol(final_lag) + 3))

## scale past speed, green-up day and sps arrival date
for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- final_lag %>% filter(cell == pull(cellspec[a,1]),
                                   species == pull(cellspec[a,2]))
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c("past_spe","ea_lat_yr","gr_mn")], 2, 
                  function(x) scale(x, scale = T))
    colnames(Anom) <- c("past_spe_z","ea_lat_yr_z","gr_mn_z")
    dat.temp <- cbind(dat.temp, Anom)
    final3 <- rbind(final3, dat.temp)
  }
}

cor(final3$past_spe_z, final3$ea_lat_yr_z, use = "na.or.complete")
cor(final3$past_spe_z, final3$gr_mn_z, use = "na.or.complete")
cor(final3$gr_mn_z, final3$ea_lat_yr_z, use = "na.or.complete")

final3_clean <- final3 %>%
  filter(!is.na(AnomLag) & !is.na(past_spe_z) & !is.na(gr_mn_z) & !is.na(ea_lat_yr_z))

mod_lag <- 
  mgcv::gam(data = final3, 
            AnomLag ~ past_spe_z + ea_lat_yr_z + gr_mn_z + 
              s(species, bs = "re") +
              s(sps_cell, bs = "re")
  ) 
summary(mod_lag)

write_rds(mod_lag,    file = glue("data/mod_lag_st{spe_thres}.rds"))
write_rds(final_lag,  file = glue("data/final_lag{spe_thres}.rds"))
write_rds(final_lag2, file = glue("data/final_lag2{spe_thres}.rds"))
write_rds(final_lag3, file = glue("data/final_lag3{spe_thres}.rds"))
write_rds(final_lag4, file = glue("data/final_lag4{spe_thres}.rds"))
write_rds(final_lag5, file = glue("data/final_lag5{spe_thres}.rds"))
write_rds(final2,     file = glue("data/final2{spe_thres}.rds"))
write_rds(final3,     file = glue("data/final3{spe_thres}.rds"))

spe_thres <- "old"
mod_lag_old <- read_rds(file = glue("data/mod_lag_st{spe_thres}.rds"))
final_lag_old  <- read_rds(file = glue("data/final_lag{spe_thres}.rds"))
final_lag2_old <- read_rds(file = glue("data/final_lag2{spe_thres}.rds"))
final_lag3_old <- read_rds(file = glue("data/final_lag3{spe_thres}.rds"))
final_lag4_old <- read_rds(file = glue("data/final_lag4{spe_thres}.rds"))
final_lag5_old <- read_rds(file = glue("data/final_lag5{spe_thres}.rds"))
final2_old <- read_rds(file = glue("data/final2{spe_thres}.rds"))
final3_old <- read_rds(file = glue("data/final3{spe_thres}.rds"))

#########################################################################################

dim(final2)

dim(final2_old)

final2 <- final2  %>% 
            mutate(unikey = glue("{sps_cell}_{year}"))

final2_old <- final2_old  %>% 
            mutate(unikey = glue("{sps_cell}_{year}"))

table(final2$unikey %in% final2_old$unikey)
table(final2_old$unikey %in% final2$unikey)

# HA! why are those not there?
final2[which(final2$unikey %!in% final2_old$unikey),]



final_com <- full_join(final2, 
                       final2_old, 
                       by = "unikey") %>% 
             select(sort(names(.)))

str(final_com)

view(final_com  %>% 
    select(vArrMag.x, vArrMag.y)  %>% 
    mutate(same = (vArrMag.x == vArrMag.y)))

final_lag1_old 
final_lag1

final3 <- final3  %>% 
            mutate(unikey = glue("{sps_cell}_{year}"))

final3_old <- final3_old  %>% 
            mutate(unikey = glue("{sps_cell}_{year}"))

table(final3$unikey %in% final3_old$unikey)
table(final3_old$unikey %in% final3$unikey)

######################################
summary(mod_lag)
mod_lag_old

final_lag
final_lag_old

final_lag2
final_lag2_old

final_lag3
final_lag3_old

final_lag4
final_lag4_old

final_lag5
final_lag5_old

final3
final3_old

mean_a <- 0.45401
sd_a <- 0.01119
mean_a -(1.96 * sd_a)
mean_a +(1.96 * sd_a)
