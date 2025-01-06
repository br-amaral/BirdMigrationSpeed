## Code to add species traits data from the literature and also calculate extra traits from the data
#
## Input:  data/birdgreen.rds : tibble with bird speed calculations from 1_GetEstimates.R 
#          data/velocityG.rds : 
#          data/cellcoor.rds :     
#          data/cellnumbs.rds : 
#
#          data/source/traits/Table_S1.csv : bird overwinter latitude data (Youngflesh et al. [2021])
#          data/source/traits/data_sensi.rds : bird sensitivity data (Youngflesh et al. [2021])
#          data/source/traits/gcb14540-sup-0001-supinfo_mass.csv : bird body mass data (Horton et al. [2019])
#          data/source/traits/jane13345-sup-0002-tables1_diet.csv : bird diet data (La Sorte & Graham [2021])
#          data/source/traits/species_tax_ord.csv : bird family data (BirdTree.org)
#          data/source/traits/birds_HWI.csv : bird hand-wing index (Sheard et al. [2020])
#           : Bird migration time data (Birds of the World [2022])

## Output: 

# load packages --------------------------
library(egg)
library(ggplot2)
library(tidyverse)
library(viridis)
library("glue")
library(lemon)
library(lme4)
library(mgcv)

# import data ---------------------------
# define maximum speed threshold
spe_thres <- 4000

## file paths
BIRD_SPEED_PATH <- glue("data/birdgreen_st{spe_thres}.rds")
GUP_SPEED_PATH  <- glue("data/velocityG_st{spe_thres}.rds")

CELL_COOR_PATH <- glue("data/cellcoor_st{spe_thres}.rds")
CELL_NUMB_PATH <- glue("data/cellnumbs_st{spe_thres}.rds")

TRAIT_OVER_PATH <- "data/source/traits/Table_S1.csv"
TRAIT_SENS_PATH <- "data/source/traits/data_sensi.rds"
TRAIT_MASS_PATH <- "data/source/traits/gcb14540-sup-0001-supinfo_mass.csv"
TRAIT_DIET_PATH <- "data/source/traits/jane13345-sup-0002-tables1_diet.csv"
TRAIT_FAMI_PATH <- "data/source/traits/species_tax_ord.csv"
TRAIT_HWI_PATH <- "data/source/traits/birds_HWI.csv"
TRAIT_MIGTIM_PATH <- "data/source/traits/sps_migtime.csv"

## read files
final <- read_rds(file = BIRD_SPEED_PATH)

cells <- readRDS(CELL_COOR_PATH)

velocityG <- readRDS(GUP_SPEED_PATH) %>% 
  left_join(., cells, by = "cell")
dimfinal <- nrow(final)

# Add data from the literature and format tibbles ------------------------------------ 
## overwinter latitude (from how far south do birds come) ---------------------------
winlat <- read_csv(TRAIT_OVER_PATH) %>%
  dplyr::select(Species, Overwinterlatitude) %>% 
  mutate(species2 = Species) %>% 
  rename(species = Species, 
         winlat = Overwinterlatitude) %>% 
  mutate(species = sub(" ", "_", species))

final <- left_join(final, winlat, by = "species")

# check!
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

## species sensitivity --------------------------------------------------------------
# sensitivity for a species in a cell
sensi <- readRDS(TRAIT_SENS_PATH) %>% 
  dplyr::select(sci_name,
                cell,
                beta_mean) %>% 
  rename(species = sci_name,
         sensi = beta_mean) 

cellnumbs <- readRDS(CELL_NUMB_PATH)
sensi <- left_join(sensi, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2)

final <- left_join(final, sensi, by = c("species","cell")) 

# check!
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

# sensitivity per species
sensisps_c <- sensi %>% 
  group_by(species) %>% 
  summarise(sensi_mean = mean(sensi, na.rm = T),
            sensi_sd = sd(sensi, na.rm = T))

final <- final %>% 
  left_join(., sensisps_c, by = "species")

# check!
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

# add cell coordinates
final <- left_join(final,
                   cells %>% 
                     dplyr::select(cell, cell_lat) %>% 
                     rename(cell_lat2 = cell_lat),
                   by = "cell")

# check again!
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

## Species body mass ----------------------------------------------------------------------------------
mass_tax <- read_csv(TRAIT_MASS_PATH) %>% 
  dplyr::select(species,Order,Family,Body_mass_g)

unique(final[which(final$species %in% pull(mass_tax[which(is.na(mass_tax$Body_mass_g)),1])),3])

final <- left_join(final, mass_tax, by = "species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

## Species migration time -----------------------------------------------------------------------------
mig_time <- read_csv(TRAIT_MIGTIM_PATH) %>% 
  dplyr::select(species, Time)

final <- left_join(final, mig_time, by = "species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

## Species diet group ---------------------------------------------------------------------------------
diet <- read_csv(TRAIT_DIET_PATH) %>% 
  dplyr::select(Species, Diet) %>% 
  rename(species = Species)
unique(final[which(final$species %in% pull(diet[which(is.na(diet$Diet)),1])),3])

final <- left_join(final, diet, by = "species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

## Species taxonomic order (family) -------------------------------------------------------------------
famcol <- read_csv(TRAIT_FAMI_PATH)
final <- left_join(final, famcol, by="species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

## Species hand-wing index ----------------------------------------------------------------------------
# wing shape values
hwicol <- read_csv(TRAIT_HWI_PATH) %>% 
  dplyr::select(species, `HWI`)  ## used the IUCN names
final <- left_join(final, hwicol, by="species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

# Calculate species specific metrics from data ----------------------------------------------------
## date when birds first arrive in North America (on average) -------------------------------------
# mean arrival date for cells under 35N latitude
ea_tab_l <- final %>% 
  dplyr::select(species, cell_lat2, cell, arr_GAM_mean, mig_cell) %>% 
  distinct() %>% 
  group_by(species) %>% 
  filter(cell_lat2 < 35,
         mig_cell == T) %>% 
  mutate(ea_lat = mean(arr_GAM_mean, na.rm = T)) %>% 
  dplyr::select(species, ea_lat) %>% 
  distinct()

ear_lat <- final %>% 
  dplyr::select(species, cell_lat2,cell_lat, cell, arr_GAM_mean, year, mig_cell) %>% 
  distinct() %>% 
  filter(cell_lat2 < 35,
         mig_cell == T)  
spseal_lm <- lmer(data = ear_lat, arr_GAM_mean ~ as.factor(species) -1 + (1|year) + (1|cell) + (1|cell_lat2))

spseal <- cbind(substring(names(getME(spseal_lm, name = "fixef")), 19),
                as.numeric(getME(spseal_lm, name = "fixef")))

colnames(spseal) <- c("species", "ea_lat_m")
spseal <- as.data.frame(spseal)
spseal$ea_lat_m <- as.numeric(spseal$ea_lat_m)

final <- final %>% 
  left_join(., spseal, by = "species") 

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))

ea_tab_l_yr <- final %>% 
  dplyr::select(species, cell_lat2, cell,arr_GAM_mean, AnomDArr, year) %>% 
  distinct() %>% 
  group_by(species, year) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat_yr = mean(arr_GAM_mean, na.rm = T),
         ea_lat_yr_ano = mean(AnomDArr, na.rm = T)) %>% 
  dplyr::select(species, year, ea_lat_yr, ea_lat_yr_ano) %>% 
  distinct()

ea_tab_ano <- final %>% 
  dplyr::select(species, year, cell_lat2, cell, AnomDArr) %>% 
  distinct() %>% 
  group_by(species, year) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat_ano = mean(AnomDArr, na.rm = T)) %>% 
  dplyr::select(species, year, ea_lat_ano) %>% 
  distinct()

final <- left_join(final, ea_tab_l, by = "species")

final <- left_join(final, ea_tab_l_yr, by = c("species", "year"))

final <- left_join(final, ea_tab_ano, by = c("species", "year"))

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnames(final))


## remove rows with bird speed greater than spe_thres
for(i in 1:nrow(final)){
  if(is.na(final$vArrMag[i])) {final$vArrMag[i]
  } else {if(final$vArrMag[i] > spe_thres) {final$vArrMag[i] <- NA}}
}

write_rds(final, file = "data/final_st{spe_thres}.rds")
