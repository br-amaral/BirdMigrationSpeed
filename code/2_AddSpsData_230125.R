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

freshr::freshr()

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
spe_thres <- 3000

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

final1 <- left_join(final, winlat, by = "species")

# check!
dim(final1) ; dim(final1)[1] == dimfinal ;  tail(colnames(final1))

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

final2 <- left_join(final1, sensi, by = c("species","cell")) 

# check!
dim(final2) ; dim(final2)[1] == dimfinal ; dim(final2)[2] > dim(final1)[2] ; tail(colnames(final2))

# sensitivity per species
sensisps_c <- sensi %>% 
  group_by(species) %>% 
  summarise(sensi_mean = mean(sensi, na.rm = T),
            sensi_sd = sd(sensi, na.rm = T))

final3 <- final2 %>% 
  left_join(., sensisps_c, by = "species")

# check!
dim(final3) ; dim(final3)[1] == dimfinal ; dim(final3)[2] > dim(final2)[2] ; tail(colnames(final3))

# add cell coordinates
final4 <- left_join(final3 %>% 
                      select(-cell_lat2),
                   cells %>% 
                     dplyr::select(cell, cell_lat) %>% 
                     rename(cell_lat2 = cell_lat),
                   by = "cell")

# check again!
dim(final4) ; dim(final4)[1] == dimfinal ; dim(final4)[2] == dim(final3)[2] ; tail(colnames(final4))

## Species body mass ----------------------------------------------------------------------------------
mass_tax <- read_csv(TRAIT_MASS_PATH) %>% 
  dplyr::select(species,Order,Family,Body_mass_g)

unique(final4[which(final4$species %in% pull(mass_tax[which(is.na(mass_tax$Body_mass_g)),1])),3])

final5 <- left_join(final4, mass_tax, by = "species")
dim(final5) ; dim(final5)[1] == dimfinal ;  tail(colnames(final5))

## Species migration time -----------------------------------------------------------------------------
mig_time <- read_csv(TRAIT_MIGTIM_PATH) %>% 
  dplyr::select(species, Time)

final6 <- left_join(final5, mig_time, by = "species")
dim(final6) ; dim(final6)[1] == dimfinal ;  tail(colnames(final6))

## Species diet group ---------------------------------------------------------------------------------
diet <- read_csv(TRAIT_DIET_PATH) %>% 
  dplyr::select(Species, Diet) %>% 
  rename(species = Species)
unique(final6[which(final6$species %in% pull(diet[which(is.na(diet$Diet)),1])),3])

final7 <- left_join(final6, diet, by = "species")
dim(final7) ; dim(final7)[1] == dimfinal ;  tail(colnames(final7))

## Species taxonomic order (family) -------------------------------------------------------------------
famcol <- read_csv(TRAIT_FAMI_PATH)
final8 <- left_join(final7, famcol, by="species")
dim(final8) ; dim(final8)[1] == dimfinal ;  tail(colnames(final8))

## Species hand-wing index ----------------------------------------------------------------------------
# wing shape values
hwicol <- read_csv(TRAIT_HWI_PATH) %>% 
  dplyr::select(species, `HWI`)  ## used the IUCN names
final9 <- left_join(final8, hwicol, by="species")
dim(final9) ; dim(final9)[1] == dimfinal ;  tail(colnames(final9))

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

ear_lat <- final9 %>% 
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

final10 <- final9 %>% 
  left_join(., spseal, by = "species") 

dim(final10) ; dim(final10)[1] == dimfinal ;  tail(colnames(final10))

ea_tab_l_yr <- final10 %>% 
  dplyr::select(species, cell_lat2, cell,arr_GAM_mean, AnomDArr, year) %>% 
  distinct() %>% 
  group_by(species, year) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat_yr = mean(arr_GAM_mean, na.rm = T),
         ea_lat_yr_ano = mean(AnomDArr, na.rm = T)) %>% 
  dplyr::select(species, year, ea_lat_yr, ea_lat_yr_ano) %>% 
  distinct()

ea_tab_ano <- final10 %>% 
  dplyr::select(species, year, cell_lat2, cell, AnomDArr) %>% 
  distinct() %>% 
  group_by(species, year) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat_ano = mean(AnomDArr, na.rm = T)) %>% 
  dplyr::select(species, year, ea_lat_ano) %>% 
  distinct()

final11 <- left_join(final10, ea_tab_l, by = "species")

final12 <- left_join(final11, ea_tab_l_yr, by = c("species", "year"))

final13 <- left_join(final12, ea_tab_ano, by = c("species", "year"))

dim(final13) ; dim(final13)[1] == dimfinal ;  tail(colnames(final13))

## remove rows with bird speed greater than spe_thres
for(i in 1:nrow(final13)){
  if(is.na(final13$vArrMag[i])) {final$vArrMag[i]
  } else {if(final13$vArrMag[i] > spe_thres) {final$vArrMag[i] <- NA}}
}

write_rds(final13, file = glue("data/final_st{spe_thres}.rds"))

print("      done!       ")
