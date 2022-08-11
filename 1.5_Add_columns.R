library(egg)
library(ggplot2)
library(tidyverse)
library(viridis)
library("glue")
library(lemon)
library(lme4)
library(mgcv)
#library(MASS)
#library(cAIC4)

colnmaes <- colnames
# IMPORT DATA ---------------------------
final <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/birdgreen.rds")
hist(final$vArrMag)
plot(final$vArrMag)

hist(final$vArrMag)
plot(final$vArrMag)
ggplot() +
  geom_boxplot(aes(y = final$vArrMag, x = final$species,
                   colour = final$species)) +
  theme(legend.position = "none")

final <- final %>% 
  filter(is.na(vArrMag) | vArrMag < 3000)
cells <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/cellcoor.rds")
velocityG <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/velocityG.rds") %>% 
  left_join(., cells, by = "cell")
dimfinal <- nrow(final)
# FORMAT DATA ---------------------------- 
### overwinter latitude (from how far south do we come!) ---------------------------
winlat <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/Table_S1.csv") %>%
  dplyr::select(Species, Overwinterlatitude) %>% 
  mutate(species2 = Species) %>% 
  rename(species = Species, 
         winlat = Overwinterlatitude) %>% 
  mutate(species = sub(" ", "_", species))

final <- left_join(final, winlat, by = "species")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### species sensitivity ---------------------------
sensi <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/data_sensi.RDS") %>% 
  dplyr::select(sci_name,
                cell,
                beta_mean) %>% 
  rename(species = sci_name,
         sensi = beta_mean) 

sensisps_c <- readRDS("data/source/spe_sensi.rds") %>% 
  rename(species = sci_name)
final <- final %>% 
  left_join(., sensisps_c, by = "species")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

cellnumbs <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/cellnumbs.rds")
sensi <- left_join(sensi, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2)

final <- left_join(final, sensi, by = c("species","cell")) 

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

final <- left_join(final,
                   cells %>% 
                     dplyr::select(cell, cell_lat) %>% 
                     rename(cell_lat2 = cell_lat),
                   by = "cell")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### early arrivers (mean arrival date for cells under x latitude, OR mean of the earliest x arrival dates) (paper: only early arrivers are arriving earlier!) ---------------------------
ea_tab_l <- final %>% 
  dplyr::select(species, cell_lat2, cell, arr_GAM_mean) %>% 
  distinct() %>% 
  group_by(species) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat = mean(arr_GAM_mean, na.rm = T)) %>% 
  dplyr::select(species, ea_lat) %>% 
  distinct()

ear_lat <- final %>% 
  dplyr::select(species, cell_lat2,cell_lat, cell, arr_GAM_mean, year) %>% 
  distinct() %>% 
  filter(cell_lat2 < 35)  
spseal_lm <- lmer(data = ear_lat, arr_GAM_mean ~ as.factor(species) -1 + (1|year) + (1|cell) + (1|cell_lat2))

spseal <- cbind(substring(names(getME(spseal_lm, name = "fixef")), 19),
                as.numeric(getME(spseal_lm, name = "fixef")))

colnames(spseal) <- c("species", "ea_lat_m")
spseal <- as.data.frame(spseal)
spseal$ea_lat_m <- as.numeric(spseal$ea_lat_m)

final <- final %>% 
  left_join(., spseal, by = "species") 

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

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

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### body mass, distance and taxonomy (random effect for family and species nested, NOP!) ---------------------------
mass_tax <- read_csv("data/source/gcb14540-sup-0001-supinfo_mass.csv") %>% 
  dplyr::select(species,Order,Family,Body_mass_g,Distance_m, Time) %>% 
  mutate(Distance_m = scale(as.numeric(Distance_m)))

unique(final[which(final$species %in% pull(mass_tax[which(is.na(mass_tax$Body_mass_g)),1])),3])

final <- left_join(final, mass_tax, by = "species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### diet group ---------------------------
diet <- read_csv("data/source/jane13345-sup-0002-tables1_diet.csv") %>% 
  dplyr::select(Species, Diet) %>% 
  rename(species = Species)
unique(final[which(final$species %in% pull(diet[which(is.na(diet$Diet)),1])),3])

final <- left_join(final, diet, by = "species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### taxonomic order
famcol <- read_csv("data/species_tax_ord.csv")
final <- left_join(final, famcol, by="species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

## annual means
bspe_yr_lm <- lmer(data = final %>% dplyr::select(vArrMag, cell, cell_lat, species, year) %>% 
                     filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                   log(vArrMag) ~ as.factor(species) * as.factor(year) -1 + (1|cell))

pred.dat <- expand.grid(species=sort(unique(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% 
                                              dplyr::select(species) %>% pull())),
                        year=seq(2003,2017,1))
pred.dat <- pred.dat %>% 
  mutate(pred = predict(bspe_yr_lm, newdata=pred.dat, re.form =~0)) %>% 
  left_join(., final %>% dplyr::select(species, species2) %>% distinct(), by = "species")

# bird speed anomaly
bspe_yr_lmA <- lmer(data = final %>% dplyr::select(AnomVArr, cell, cell_lat, species, year) %>% 
                      filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                    AnomVArr ~ as.factor(species) * as.factor(year) + (1|cell))

pred.datA1 <- expand.grid(species=sort(unique(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% 
                                                dplyr::select(species) %>% pull())),
                          year=seq(2003,2017,1))
pred.datA1 <- pred.datA1 %>%  mutate(pred = predict(bspe_yr_lmA, newdata=pred.datA1, re.form =~0)) %>% 
  left_join(., final %>% dplyr::select(species, species2) %>% distinct(), by = "species")

# green up speed anomaly
guspeA_lm <- lmer(data = final %>% dplyr::select(AnomVGr, cell, cell_lat, year) %>% distinct(),
                  AnomVGr ~ as.factor(year) - 1 + (1|cell))
yearsAlm <- cbind(as.numeric(substring(names(getME(guspeA_lm, name = "fixef")), 16)),
                  as.numeric(getME(guspeA_lm, name = "fixef")))
colnames(yearsAlm) <- c("year", "gu_speA_mea")
yearsAlm <- as.data.frame(yearsAlm)
rownames(yearsAlm) <- NULL

# green up date anomaly
gudatA_lm <- lmer(data = final %>% dplyr::select(AnomDGr, cell, cell_lat, year) %>% distinct(),
                  AnomDGr ~ as.factor(year) - 1 + (1|cell))
yeardAlm <- cbind(as.numeric(substring(names(getME(gudatA_lm, name = "fixef")), 16)),
                  as.numeric(getME(gudatA_lm, name = "fixef")))
colnames(yeardAlm) <- c("year", "gu_datA_mea")
yeardAlm <- as.data.frame(yeardAlm)
rownames(yeardAlm) <- NULL

annual <- left_join(pred.dat %>% rename(BS = pred), 
                    pred.datA1 %>% dplyr::select(species, pred, year) %>% rename(ABS = pred),
                    by = c("species","year")) %>% 
  #left_join(., yearslm, by = "year") %>% 
  #left_join(., yeardlm, by = "year") %>% 
  left_join(., yearsAlm, by = "year") %>% 
  left_join(., yeardAlm, by = "year") %>% 
  rename(#med_spe = meds,
    #         GS = gu_spe_mean,
    #         GD = gu_dat_mea,
    AGS = gu_speA_mea,
    AGD = gu_datA_mea)

saveRDS(final, file = "data/final.rds")
saveRDS(annual, file = "data/annual.rds")

