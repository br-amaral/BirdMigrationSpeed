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
final <- final %>% 
  filter(vArrMag <500)
cells <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/cellcoor.rds")
velocityG <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/velocityG.rds") %>% 
  left_join(., cells, by = "cell")
dimfinal <- 6499      #15875
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

### categories for overwinter latitude
hist(final$winlat)

#(q1wl <- quantile(final$winlat, c(0.33, 0.66), na.rm = T)[1])
#(q2wl <- quantile(final$winlat, c(0.33, 0.66), na.rm = T)[2])

# q1wl <- -5 ; q1wl <- 13

x <- seq(min(final %>% dplyr::select(winlat), na.rm = T), 
         max(final %>% dplyr::select(winlat), na.rm = T), 1)
q1wl <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2wl <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

abline(v = q1wl, col = "red")
abline(v = q2wl, col = "red")

final <- final %>% 
  mutate(winlatcat = ifelse(winlat < (q1wl), 'Long', 
                            ifelse(winlat >= (q1wl) & winlat < q2wl, "Medium", 
                                   ifelse(winlat >= q2wl, "Short", "opps"))))

final$winlatcat <- factor(final$winlatcat, ordered = TRUE,
                          levels = c("Short","Medium","Long"))

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

sensi_lm <-lmer(data = sensi, sensi ~ as.factor(species) - 1 + (1|cell))

sensi2 <- cbind(substring(names(getME(sensi_lm, name = "fixef")), 19),
                 as.numeric(getME(sensi_lm, name = "fixef")))

colnames(sensi2) <- c("species", "sensi_m")
sensi2 <- as.data.frame(sensi2)
sensi2$sensi_m <- as.numeric(sensi2$sensi_m)

final <- final %>% 
  left_join(., sensi2, by = "species")

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

### sensitivity categorical
hist(pull(final %>% dplyr::select(sensi_m)))

#(q1sen <- quantile(final %>% dplyr::select(sensi), c(0.33, 0.66), na.rm = T)[1])
#(q2sen <- quantile(final %>% dplyr::select(sensi), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final$sensi_m, na.rm = T), max(final$sensi_m, na.rm = T), 0.01)
q1sen <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2sen <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]
# q1sen <- -5 ; q1sen <- 2.5

abline(v = q1sen, col = "red")
abline(v = q2sen, col = "red")

final <- final %>% 
  mutate(sensicat = ifelse(sensi_m < q1sen, 'Low', 
                           ifelse(sensi_m >= q1sen & sensi_m < q2sen, "Medium", 
                                  ifelse(sensi_m >= q2sen, "High", "opps"))))

final$sensicat <- factor(final$sensicat, ordered = TRUE,
                         levels = c("Low","Medium","High"))

### green up years: fast-slow/ early-late---------------------------
gudat_lm <- lmer(data = final %>% dplyr::select(gr_mn, cell, cell_lat, year) %>% distinct(),
   gr_mn ~ as.factor(year) - 1 + (1|cell))

yeardlm <- cbind(as.numeric(substring(names(getME(gudat_lm, name = "fixef")), 16)),
                 as.numeric(getME(gudat_lm, name = "fixef")))
colnames(yeardlm) <- c("year", "gu_dat_mea")
yeardlm <- as.data.frame(yeardlm)
rownames(yeardlm) <- NULL

guspe_lm <- lmer(data = final %>% dplyr::select(vGrMag, cell, cell_lat, year) %>% distinct(),
               log(vGrMag) ~ as.factor(year) - 1 + (1|cell))
yearslm <- cbind(as.numeric(substring(names(getME(guspe_lm, name = "fixef")), 16)),
                 as.numeric(getME(guspe_lm, name = "fixef")))

colnames(yearslm) <- c("year", "gu_spe_mean")
yearslm <- as.data.frame(yearslm)
rownames(yearslm) <- NULL

final <- final %>% 
  left_join(., yearslm, by = "year") %>% 
  left_join(., yeardlm, by = "year")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

# same, but for green up anomalies
gudatA_lm <- lmer(data = final %>% dplyr::select(AnomDGr, cell, cell_lat, year) %>% distinct(),
                AnomDGr ~ as.factor(year) - 1 + (1|cell))
yeardAlm <- cbind(as.numeric(substring(names(getME(gudatA_lm, name = "fixef")), 16)),
                  as.numeric(getME(gudatA_lm, name = "fixef")))
colnames(yeardAlm) <- c("year", "gu_datA_mea")
yeardAlm <- as.data.frame(yeardAlm)
rownames(yeardAlm) <- NULL

#speed
guspeA_lm <- lmer(data = final %>% dplyr::select(AnomVGr, cell, cell_lat, year) %>% distinct(),
                AnomVGr ~ as.factor(year) - 1 + (1|cell))
yearsAlm <- cbind(as.numeric(substring(names(getME(guspeA_lm, name = "fixef")), 16)),
                  as.numeric(getME(guspeA_lm, name = "fixef")))
colnames(yearsAlm) <- c("year", "gu_speA_mea")
yearsAlm <- as.data.frame(yearsAlm)
rownames(yearsAlm) <- NULL

#final <- final %>% 
#  left_join(., yearsAlm, by = "year") %>% 
  #left_join(., yeardAlm, by = "year")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

## plots! 
### early and late years
final %>% 
  dplyr::select(year, gr_mn, gu_dat_mea) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year, y = gr_mn)) +
#  geom_point(aes(x = year, y = gu_dat_mea), col = "#00BFC4") +
  theme_bw()  +
  ggtitle("green up date annual")

final %>% 
  dplyr::select(year, AnomDGr, gu_dat_mea, ) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year, y = AnomDGr)) +
  #geom_point(aes(x = year, y = gu_datA_mea), col = "#00BFC4") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("green up date annual anomaly")

### slow and fast years
final %>% 
  dplyr::select(year, vGrMag, AnomVGr, gu_spe_mean) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
    geom_boxplot(aes(x = year, y = log(vGrMag))) +
    geom_point(aes(x = year, y = gu_spe_mean), col = "#FF68A1") +
    theme_bw() +
  ggtitle("Annual green up speed")

final %>% 
  dplyr::select(year, vGrMag, AnomVGr, gu_spe_mean) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year, y = AnomVGr)) +
  #geom_point(aes(x = year, y = gu_speA_mea), col = "#FF68A1") +
  theme_bw() +
  ggtitle("Annual green up speed anomaly")

final %>% 
  dplyr::select(year, vArrMag, AnomVArr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year, y = AnomVArr)) +
  #geom_point(aes(x = year, y = gu_speA_mea), col = "#FF68A1") +
  theme_bw() +
  ggtitle("Annual green up speed anomaly")

### are early years faster in general? NO
final %>% 
  dplyr::select(AnomDArr,AnomDGr,AnomVArr,AnomVGr,AnomLag,gu_spe_mean, 
                gu_dat_mea) %>% 
  ggplot() + # speed is green
  geom_point(aes(x = AnomDGr, y = AnomDArr), col = "#00BFC4", alpha = 0.2) +
  geom_smooth(aes(x = AnomDGr, y = AnomDArr))+
  theme_bw() +
  ggtitle("Date anomaly")

final %>% 
  dplyr::select(AnomDArr,AnomDGr,AnomVArr,AnomVGr,AnomLag,gu_spe_mean, 
                gu_dat_mea) %>% 
  ggplot() + # speed is green
  geom_point(aes(x = AnomVGr, y = AnomVArr), col = "#7CAE00", alpha = 0.2) +
  geom_smooth(aes(x = AnomVGr, y = AnomVArr)) +
  theme_bw() +
  ggtitle("Speed anomaly")
## how correlated are speed and dates?
final %>% 
  ggplot(aes(x = gr_mn, y = log(vGrMag))) + 
  geom_point(col = "#7CAE00", alpha = 0.2) +
  geom_smooth() +
  geom_smooth(method = lm, col = "black") +
  theme_bw()

final %>% 
  ggplot(aes(x = AnomDGr, y = AnomVGr)) + 
  geom_point(col = "#7CAE00", alpha = 0.2) +
  geom_smooth() +
  geom_smooth(method = lm, col = "black") +
  theme_bw()

final %>% 
  ggplot(aes(x = arr_GAM_mean, y = log(vArrMag))) + 
  geom_point(col = "#00BFC4", alpha = 0.2) +
  geom_smooth() +
  geom_smooth(method = lm, col = "black") +
  theme_bw()

final %>% 
  ggplot(aes(x = AnomDArr, y = AnomVArr)) + 
  geom_point(col = "#00BFC4", alpha = 0.2) +
  geom_smooth() +
  geom_smooth(method = lm, col = "black") +
  theme_bw()

### bird speed: fast/slow species (lm) - (within year var adjust? la sorte 2017))  ---------------------------
bspe_lm <- lmer(data = final %>% dplyr::select(vArrMag, cell, cell_lat, species, year) %>% 
                  filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                vArrMag ~ as.factor(species) - 1 + (1|year) + (1|cell))

spspelm <- cbind(substring(names(getME(bspe_lm, name = "fixef")), 19),
                 as.numeric(getME(bspe_lm, name = "fixef")))
  
colnames(spspelm) <- c("species", "b_spe_mean")
spspelm <- as.data.frame(spspelm)
spspelm$b_spe_mean <- as.numeric(spspelm$b_spe_mean)

final <- final %>% 
  left_join(., spspelm, by = "species") 

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

final %>% 
  ggplot() +
  geom_boxplot(aes(y = vArrMag, x = species)) +
  geom_point(aes(y = b_spe_mean, x = species), col = "red") +
  theme_bw()

## species mean annual speed
bspe_yr_lm <- lmer(data = final %>% dplyr::select(vArrMag, cell, cell_lat, species, year) %>% 
                  filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                log(vArrMag) ~ as.factor(species) * as.factor(year) -1 + (1|cell))

pred.dat <- expand.grid(species=sort(unique(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% 
                                              dplyr::select(species) %>% pull())),
                       year=seq(2003,2017,1))
pred.dat <- pred.dat %>% 
  mutate(pred = predict(bspe_yr_lm, newdata=pred.dat, re.form =~0)) %>% 
  left_join(., final %>% dplyr::select(species, species2) %>% distinct(), by = "species")


spspelm_yr <- cbind(str_extract(substring(names(getME(bspe_yr_lm, name = "fixef")), 19), "[^:]+"),
                    str_sub(names(getME(bspe_yr_lm, name = "fixef")), -4),
                    as.numeric(getME(bspe_yr_lm, name = "fixef")))

colnames(spspelm_yr) <- c("species", "b_spe_mean_yr")
spspelm_yr <- as.data.frame(spspelm_yr)
spspelm_yr$b_spe_mean_yr <- as.numeric(spspelm_yr$b_spe_mean_yr)

## anomaly speed means
bspe_yr_lmA <- lmer(data = final %>% dplyr::select(AnomVArr, cell, cell_lat, species, year) %>% 
                     filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                   AnomVArr ~ as.factor(species) * as.factor(year) -1 + (1|cell))

pred.datA1 <- expand.grid(species=sort(unique(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% 
                                              dplyr::select(species) %>% pull())),
                        year=seq(2003,2017,1))
pred.datA1 <- pred.datA1 %>%  mutate(pred = predict(bspe_yr_lmA, newdata=pred.datA1, re.form =~0)) %>% 
  left_join(., final %>% dplyr::select(species, species2) %>% distinct(), by = "species")

# all species together species anomaly
bdatA_lm <- lmer(data = final %>% dplyr::select(AnomVArr, cell, cell_lat, year) %>% distinct(),
                  AnomVArr ~ as.factor(year) - 1 + (1|cell))
byeardAlm <- cbind(as.numeric(substring(names(getME(bdatA_lm, name = "fixef")), 16)),
                  as.numeric(getME(bdatA_lm, name = "fixef")))
colnames(byeardAlm) <- c("year", "b_speA_mea")
byeardAlm <- as.data.frame(byeardAlm)
rownames(byeardAlm) <- NULL

## plots! 
### bird annual speed
final %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag))) +
  geom_point(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                 y = log(b_spe_mean)), col = "#00BFC4") +
  theme_bw() +
  ggtitle("Average bird species speed") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic"))

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
  dplyr::select(species, cell_lat2, cell, AnomDArr, year) %>% 
  distinct() %>% 
  group_by(species, year) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat_yr = mean(AnomDArr, na.rm = T)) %>% 
  dplyr::select(species, year, ea_lat_yr) %>% 
  distinct()

ea_tab_d <- final %>% 
  dplyr::select(species, cell_lat2, cell, arr_GAM_mean) %>% 
  distinct() %>% 
  group_by(species) %>% 
  slice_min(order_by = arr_GAM_mean, n = 20) %>% 
  mutate(ea_dat = mean(arr_GAM_mean, na.rm = T)) %>% 
  dplyr::select(species, ea_dat) %>% 
  distinct()

ea_tab_ano <- final %>% 
  dplyr::select(species, year, cell_lat2, cell, AnomDArr) %>% 
  distinct() %>% 
  group_by(species, year) %>% 
  filter(cell_lat2 < 35) %>% 
  mutate(ea_lat_ano = mean(AnomDArr, na.rm = T)) %>% 
  dplyr::select(species, year, ea_lat_ano) %>% 
  distinct()

ea_tab <- full_join(ea_tab_l, ea_tab_d, by = "species")

final <- left_join(final, ea_tab, by = "species")

final <- left_join(final, ea_tab_l_yr, by = c("species", "year"))

final <- left_join(final, ea_tab_ano, by = c("species", "year"))

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

## ea categorical:
hist(pull(final %>% dplyr::select(ea_dat)))
x <- seq(min(final$ea_dat, na.rm = T), max(final$ea_dat, na.rm = T), 0.01)
q1ead <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2ead <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]
# q1sen <- -5 ; q1sen <- 2.5

abline(v = q1ead, col = "red")
abline(v = q2ead, col = "red")

final <- final %>% 
  mutate(ead_cat = ifelse(ea_dat < q1ead, 'Early', 
                           ifelse(ea_dat >= q1ead & ea_dat < q2ead, "Medium", 
                                  ifelse(ea_dat >= q2ead, "Late", "opps"))))

final$ead_cat <- factor(final$ead_cat, ordered = TRUE,
                         levels = c("Early","Medium","Late"))

hist(pull(final %>% dplyr::select(ea_lat)))
x <- seq(min(final$ea_lat, na.rm = T), max(final$ea_lat, na.rm = T), 0.01)
q1eal <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2eal <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]
# q1sen <- -5 ; q1sen <- 2.5

abline(v = q1eal, col = "red")
abline(v = q2eal, col = "red")

final <- final %>% 
  mutate(eal_cat = ifelse(ea_dat < q1eal, 'Early', 
                          ifelse(ea_dat >= q1eal & ea_dat < q2eal, "Medium", 
                                 ifelse(ea_dat >= q2eal, "Late", "opps"))))

final$eal_cat <- factor(final$eal_cat, ordered = TRUE,
                        levels = c("Early","Medium","Late"))

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))


### breeding latitude (how far north we go! mean, med, max) ---------------------------
avgsumlat <- final %>%
  dplyr::select(species, breed_cell, cell_lat) %>% 
  filter(breed_cell == T) %>% 
  group_by(species) %>% 
  mutate(bre_lat_mea = mean(cell_lat, na.rm = T),
         bre_lat_med = median(cell_lat, na.rm = T),
         bre_lat_max = max(cell_lat, na.rm = T)) %>% 
  dplyr::select(-cell_lat) %>% 
  dplyr::select(-breed_cell) %>% 
  distinct()

plot(avgsumlat$bre_lat_mea, avgsumlat$bre_lat_med)
plot(avgsumlat$bre_lat_mea, avgsumlat$bre_lat_max)
avgsumlat <- avgsumlat %>% 
  dplyr::select(-bre_lat_med)

final <- final %>% 
  left_join(., avgsumlat, by = "species") 

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### average breeding latitude categorical 
hist(pull(final %>% filter(breed_cell == T) %>% dplyr::select(cell_lat)))

#(q1bl <- quantile(final %>% filter(breed_cell == T) %>% dplyr::select(cell_lat), c(0.33, 0.66), na.rm = T)[1])
#(q2bl <- quantile(final %>% filter(breed_cell == T) %>% dplyr::select(cell_lat), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final %>% filter(breed_cell == T) %>% dplyr::select(cell_lat), na.rm = T), 
         max(final %>% filter(breed_cell == T) %>% dplyr::select(cell_lat), na.rm = T), 1)
q1bl <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2bl <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

abline(v = q1bl, col = "red")
abline(v = q2bl, col = "red")

final <- final %>% 
  mutate(brelatcat = ifelse(cell_lat < q1bl & breed_cell == T, 'Low', 
                            ifelse(cell_lat >= q1bl & cell_lat < q2bl  & breed_cell == T, "Medium", 
                                   ifelse(cell_lat >= q2bl & breed_cell == T, "High", "opps"))))

final$brelatcat <- factor(final$brelatcat, ordered = TRUE,
                          levels = c("Low","Medium","High"))

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### average lag per species ---------------------------
blag_lm <- lmer(data = final %>% dplyr::select(lag, cell, cell_lat, species, year) %>% 
                  filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                lag ~ as.factor(species) - 1 + (1|year) + (1|cell))

splaglm <- cbind(substring(names(getME(blag_lm, name = "fixef")), 19),
                 as.numeric(getME(blag_lm, name = "fixef")))
  
colnames(splaglm) <- c("species", "b_lag_mean")
splaglm <- as.data.frame(splaglm)

final <- final %>% 
  left_join(., splaglm, by = "species") 

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

final$b_lag_mean <- as.numeric(final$b_lag_mean)

## lat categorical
hist(final$cell_lat)

#(q1bs <- quantile(log(final$vArrMag), c(0.33, 0.66), na.rm = T)[1])
#(q2bs <- quantile(log(final$vArrMag), c(0.33, 0.66), na.rm = T)[2])

x <- seq(min(final$cell_lat, na.rm = T), 
         max(final$cell_lat, na.rm = T), 0.1)
q1lat <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2lat <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

abline(v = q1lat, col = "red")
abline(v = q2lat, col = "red")

final <- final %>% 
  mutate(latcat = ifelse(cell_lat < q1lat, 'Low', 
                         ifelse(cell_lat >= q1lat & cell_lat < q2lat, "Medium", 
                                ifelse(cell_lat >= q2lat, "High", "opps"))))

final$latcat <- factor(final$latcat, ordered = TRUE,
                       levels = c("Low","Medium","High"))
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))


### speed in migration route ---------------------------
b_spe_mig <- final %>%
  dplyr::select(species, mig_cell, vArrMag, breed_cell, year, cell) %>% 
  filter(mig_cell == T,
         breed_cell == F)
bspemig_lm <- lmer(data = b_spe_mig, 
                   vArrMag ~ as.factor(species) - 1 + (1|year) + (1|cell))

bspemig <- cbind(substring(names(getME(bspemig_lm, name = "fixef")), 19),
                 as.numeric(getME(bspemig_lm, name = "fixef")))

colnames(bspemig) <- c("species", "b_spe_mig_l")
bspemig <- as.data.frame(bspemig)

final <- final %>% 
  left_join(., bspemig, by = "species")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### speed in breeding route ---------------------------
b_spe_bre <- final %>%
  dplyr::select(species, vArrMag, breed_cell, year, cell) %>% 
  filter(breed_cell == F)

bspebre_lm <- lmer(data = b_spe_bre, 
                   vArrMag ~ as.factor(species) - 1 + (1|year) + (1|cell))

bspebre <- cbind(substring(names(getME(bspebre_lm, name = "fixef")), 19),
                 as.numeric(getME(bspebre_lm, name = "fixef")))

colnames(bspebre) <- c("species", "b_spe_bre_l")
bspebre <- as.data.frame(bspebre)

final <- final %>% 
  left_join(., bspebre, by = "species")

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### body mass, distance and taxonomy (random effect for family and species nested, NOP!) ---------------------------
mass_tax <- read_csv("data/source/gcb14540-sup-0001-supinfo_mass.csv") %>% 
  dplyr::select(species,Order,Family,Body_mass_g,Distance_m, Time) %>% 
  mutate(Distance_m = scale(as.numeric(Distance_m)))

unique(final[which(final$species %in% pull(mass_tax[which(is.na(mass_tax$Body_mass_g)),1])),3])

final <- left_join(final, mass_tax, by = "species")
dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### body mass categorical
hist(pull(final %>% dplyr::select(Body_mass_g))) 

#(q1bm <- quantile(final %>% dplyr::select(Body_mass_g), c(0.33, 0.66), na.rm = T)[1])
#(q2bm <- quantile(final %>% dplyr::select(Body_mass_g), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final$Body_mass_g, na.rm = T), max(final$Body_mass_g, na.rm = T), 1)
# q1bm <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
# q2bm <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

q1bm <- 15
q2bm <- 35

abline(v = q1bm, col = "red")
abline(v = q2bm, col = "red")

final <- final %>% 
  mutate(bodymass_cat = ifelse(Body_mass_g < q1bm, 'Light', 
                           ifelse(Body_mass_g >= q1bm & Body_mass_g < q2bm, "Medium", 
                                  ifelse(Body_mass_g >= q2bm, "Heavy", "opps"))))

final$bodymass_cat <- factor(final$bodymass_cat, ordered = TRUE,
                         levels = c("Light","Medium","Heavy"))

dim(final) ; dim(final)[1] == dimfinal ;  tail(colnmaes(final))

### distance categorical
hist(pull(final %>% dplyr::select(Distance_m))) 

#(q1dm <- quantile(final %>% dplyr::select(Distance_m), c(0.33, 0.66), na.rm = T)[1])
#(q2dm <- quantile(final %>% dplyr::select(Distance_m), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final$Distance_m, na.rm = T), max(final$Distance_m, na.rm = T), 1)
q1dm <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2dm <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

abline(v = q1dm, col = "red")
abline(v = q2dm, col = "red")

final <- final %>% 
  mutate(Distance_cat = ifelse(Distance_m < q1dm, 'Light', 
                               ifelse(Distance_m >= q1dm & Distance_m < q2dm, "Medium", 
                                      ifelse(Distance_m >= q2dm, "Heavy", "opps"))))

final$Distance_cat <- factor(final$Distance_cat, ordered = TRUE,
                             levels = c("Light","Medium","Heavy"))

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


##------------------------------------------------------
### get data for:
### diurnal vs nocturnal migrants (Newson 2016 in Schmaljohann) -----------------------------
### specialist vs. generalist -----------------------------
#(usui: Running counter to this prediction, however, previous comparative studies have found 
#   that generalist species are more responsive than specialists to climate change (Végvári et al. 2010;
#   Moussus et al. 2011; Hurlbert & Liang 2012).)
#------------------------------------------------------
# MODELS AND PLOTS -----------------------------
## SPEED as response ----------------------------
### birds traits - what makes a species fast or slow? ---------------------------
# how far north they go to breed, how far south they come from, how sensitive they are,
#   early vs late arriver, body size, diet group, distance_m (?), taxonomy (not enough orders)
mod_a1 <- lm(data = final, log(vArrMag) ~ winlat + sensi + sensi_m + 
               ea_lat +# ea_dat + bre_lat_mea + bre_lat_max + 
               Diet + Body_mass_g + Distance_m + cell_lat) 

mod_a2 <- lmer(data = final, log(vArrMag) ~ winlat + sensi + sensi_m + 
                 ea_lat +# ea_dat + bre_lat_mea + bre_lat_max +
                 Diet + Body_mass_g + Distance_m  + cell_lat +
                (1|species) + (1|year) + (1|cell)) 

head(aic_a <- AIC(mod_a1, mod_a2
                  ) %>% arrange(AIC)) 

sjPlot::tab_model(get(rownames(aic_a)[1]), show.re.var= TRUE, digits = 3)

mod_ag <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               log(vArrMag) ~ winlat + #sensi + 
                sensi_m + 
                ea_lat + #ea_dat + 
                #bre_lat_mea + bre_lat_max +
                Diet + Body_mass_g + Distance_m  + s(cell_lat, bs = "tp") +
                 s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 
summary(mod_ag)

# Stepwise regression model
step.model_a2 <- stepcAIC(mod_ag, direction = "both", 
                      trace = FALSE, data = final)
summary(step.model_a2)

step.model_a1 <- stepAIC(mod_a1, direction = "both", 
                         trace = FALSE, data = final)
summary(step.model_a1)

step.model_a3 <- stepAIC(mod_a3, direction = "both", 
                         trace = FALSE, data = final)
summary(step.model_a3)

## body mass is off, BUT sensitivity is ON!!!!!!!

#### - winlat ---------------------------
pw1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = winlat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Wintering\nLatitude\n(degrees)\n") 

meansw <- final %>% 
  group_by(winlatcat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pw2 <- ggplot(data = final) +
  geom_boxplot(aes(x = winlatcat, 
                   y = log(vArrMag), fill = winlatcat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Migration distance", y="Log(Bird speed)") +
  geom_point(data = meansw, aes(x = winlatcat, y = meanspe))

pw3 <- ggplot(data = final ) +
  geom_point(aes(x = winlat, y = log(vArrMag), col = winlatcat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
  ) +
  labs(y="Log(Bird speed)", x = "Wintering Latitude") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = winlat, y = log(vArrMag)), col = "black")

egg::ggarrange(pw3, pw2, pw1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

#### - sensi ---------------------------
ps1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = sensi_m),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Species\nsensitivity") 

meanss <- final %>% 
  group_by(sensicat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

ps2 <- ggplot(data = final) +
  geom_boxplot(aes(x = sensicat, 
                   y = log(vArrMag), fill = sensicat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="S e n s i t i v i t y") +
  geom_point(data = meanss, aes(x = sensicat, y = meanspe))

ps3 <- ggplot(data = final ) +
  geom_point(aes(x = sensi, y = log(vArrMag), col = sensicat),
            alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        ) +
  labs(y="Log(Bird speed)", x = "Sensitivity") +
  scale_fill_viridis(discrete = TRUE, name = "Species\nsensitivity") +
  geom_smooth(aes(x = sensi, y = log(vArrMag)), col = "black")
 
egg::ggarrange(ps3, ps2, ps1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))


#### - breeding range lat ---------------------------
pblm1 <- ggplot(data = final %>% filter(!is.na(brelatcat))) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = bre_lat_mea),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Breeding\nlatitude") 

meansblm <- final %>% 
  filter(!is.na(brelatcat)) %>% 
  group_by(brelatcat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pblm2 <- ggplot(data = final %>% filter(!is.na(brelatcat))) +
  geom_boxplot(aes(x = brelatcat, 
                   y = log(vArrMag), fill = brelatcat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansblm, aes(x = brelatcat, y = meanspe))

pblm3 <- ggplot(data = final %>% filter(!is.na(brelatcat))) +
  geom_point(aes(x = bre_lat_mea, y = log(vArrMag), col = brelatcat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
  ) +
  labs(y="Log(Bird speed)", x = "Breeding latitude") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = bre_lat_mea, y = log(vArrMag)), col = "black") +
  geom_smooth(aes(x = bre_lat_mea, y = log(vArrMag)), col = "gray", method = "lm")

egg::ggarrange(pblm3, pblm2, pblm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

mod_blm1 <- lmer(data = final, log(vArrMag) ~ bre_lat_mea + bre_lat_max + cell_lat +
                 (1|species) + (1|year) + (1|cell)) 

mod_blm1g <- mgcv::gam(data = final %>% 
                   mutate(species = as.factor(species),
                          cell = as.factor(cell)),
                 log(vArrMag) ~ bre_lat_mea + bre_lat_max +
                   s(species, bs = 're') + s(year, bs = 're') + s(cell_lat, bs = 'tp') + s(cell, bs = 're')) 

sjPlot::tab_model(mod_blm1,show.re.var= TRUE, digits = 3)
summary(mod_blm1g)

#### - early arrivers ---------------------------

pdeam1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = ea_dat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Early arrival\ndate mean") 

meansdeam <- final %>% 
  filter(!is.na(ea_dat)) %>% 
  group_by(ea_dat_cat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pdeam2 <- ggplot(data = final %>% filter(!is.na(ea_dat_cat))) +
  geom_boxplot(aes(x = ea_dat_cat, 
                   y = log(vArrMag), fill = ea_dat_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansdeam, aes(x = ea_dat_cat, y = meanspe))

pdeam3 <- ggplot(data = final %>% filter(!is.na(ea_dat_cat))) +
  geom_point(aes(x = ea_dat, y = log(vArrMag), col = ea_dat_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Early arrival date") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = ea_dat, y = log(vArrMag)), col = "black")

egg::ggarrange(pdeam3, pdeam2, pdeam1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

pdeal1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = ea_lat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Early arrival\ndate mean") 

meansdeal <- final %>% 
  filter(!is.na(ea_lat)) %>% 
  group_by(ea_lat_cat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pdeal2 <- ggplot(data = final %>% filter(!is.na(ea_lat_cat))) +
  geom_boxplot(aes(x = ea_lat_cat, 
                   y = log(vArrMag), fill = ea_lat_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansdeal, aes(x = ea_lat_cat, y = meanspe))

pdeal3 <- ggplot(data = final %>% filter(!is.na(ea_lat_cat))) +
  geom_point(aes(x = ea_lat, y = log(vArrMag), col = ea_lat_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Early arrival date") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = ea_lat, y = log(vArrMag)), col = "black")

egg::ggarrange(pdeal3, pdeal2, pdeal1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

mod_deam1 <- lmer(data = final, log(vArrMag) ~ ea_dat + ea_lat + cell_lat +
                   (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)) 

mod_deam1g <- gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ ea_dat + ea_lat +
                    s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

sjPlot::tab_model(mod_deam1,show.re.var= TRUE, digits = 3)
summary(mod_deam1g)

#### - Body_mass_g ---------------------------
pbm1 <- ggplot(data = final) +
  geom_boxplot(aes(x = fct_reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = Body_mass_g, colour=final$FamilyColor),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.ticks.x = element_line(colour=final$FamilyColor),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic"),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(title="Species") +
  scale_fill_viridis(name = "Body mass") 

meansbm <- final %>% 
  filter(!is.na(bodymass_cat)) %>% 
  group_by(bodymass_cat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pbm2 <- ggplot(data = final %>% filter(!is.na(bodymass_cat))) +
  geom_boxplot(aes(x = bodymass_cat, 
                   y = log(vArrMag), fill = bodymass_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansbm, aes(x = bodymass_cat, y = meanspe))

pbm3 <- ggplot(data = final %>% filter(!is.na(bodymass_cat))) +
  geom_point(aes(x = Body_mass_g, y = log(vArrMag), col = bodymass_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Body mass (g)") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = Body_mass_g, y = log(vArrMag)), col = "black")

egg::ggarrange(pbm3, pbm2, pbm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

mod_bm1 <- lmer(data = final, log(vArrMag) ~ Body_mass_g + cell_lat +
                    (1|Family:species) + (1|year) + (1|cell)) 
mod_bm1g <- gam(data = final %>% 
                   mutate(species = as.factor(species),
                          cell = as.factor(cell)), 
                 log(vArrMag) ~ Body_mass_g + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

sjPlot::tab_model(mod_bm1, show.re.var= TRUE, digits = 3)
summary(mod_bm1g)

#### - Distance_m ---------------------------
pdm1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = Distance_m),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Distance") 

meansdm <- final %>% 
  filter(!is.na(Distance_cat)) %>% 
  group_by(Distance_cat) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pdm2 <- ggplot(data = final %>% filter(!is.na(Distance_cat))) +
  geom_boxplot(aes(x = Distance_cat, 
                   y = log(vArrMag), fill = Distance_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Distance") +
  geom_point(data = meansdm, aes(x = Distance_cat, y = meanspe))

pdm3 <- ggplot(data = final) +
  geom_point(aes(x = Distance_m, y = log(vArrMag), col = Distance_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Distance") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = Distance_m, y = log(vArrMag)), col = "black")

egg::ggarrange(pdm3, pdm2, pdm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

mod_dm1 <- lmer(data = final, log(vArrMag) ~ Distance_m + cell_lat + 
                  (1|Family:species) + (1|year) + (1|cell)) 
mod_dm1g <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                log(vArrMag) ~ Distance_m + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

sjPlot::tab_model(mod_dm1,show.re.var= TRUE, digits = 3)
summary(mod_dm1g)

#### - Diet group ----------------------------------
pdt1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = Diet),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(discrete = TRUE, name = "Diet") 

meansdt <- final %>% 
  filter(!is.na(Diet)) %>% 
  group_by(Diet) %>% 
  summarise(meanspe = mean(log(vArrMag), na.rm = T))

pdt2 <- ggplot(data = final %>% filter(!is.na(Diet))) +
  geom_boxplot(aes(x = Diet, 
                   y = log(vArrMag), fill = Diet),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Distance") +
  geom_point(data = meansdt, aes(x = Diet, y = meanspe))

egg::ggarrange(pdt2, pdt1, ncol = 2, 
               top = "Migration speed", widths = c(2, 12))

mod_dm1 <- lmer(data = final, log(vArrMag) ~ Diet + cell_lat + 
                  (1|Family:species) + (1|year) + (1|cell)) 

sjPlot::tab_model(mod_dm1, show.re.var= TRUE, digits = 3)
sort(unique(final$Diet))

### Species mean speed (not all speeds for all cells and years, result of lm model) ----------------------
#### - winlat ----------------------
final %>% 
  ggplot() +
  geom_point(aes(x = winlat, y = log(b_spe_mean), col = winlatcat)) +
  geom_smooth(aes(x = winlat, y = log(b_spe_mean))) +
  geom_smooth(aes(x = winlat, y = log(b_spe_mean)), method = "lm", col = "red") +
  theme_bw() +
  labs(y = "Species mean speed", x = "Wintering latitiude")

#### - sensi ----------------------
final %>% 
  ggplot() +
  #geom_point(aes(y = log(b_spe_mean), x = sensi, col = sensicat)) +
  geom_point(aes(y = log(b_spe_mean), x = sensi_m, col = "black"), shape = 8) +
  #geom_smooth(aes(y =log( b_spe_mean), x = sensi)) +
  geom_smooth(aes(y = log(b_spe_mean), x = sensi_m), method = "lm", col = "red") +
  theme_bw() +
  labs(y = "Species mean speed", x = "sensitivity")
#### - ea_lat ----------------------
final %>% 
  ggplot() +
  #geom_point(aes(y = log(b_spe_mean), x = sensi, col = sensicat)) +
  geom_point(aes(y = log(b_spe_mean), x = eal_cat, col = "black"), shape = 8) +
  #geom_smooth(aes(y =log( b_spe_mean), x = sensi)) +
  geom_smooth(aes(y = log(b_spe_mean), x = eal_cat), method = "lm", col = "red") +
  theme_bw() +
  labs(y = "Species mean speed", x = "Early arrival date")
#### - breeding range lat ----------------------
final %>% 
  ggplot() +
  geom_point(aes(y = log(b_spe_mean), x = bre_lat_mea, col = brelatcat)) +
  geom_smooth(aes(y =log( b_spe_mean), x = bre_lat_mea)) +
  geom_smooth(aes(y = log(b_spe_mean), x = bre_lat_mea), method = "lm", col = "red") +
  theme_bw() +
  labs(y = "Species mean speed", x = "mean breeding latitude")

#### - Body_mass_g ----------------------
final %>% 
  ggplot() +
  geom_point(aes(y = log(b_spe_mean), x = Body_mass_g, col = bodymass_cat)) +
  geom_smooth(aes(y =log( b_spe_mean), x = Body_mass_g)) +
  geom_smooth(aes(y = log(b_spe_mean), x = Body_mass_g), method = "lm", col = "red") +
  theme_bw() +
  labs(y = "Species mean speed", x = "Body mass")

#### - Distance_m ----------------------
final %>% 
  ggplot() +
  geom_point(aes(y = log(b_spe_mean), x = Distance_m, col = Distance_cat)) +
  geom_smooth(aes(y =log( b_spe_mean), x = Distance_m)) +
  geom_smooth(aes(y = log(b_spe_mean), x = Distance_m), method = "lm", col = "red") +
  theme_bw() +
  labs(y = "Species mean speed", x = "Migratory distance")

#### - Diet group ----------------------
final %>% 
  ggplot() +
  geom_point(aes(y = log(vArrMag), x = Diet)) +
  geom_boxplot(aes(y = log(b_spe_mean), x = Diet)) +
   theme_bw() +
  labs(y = "Species mean speed", x = "Diet")

### Green-up - are birds tracking green up? --------------------------------


mod_b1g <- mgcv::gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                log(vArrMag) ~ 
                 AnomDGr + AnomVGr +
                 s(species, bs = "re") + s(year, bs = "re") + 
                 s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
summary(mod_b1g)
mod_b2g <- mgcv::gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               log(vArrMag) ~ #gr_mn + log(vGrMag) + 
                 #AnomDGr + AnomVGr +
                 gu_spe_mean + gu_dat_mea + #gu_speA_mea + gu_datA_mea +
                 s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
summary(mod_b2g)
AIC(mod_b1g, mod_b2g) %>% arrange(AIC)

# this does not make sense. annual
mod_b1gm <- mgcv::gam(data = final,
                     log(b_spe_mean) ~ 
                       gu_spe_mean + gu_dat_mea +
                       s(year, bs = "re")) 
summary(mod_b1gm)


head(aic_b <- AIC(mod_b1, mod_b2#, mod_b3
                  ) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_b)[1]), show.re.var= TRUE, digits = 3)

# Stepwise regression model
step.model_b1 <- stepcAIC(mod_b1, direction = "both", 
                          #groupCandidates = c("cell", "Family/species"),
                          #slopeCandidates = c("year", "cell_lat"),
                          trace = FALSE, data = final)
summary(step.model_b1)

step.model_b2 <- stepAIC(mod_b2, direction = "both", 
                         trace = FALSE, data = final)
summary(step.model_b2)

#### gr_mn - positive effect: the later green up is in the year, the faster birds move ----------------
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = gr_mn, y = log(vArrMag), col = species), alpha = 0.4) +
  geom_smooth(aes(x = gr_mn, y = log(vArrMag)), col = "black") +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")
  
#### AnomDGr - negative effect: the earlier the green up is, the faster the birds are  ----------------
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr, y = log(vArrMag), col = species), alpha = 0.4) +
  geom_smooth(aes(x = AnomDGr, y = log(vArrMag)),# col = species), se = F
              col = "black"
              ) +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")

### Lag - is being behind or ahead makes bird change speed?  ------------------------------
mod_c1g <- mgcv::gam(data = final%>% 
                      mutate(species = as.factor(species),
                             cell = as.factor(cell)), 
                    log(vArrMag) ~ AnomLag + I(AnomLag^2) +
                      s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re"))
summary(mod_c1g)

mod_c2g <- lmer(data = final%>% 
                      mutate(species = as.factor(species)), 
                    log(b_spe_mean) ~ b_lag_mean + I(b_lag_mean^2) +
                      (1|species))

mod_c3g <- mgcv::gam(data = final%>% 
                       mutate(species = as.factor(species)), 
                     log(b_spe_mean) ~ s(b_lag_mean, bs = "tp") + 
                       s(species, bs = "re"))
summary(mod_c3g)
plot(mod_c3g)

y <- log(final$b_spe_mean)
x <- final$b_lag_mean

pred <- predict(mod_c2g)
ix <- sort(x, index.return=T)$ix

#add polynomial curve to plot
plot(x, y, pch=16, cex=1) 
lines(x[ix], pred[ix], col='red', lwd=2)

### Breeding vs migration route - how is speed changing?  -------------------
mod_ran1 <- lmer(data = final,
                 log(vArrMag) ~ mig_cell + cell_lat +
                   (1|species) + (1|year) + (1|cell))

mod_ran2 <- lm(data = final,
                 log(vArrMag) ~ mig_cell + cell_lat)

mod_ran1g <- mgcv::gam(data = final %>% 
                   mutate(cell_type = as.factor(mig_cell),
                          species = as.factor(species),
                          cell = as.factor(cell)),
                 log(vArrMag) ~ mig_cell +
                   s(species, bs = 're') + s(year, bs = 're') + 
                   s(cell_lat, bs = 'tp') + s(cell, bs = 're')) 

head(aic_ran <- AIC(mod_ran1, mod_ran2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_ran)[1]), show.re.var= TRUE, digits = 3)
summary(mod_ran1g)

### Latitude ---------------------------------------------------
#### speed across latitude  ---------------------------------------------------
mod_ls1 <- lmer(data = final, log(vArrMag) ~ cell_lat + I(cell_lat^2) +
                 (1|species) + (1|year) + (1|cell))

mod_ls2 <- lm(data = final, log(vArrMag) ~ cell_lat + I(cell_lat^2))

mod_lsg <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               log(vArrMag) ~ s(cell_lat, bs = "tp") +
                 s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 


head(aic_ls <- AIC(mod_ls1, mod_ls2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_ls)[1]), show.re.var= TRUE, digits = 3)
summary(mod_lsg)

#### Lag across latitude  ---------------------------------------------------
mod_ll1 <- lmer(data = final, lag ~ cell_lat + I(cell_lat^2) +
                 (1|species) + (1|year) + (1|cell))

mod_ll2 <- lm(data = final, lag ~ cell_lat + I(cell_lat^2))

mod_llg <- mgcv::gam(data = final %>% 
                mutate(species = as.factor(species),
                       cell = as.factor(cell)), 
              lag ~ s(cell_lat, bs = "tp") +
                s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 
summary(mod_llg)




head(aic_ll <- AIC(mod_ll1, mod_ll2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_ll)[1]),show.re.var= TRUE, digits = 3)
summary(mod_llg)

mod_lal1 <- lmer(data = final, AnomLag ~ cell_lat + I(cell_lat^2) +
                  (1|species) + (1|year) + (1|cell))

mod_lal2 <- lm(data = final, AnomLag ~ cell_lat + I(cell_lat^2))

mod_lalg <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
                AnomLag ~ s(cell_lat, bs = "tp") +
                 s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 

head(aic_lal <- AIC(mod_lal1, mod_lal2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_lal)[1]),show.re.var= TRUE, digits = 3)
summary(mod_lalg)

#### are late birds faster?
final %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr, y = AnomVArr, col = species), alpha = 0.5) +
  geom_smooth(aes(x = AnomDGr, y = AnomVArr), col = "black") +
  theme_bw() +
  ggtitle("Arrival dates vs speed") +
  theme(legend.position = "none")

final %>% 
  ggplot() +
  geom_point(aes(x = gr_mn, y = vArrMag, col = species), alpha = 0.5) +
  geom_smooth(aes(x = gr_mn, y = vArrMag), col = "black") +
  theme_bw() +
  ggtitle("Arrival dates vs speed") +
  theme(legend.position = "none")

#### - faster are more flexible --------------------------------------

# Same, but with ANOMALY SPEED
### birds traits - what makes a species fast or slow? ---------------------------
# how far north they go to breed, how far south they come from, how sensitive they are,
#   early vs late arriver, body size, diet group, distance_m (?), taxonomy (not enough orders)
modA_a1 <- lm(data = final, AnomVArr ~ winlat + sensi + sensi_m + 
                ea_lat + ea_dat + bre_lat_mea + bre_lat_max + 
                Diet + Body_mass_g + Distance_m + cell_lat) 

modA_a2 <- lmer(data = final, AnomVArr ~ winlat + sensi + sensi_m + 
                  ea_lat + ea_dat + bre_lat_mea + bre_lat_max +
                  Diet + Body_mass_g + Distance_m  + cell_lat +
                  (1|species) + (1|year) + (1|cell)) 

head(aic_a <- AIC(modA_a1, modA_a2
) %>% arrange(AIC)) 

sjPlot::tab_model(get(rownames(aic_a)[1]), show.re.var= TRUE, digits = 3)

modA_ag <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               AnomVArr ~ winlat + sensi + sensi_m + 
                 ea_lat + ea_dat + bre_lat_mea + bre_lat_max +
                 Diet + Body_mass_g + Distance_m  + s(cell_lat, bs = "tp") +
                 s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 
summary(modA_ag)

#### - winlat ---------------------------
pw1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = winlat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Wintering\nLatitude\n(degrees)\n") 

meansw <- final %>% 
  group_by(winlatcat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pw2 <- ggplot(data = final) +
  geom_boxplot(aes(x = winlatcat, 
                   y = AnomVArr, fill = winlatcat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Migration distance", y="Log(Bird speed)") +
  geom_point(data = meansw, aes(x = winlatcat, y = meanspe))

pw3 <- ggplot(data = final ) +
  geom_point(aes(x = winlat, y = AnomVArr, col = winlatcat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
  ) +
  labs(y="Log(Bird speed)", x = "Wintering Latitude") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = winlat, y = AnomVArr), col = "black")

egg::ggarrange(pw3, pw2, pw1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

#### - sensi ---------------------------
ps1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = sensi_m),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Species\nsensitivity") 

meanss <- final %>% 
  group_by(sensicat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

ps2 <- ggplot(data = final) +
  geom_boxplot(aes(x = sensicat, 
                   y = AnomVArr, fill = sensicat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="S e n s i t i v i t y") +
  geom_point(data = meanss, aes(x = sensicat, y = meanspe))

ps3 <- ggplot(data = final ) +
  geom_point(aes(x = sensi, y = AnomVArr, col = sensicat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
  ) +
  labs(y="Log(Bird speed)", x = "Sensitivity") +
  scale_fill_viridis(discrete = TRUE, name = "Species\nsensitivity") +
  geom_smooth(aes(x = sensi, y = AnomVArr), col = "black")

egg::ggarrange(ps3, ps2, ps1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))


#### - breeding range lat ---------------------------
pblm1 <- ggplot(data = final %>% filter(!is.na(brelatcat))) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = bre_lat_mea),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Breeding\nlatitude") 

meansblm <- final %>% 
  filter(!is.na(brelatcat)) %>% 
  group_by(brelatcat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pblm2 <- ggplot(data = final %>% filter(!is.na(brelatcat))) +
  geom_boxplot(aes(x = brelatcat, 
                   y = AnomVArr, fill = brelatcat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansblm, aes(x = brelatcat, y = meanspe))

pblm3 <- ggplot(data = final %>% filter(!is.na(brelatcat))) +
  geom_point(aes(x = bre_lat_mea, y = AnomVArr, col = brelatcat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
  ) +
  labs(y="Log(Bird speed)", x = "Breeding latitude") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = bre_lat_mea, y = AnomVArr), col = "black") +
  geom_smooth(aes(x = bre_lat_mea, y = AnomVArr), col = "gray", method = "lm")

egg::ggarrange(pblm3, pblm2, pblm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

modA_blm1 <- lmer(data = final, AnomVArr ~ bre_lat_mea + bre_lat_max + cell_lat +
                    (1|species) + (1|year) + (1|cell)) 

modA_blm1g <- gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)),
                  AnomVArr ~ bre_lat_mea + bre_lat_max +
                    s(species, bs = 're') + s(year, bs = 're') + s(cell_lat, bs = 'tp') + s(cell, bs = 're')) 

sjPlot::tab_model(modA_blm1,show.re.var= TRUE, digits = 3)
summary(modA_blm1g)

#### - early arrivers ---------------------------

pdeam1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = ea_dat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Early arrival\ndate mean") 

meansdeam <- final %>% 
  filter(!is.na(ea_dat)) %>% 
  group_by(ea_dat_cat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pdeam2 <- ggplot(data = final %>% filter(!is.na(ea_dat_cat))) +
  geom_boxplot(aes(x = ea_dat_cat, 
                   y = AnomVArr, fill = ea_dat_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansdeam, aes(x = ea_dat_cat, y = meanspe))

pdeam3 <- ggplot(data = final %>% filter(!is.na(ea_dat_cat))) +
  geom_point(aes(x = ea_dat, y = AnomVArr, col = ea_dat_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Early arrival date") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = ea_dat, y = AnomVArr), col = "black")

egg::ggarrange(pdeam3, pdeam2, pdeam1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

pdeal1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = ea_lat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Early arrival\ndate mean") 

meansdeal <- final %>% 
  filter(!is.na(ea_lat)) %>% 
  group_by(ea_lat_cat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pdeal2 <- ggplot(data = final %>% filter(!is.na(ea_lat_cat))) +
  geom_boxplot(aes(x = ea_lat_cat, 
                   y = AnomVArr, fill = ea_lat_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansdeal, aes(x = ea_lat_cat, y = meanspe))

pdeal3 <- ggplot(data = final %>% filter(!is.na(ea_lat_cat))) +
  geom_point(aes(x = ea_lat, y = AnomVArr, col = ea_lat_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Early arrival date") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = ea_lat, y = AnomVArr), col = "black")

egg::ggarrange(pdeal3, pdeal2, pdeal1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

modA_deam1 <- lmer(data = final, AnomVArr ~ ea_dat + ea_lat + cell_lat +
                     (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)) 

modA_deam1g <- gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomVArr ~ ea_dat + ea_lat +
                     s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

sjPlot::tab_model(modA_deam1,show.re.var= TRUE, digits = 3)
summary(modA_deam1g)

#### - Body_mass_g ---------------------------
pbm1 <- ggplot(data = final) +
  geom_boxplot(aes(x = fct_reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = Body_mass_g, colour=final$FamilyColor),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.ticks.x = element_line(colour=final$FamilyColor),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic"),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(title="Species") +
  scale_fill_viridis(name = "Body mass") 

meansbm <- final %>% 
  filter(!is.na(bodymass_cat)) %>% 
  group_by(bodymass_cat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pbm2 <- ggplot(data = final %>% filter(!is.na(bodymass_cat))) +
  geom_boxplot(aes(x = bodymass_cat, 
                   y = AnomVArr, fill = bodymass_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Breeding Latitude") +
  geom_point(data = meansbm, aes(x = bodymass_cat, y = meanspe))

pbm3 <- ggplot(data = final %>% filter(!is.na(bodymass_cat))) +
  geom_point(aes(x = Body_mass_g, y = AnomVArr, col = bodymass_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Body mass (g)") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = Body_mass_g, y = AnomVArr), col = "black")

egg::ggarrange(pbm3, pbm2, pbm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

modA_bm1 <- lmer(data = final, AnomVArr ~ Body_mass_g + cell_lat +
                   (1|Family:species) + (1|year) + (1|cell)) 
modA_bm1g <- gam(data = final %>% 
                   mutate(species = as.factor(species),
                          cell = as.factor(cell)), 
                 AnomVArr ~ Body_mass_g + 
                   s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

sjPlot::tab_model(modA_bm1, show.re.var= TRUE, digits = 3)
summary(modA_bm1g)

#### - Distance_m ---------------------------
pdm1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = Distance_m),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(name = "Distance") 

meansdm <- final %>% 
  filter(!is.na(Distance_cat)) %>% 
  group_by(Distance_cat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pdm2 <- ggplot(data = final %>% filter(!is.na(Distance_cat))) +
  geom_boxplot(aes(x = Distance_cat, 
                   y = AnomVArr, fill = Distance_cat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Distance") +
  geom_point(data = meansdm, aes(x = Distance_cat, y = meanspe))

pdm3 <- ggplot(data = final) +
  geom_point(aes(x = Distance_m, y = AnomVArr, col = Distance_cat),
             alpha = 0.3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y="Log(Bird speed)", x = "Distance") +
  scale_fill_viridis(discrete = TRUE) +
  geom_smooth(aes(x = Distance_m, y = AnomVArr), col = "black")

egg::ggarrange(pdm3, pdm2, pdm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

modA_dm1 <- lmer(data = final, AnomVArr ~ Distance_m + cell_lat + 
                   (1|Family:species) + (1|year) + (1|cell)) 
modA_dm1g <- gam(data = final %>% 
                   mutate(species = as.factor(species),
                          cell = as.factor(cell)), 
                 AnomVArr ~ Distance_m + 
                   s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

sjPlot::tab_model(modA_dm1,show.re.var= TRUE, digits = 3)
summary(modA_dm1g)

#### - Diet group ----------------------------------
pdt1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = Diet),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
  scale_fill_viridis(discrete = TRUE, name = "Diet") 

meansdt <- final %>% 
  filter(!is.na(Diet)) %>% 
  group_by(Diet) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pdt2 <- ggplot(data = final %>% filter(!is.na(Diet))) +
  geom_boxplot(aes(x = Diet, 
                   y = AnomVArr, fill = Diet),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = -6, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Distance") +
  geom_point(data = meansdt, aes(x = Diet, y = meanspe))

egg::ggarrange(pdt2, pdt1, ncol = 2, 
               top = "Migration speed", widths = c(2, 12))

modA_dm1 <- lmer(data = final, AnomVArr ~ Diet + cell_lat + 
                   (1|Family:species) + (1|year) + (1|cell)) 

sjPlot::tab_model(modA_dm1, show.re.var= TRUE, digits = 3)
sort(unique(final$Diet))

### Green-up - are birds tracking green up? --------------------------------
modA_b1 <- lmer(data = final, AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                  cell_lat + (1|species) + (1|year) + (1|cell))

modA_b2 <- lm(data = final, AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                cell_lat)

modA_b3 <- lmer(data = final, AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                  cell_lat + (1|species) + (1|year) + (1|cell) +
                  gu_spe_mean + gu_dat_mea + gu_speA_mea + gu_datA_mea)

modA_b2g <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

head(aic_b <- AIC(modA_b1, modA_b2#, modA_b3
) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_b)[1]), show.re.var= TRUE, digits = 3)
summary(modA_b2g)

#### gr_mn - positive effect: the later green up is in the year, the faster birds move ----------------
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = gr_mn, y = AnomVArr, col = species), alpha = 0.4) +
  geom_smooth(aes(x = gr_mn, y = AnomVArr), col = "black") +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")

#### AnomDGr - negative effect: the earlier the green up is, the faster the birds are  ----------------
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr, y = AnomVArr, col = species), alpha = 0.4) +
  geom_smooth(aes(x = AnomDGr, y = AnomVArr),# col = species), se = F
              col = "black"
  ) +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")

### Lag - is being behind or ahead makes bird change speed?  ------------------------------
modA_c1 <- lmer(data = final, AnomVArr ~ AnomLag + lag + b_lag_mean + I(AnomLag^2) +
                  (1|species) + (1|year) + cell_lat + (1|cell))

modA_c2 <- lm(data = final, AnomVArr ~ AnomLag + lag + b_lag_mean + I(AnomLag^2) + cell_lat)

modA_c3 <- lmer(data = final, AnomVArr ~ AnomLag + lag + b_lag_mean + 
                  (1|species) + (1|year) + cell_lat + (1|cell))

modA_c2g <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                AnomVArr ~ AnomLag + lag + b_lag_mean + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

modA_c3g <- mgcv::gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                log(vArrMag) ~ s(AnomLag, bs = "tp") + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
summary(modA_c3g)

head(aic_c <- AIC(modA_c1, modA_c2, modA_c3) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_c)[1]), show.re.var= TRUE, digits = 3)
summary(modA_c2g)

### Breeding vs migration route - how is speed changing?  -------------------
modA_ran1 <- lmer(data = final,
                  AnomVArr ~ mig_cell + cell_lat +
                    (1|species) + (1|year) + (1|cell))

modA_ran2 <- lm(data = final,
                AnomVArr ~ mig_cell + cell_lat)

modA_ran1g <- gam(data = final %>% 
                    mutate(cell_type = as.factor(mig_cell),
                           species = as.factor(species),
                           cell = as.factor(cell)),
                  AnomVArr ~ mig_cell +
                    s(species, bs = 're') + s(year, bs = 're') + s(cell_lat, bs = 'tp') + s(cell, bs = 're')) 

head(aic_ran <- AIC(modA_ran1, modA_ran2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_ran)[1]), show.re.var= TRUE, digits = 3)
summary(modA_ran1g)

### Latitude ---------------------------------------------------
#### speed across latitude  ---------------------------------------------------
modA_ls1 <- lmer(data = final, AnomVArr ~ cell_lat + I(cell_lat^2) +
                   (1|species) + (1|year) + (1|cell))

modA_ls2 <- lm(data = final, AnomVArr ~ cell_lat + I(cell_lat^2))

modA_lsg <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                AnomVArr ~ s(cell_lat, bs = "tp") +
                  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 


head(aic_ls <- AIC(modA_ls1, modA_ls2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_ls)[1]), show.re.var= TRUE, digits = 3)
summary(modA_lsg)

#### Lag across latitude  ---------------------------------------------------
modA_ll1 <- lmer(data = final, lag ~ cell_lat + I(cell_lat^2) +
                   (1|species) + (1|year) + (1|cell))

modA_ll2 <- lm(data = final, lag ~ cell_lat + I(cell_lat^2))

modA_llg <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                lag ~ s(cell_lat, bs = "tp") +
                  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 

head(aic_ll <- AIC(modA_ll1, modA_ll2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_ll)[1]),show.re.var= TRUE, digits = 3)
summary(modA_llg)

modA_lal1 <- lmer(data = final, AnomLag ~ cell_lat + I(cell_lat^2) +
                    (1|species) + (1|year) + (1|cell))

modA_lal2 <- lm(data = final, AnomLag ~ cell_lat + I(cell_lat^2))

modA_lalg <- gam(data = final %>% 
                   mutate(species = as.factor(species),
                          cell = as.factor(cell)), 
                 AnomLag ~ s(cell_lat, bs = "tp") +
                   s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 

head(aic_lal <- AIC(modA_lal1, modA_lal2) %>% arrange(AIC)) 
sjPlot::tab_model(get(rownames(aic_lal)[1]),show.re.var= TRUE, digits = 3)
summary(modA_lalg)

#### are late birds faster?
final %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr, y = AnomVArr, col = species), alpha = 0.5) +
  geom_smooth(aes(x = AnomDGr, y = AnomVArr), col = "black") +
  theme_bw() +
  ggtitle("Arrival dates vs speed") +
  theme(legend.position = "none")

final %>% 
  ggplot() +
  geom_point(aes(x = gr_mn, y = AnomVArr, col = species), alpha = 0.5) +
  geom_smooth(aes(x = gr_mn, y = AnomVArr), col = "black") +
  theme_bw() +
  ggtitle("Arrival dates vs speed") +
  theme(legend.position = "none")


