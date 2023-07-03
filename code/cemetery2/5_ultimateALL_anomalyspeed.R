library(egg)
library(ggplot2)
library(tidyverse)
library(viridis)
library("glue")
library(lemon)
library(lme4)
library(mgcv)
library(MASS)
library(cAIC4)

# IMPORT DATA ---------------------------
final <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/birdgreen.rds")
cells <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/cellcoor.rds")
velocityG <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/velocityG.rds") %>% 
  left_join(., cells, by = "cell")


# FORMAT DATA ---------------------------- 
### green up years: intercept and slopes of arrival dates and speed per years (fast-slow/ early-late)---------------------------
gudat_lm <- lm(data = final %>% select(gr_mn, cell, cell_lat, year) %>% distinct(),
               gr_mn ~ cell_lat * as.factor(year) - 1)
summary(lm(data = final %>% select(gr_mn, cell, cell_lat, year) %>% distinct(),
           gr_mn ~ cell_lat * as.factor(year)))
yeardlm <- cbind(seq(2002, 2017, 1),as.numeric(gudat_lm$coefficients[2:17]), 
                 c(gudat_lm$coefficients[1], (gudat_lm$coefficients[1] - as.numeric(gudat_lm$coefficients[18:32]))))
colnames(yeardlm) <- c("year", "gu_dat_int", "gu_dat_slo")
yeardlm <- as.data.frame(yeardlm)
rownames(yeardlm) <- NULL

gudat_lm_mean <- lmer(data = final %>% select(gr_mn, cell, cell_lat, year) %>% distinct(),
                      gr_mn ~ cell_lat + (1|year))
summary(gudat_lm_mean)
gudat_lm_mean_int <- as.numeric(getME(gudat_lm_mean, name = "fixef")[1])
gudat_lm_mean_slo <- as.numeric(getME(gudat_lm_mean, name = "fixef")[2])

yeardlm <- yeardlm %>% 
  mutate(gu_dat_int = as.numeric(gu_dat_int),
         gu_dat_slo = as.numeric(gu_dat_slo),
         ano_gu_dat_int = gu_dat_int - gudat_lm_mean_int,
         ano_gu_dat_slo = gu_dat_slo - gudat_lm_mean_slo)

guspe_lm <- lm(data = final %>% select(vGrMag, cell, cell_lat, year) %>% distinct(),
               log(vGrMag) ~ cell_lat * as.factor(year) - 1)
summary(lm(data = final %>% select(vGrMag, cell, cell_lat, year) %>% distinct(),
           log(vGrMag) ~ cell_lat * as.factor(year)))
yearslm <- cbind(seq(2002, 2017, 1),as.numeric(guspe_lm$coefficients[2:17]), 
                 c(guspe_lm$coefficients[1], (guspe_lm$coefficients[1] - as.numeric(guspe_lm$coefficients[18:32]))))
colnames(yearslm) <- c("year", "gu_spe_int", "gu_spe_slo")
yearslm <- as.data.frame(yearslm)
rownames(yearslm) <- NULL

guspe_lm_mean <- lmer(data = final %>% select(vGrMag, cell, cell_lat, year) %>% distinct(),
                      log(vGrMag) ~ cell_lat + (1|year))
sjPlot::tab_model(guspe_lm_mean, show.re.var= TRUE, digits = 3)

guspe_lm_mean_int <- as.numeric(getME(guspe_lm_mean, name = "fixef")[1])
guspe_lm_mean_slo <- as.numeric(getME(guspe_lm_mean, name = "fixef")[2])

yearslm <- yearslm %>% 
  mutate(gu_spe_int = as.numeric(gu_spe_int),
         gu_spe_slo = as.numeric(gu_spe_slo),
         ano_gu_spe_int = gu_spe_int - guspe_lm_mean_int,
         ano_gu_spe_slo = gu_spe_slo - guspe_lm_mean_slo)

final <- final %>% 
  left_join(., yearslm, by = "year") %>% 
  left_join(., yeardlm, by = "year")

dim(final) ; dim(final)[1] == 15875


### overwinter latitude (from how far south do we come!) ---------------------------
winlat <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/Table_S1.csv") %>%
  dplyr::select(Species, Overwinterlatitude) %>% 
  mutate(species2 = Species) %>% 
  rename(species = Species, 
         winlat = Overwinterlatitude) %>% 
  mutate(species = sub(" ", "_", species))

final <- left_join(final, winlat, by = "species")

dim(final) ; dim(final)[1] == 15875

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

dim(final) ; dim(final)[1] == 15875

### species sensitivity ---------------------------
sensi <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/data_sensi.RDS") %>% 
  dplyr::select(sci_name,
                cell,
                beta_mean) %>% 
  rename(species = sci_name,
         sensi = beta_mean) %>% 
  group_by(species) %>% 
  mutate(sensi_mea = mean(sensi, na.rm = T),
         sensi_med = median(sensi, na.rm = T)) %>% 
  ungroup()

### cell numbers ---------------------------
cellnumbs <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdMigrationSpeed/data/cellnumbs.rds")
sensi <- left_join(sensi, cellnumbs, by = "cell") %>% 
  dplyr::select(-cell) %>% 
  rename(cell = cell2)

final <- left_join(final, sensi, by = c("species","cell")) 

dim(final) ; dim(final)[1] == 15875

### sensitivity categorical
hist(pull(final %>% dplyr::select(sensi)))

#(q1sen <- quantile(final %>% dplyr::select(sensi), c(0.33, 0.66), na.rm = T)[1])
#(q2sen <- quantile(final %>% dplyr::select(sensi), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final$sensi, na.rm = T), max(final$sensi, na.rm = T), 0.01)
q1sen <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2sen <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]
# q1sen <- -5 ; q1sen <- 2.5

abline(v = q1sen, col = "red")
abline(v = q2sen, col = "red")

final <- final %>% 
  mutate(sensicat = ifelse(sensi < q1sen, 'Low', 
                           ifelse(sensi >= q1sen & sensi < q2sen, "Medium", 
                                  ifelse(sensi >= q2sen, "High", "opps"))))

final$sensicat <- factor(final$sensicat, ordered = TRUE,
                         levels = c("Low","Medium","High"))

### breeding latitude (how far north we go!) ---------------------------
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

final <- final %>% 
  left_join(., avgsumlat, by = "species") 

dim(final) ; dim(final)[1] == 15875

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

dim(final) ; dim(final)[1] == 15875

### bird speed: lm - species and years - intercept is baseline, slope is variation (check within and between years (within adjust? la sorte 2017))  ---------------------------
###                  years - years that birds are going fast and slow ???

### lm for speed  - species that are fast and slow
bspe_lm <- lmer(data = final %>% select(AnomVArr, cell, cell_lat, species, year) %>% 
                  filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                AnomVArr ~ cell_lat * as.factor(species) - 1 + (1|year))
bspe_lm_mean <- lmer(data = final %>% select(AnomVArr, cell, cell_lat, species, year) %>% 
                       filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                     AnomVArr ~ cell_lat + (1|species))
bspe_lm_mean_int <- as.numeric(getME(bspe_lm_mean, name = "fixef")[1])
bspe_lm_mean_slo <- as.numeric(getME(bspe_lm_mean, name = "fixef")[2])

spspelm <- cbind(sort(unique(pull(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% dplyr::select(species)))), 
                 as.numeric(getME(bspe_lm, name = "fixef")[2:55]), 
                 c(as.numeric(getME(bspe_lm, name = "fixef")[1]), (as.numeric(getME(bspe_lm, name = "fixef")[1]) - as.numeric(getME(bspe_lm, name = "fixef")[56:108]))))
colnames(spspelm) <- c("species", "b_spe_int", "b_spe_slo")
spspelm <- as.data.frame(spspelm)
spspelm <- spspelm %>% 
  mutate(b_spe_int = as.numeric(b_spe_int),
         b_spe_slo = as.numeric(b_spe_slo),
         ano_b_spe_int = b_spe_int - bspe_lm_mean_int,
         ano_b_spe_slo = b_spe_slo - bspe_lm_mean_slo)

final <- final %>% 
  left_join(., spspelm, by = "species") 

dim(final) ; dim(final)[1] == 15875

## years that are fast and slow
byr_lm <- lmer(data = final %>% select(AnomVArr, cell, cell_lat, year, species) %>% distinct(),
               AnomVArr ~ cell_lat * as.factor(year) - 1 + (1|species))    #no estimate for 2002?
yearslm <- cbind(seq(2003, 2017, 1), as.numeric(getME(byr_lm, name = "fixef")[2:16]), 
                 c(as.numeric(getME(byr_lm, name = "fixef")[1]), 
                   (as.numeric(getME(byr_lm, name = "fixef")[1]) - as.numeric(as.numeric(getME(byr_lm, name = "fixef")[17:30])))))
colnames(yearslm) <- c("year", "b_yr_int", "b_yr_slo")
yearslm <- as.data.frame(yearslm)
rownames(yearslm) <- NULL

byr_lm_mean <- lmer(data = final %>% select(AnomVArr, cell, cell_lat, year) %>% distinct(),
                    AnomVArr ~ cell_lat + (1|year))
byr_lm_mean_int <- as.numeric(getME(byr_lm_mean, name = "fixef")[1])
byr_lm_mean_slo <- as.numeric(getME(byr_lm_mean, name = "fixef")[2])

yearslm <- yearslm %>% 
  mutate(b_yr_int = as.numeric(b_yr_int),
         b_yr_slo = as.numeric(b_yr_slo),
         ano_b_yr_int = b_yr_int - byr_lm_mean_int,
         ano_b_yr_slo = b_yr_slo - byr_lm_mean_slo)

final <- final %>% 
  left_join(., yearslm, by = "year") 

dim(final) ; dim(final)[1] == 15875

### lm for arrival dates  - species that are early or late
bdat_lm <- lmer(data = final %>% select(arr_GAM_mean, cell, cell_lat, species, year) %>% 
                  filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                arr_GAM_mean ~ cell_lat * as.factor(species) - 1 + (1|year))
bdat_lm_mean <- lmer(data = final %>% select(arr_GAM_mean, cell, cell_lat, species, year) %>% 
                       filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                     arr_GAM_mean ~ cell_lat + (1|species))
bdat_lm_mean_int <- as.numeric(getME(bdat_lm_mean, name = "fixef")[1])
bdat_lm_mean_slo <- as.numeric(getME(bdat_lm_mean, name = "fixef")[2])

spdatlm <- cbind(sort(unique(pull(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% dplyr::select(species)))), 
                 as.numeric(getME(bdat_lm, name = "fixef")[2:55]), 
                 c(as.numeric(getME(bdat_lm, name = "fixef")[1]), (as.numeric(getME(bdat_lm, name = "fixef")[1]) - as.numeric(getME(bdat_lm, name = "fixef")[56:108]))))
colnames(spdatlm) <- c("species", "b_dat_int", "b_dat_slo")
spdatlm <- as.data.frame(spdatlm)
spdatlm <- spdatlm %>% 
  mutate(b_dat_int = as.numeric(b_dat_int),
         b_dat_slo = as.numeric(b_dat_slo),
         ano_b_dat_int = b_dat_int - bdat_lm_mean_int,
         ano_b_dat_slo = b_dat_slo - bdat_lm_mean_slo)

final <- final %>% 
  left_join(., spdatlm, by = "species") 

dim(final) ; dim(final)[1] == 15875

## plots! 
### bird annual speed
final %>% 
  dplyr::select(year, AnomVArr, b_yr_int, ano_b_yr_int) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year, y = AnomVArr)) +
  geom_point(aes(x = year, y = b_yr_int), col = "#00BFC4") +
  theme_bw() +
  geom_errorbar(aes(ymin = (b_yr_int - ano_b_yr_int), ymax = (b_yr_int + ano_b_yr_int), x = year), 
                width=.2, position=position_dodge(.9), col = "#00BFC4") +
  ggtitle("Intercept of bird annual speed")

### bird species speed
final %>% 
  dplyr::select(species, year, AnomVArr, b_spe_int, ano_b_spe_int, species2) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr)) +
  geom_point(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                 y = b_spe_int), col = "#FF68A1") +
  theme_bw() +
  geom_errorbar(aes(ymin = (b_spe_int - ano_b_spe_int), ymax = (b_spe_int + ano_b_spe_int), 
                    x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE)), 
                width=.2, position=position_dodge(.9), col = "#FF68A1") +
  ggtitle("Intercept of bird species speed") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic"))

### bird annual speed 'anomaly'
final %>% 
  dplyr::select(year, AnomVArr, b_yr_int, ano_b_yr_int) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = ano_b_yr_int), col = "#00BFC4") +
  theme_bw() +
  geom_errorbar(aes(ymin = (b_yr_int - ano_b_yr_int), ymax = (b_yr_int + ano_b_yr_int), x = year), 
                width=.2, position=position_dodge(.9), col = "#00BFC4") +
  ggtitle("Intercept of bird annual speed")

### are late birds faster?
final %>% 
  dplyr::select(year, species, AnomVArr, b_spe_int, ano_b_spe_int, b_dat_int, ano_b_dat_int) %>% 
  ggplot() +
  geom_point(aes(x = b_dat_int, y = b_spe_int, col = species), alpha = 0.5) +
  geom_smooth(aes(x = b_dat_int, y = b_spe_int), col = "black") +
  theme_bw() +
  ggtitle("Arrival dates vs speed") +
  theme(legend.position = "none")
## same :(
final %>% 
  dplyr::select(year, species, AnomVArr, b_spe_int, ano_b_spe_int, b_dat_int, ano_b_dat_int) %>% 
  ggplot() +
  geom_point(aes(x = ano_b_dat_int, y = ano_b_spe_int, col = species), alpha = 0.5) +
  geom_smooth(aes(x = ano_b_dat_int, y = ano_b_spe_int), col = "black") +
  theme_bw() +
  ggtitle("Arrival dates anomaly vs speed anomaly") +
  theme(legend.position = "none")

### lag : mean, median and variance ---------------------------
final <- final %>% 
  group_by(species) %>% 
  mutate(lag_var = var(lag, na.rm = T),
         lag_mea = mean(lag, na.rm = T),
         lag_med = median(lag, na.rm = T)) %>% 
  ungroup() 

dim(final) ; dim(final)[1] == 15875

### early arrivers (date classification) (paper: only early arrivers are arriving earlier!) ---------------------------
# five first cells each species arrived, and get the arrival means, medians and variances for that
ea_tab <- final %>% 
  filter(mig_cell == T) %>% 
  dplyr::select(species, cell_lat, cell) %>% 
  distinct() %>% 
  group_by(species) %>% 
  slice_min(order_by = cell_lat, n = 5) %>% 
  mutate(eararr_cell = T) %>% 
  dplyr::select(-cell_lat)

ea_tab2 <- left_join(final, ea_tab, by = c("species", "cell")) %>% 
  filter(eararr_cell == T) 

hist(ea_tab2$arr_GAM_mean)

#(q1ea <- quantile(ea_tab2$arr_GAM_mean, c(0.33, 0.66), na.rm = T)[1])
#(q2ea <- quantile(ea_tab2$arr_GAM_mean, c(0.33, 0.66), na.rm = T)[2])

x <- seq(min(ea_tab2$arr_GAM_mean, na.rm = T), 
         max(ea_tab2$arr_GAM_mean, na.rm = T), 1)
q1ea <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2ea <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

abline(v = q1ea, col = "red")
abline(v = q2ea, col = "red")

ea_tab3 <- ea_tab2 %>% 
  group_by(species) %>% 
  mutate(b_datea_mea = mean(arr_GAM_mean, na.rm = T),
         b_datea_med = median(arr_GAM_mean, na.rm = T),
         b_datea_var = var(arr_GAM_mean, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::select(species, b_datea_mea, b_datea_med, b_datea_var) %>% 
  distinct()  %>% 
  mutate(b_datea_cat = ifelse(b_datea_mea < q1ea, 'Early', 
                              ifelse(b_datea_mea >= q1ea & b_datea_mea < q2ea, "Medium", 
                                     ifelse(b_datea_mea >= q1ea, "Late", "opps"))))

plot(ea_tab3$b_datea_mea, ea_tab3$b_datea_med)

final <- left_join(final, ea_tab3, by = "species")

final$b_datea_cat <- factor(final$b_datea_cat, ordered = TRUE,
                            levels = c("Early","Medium","Late"))

dim(final) ; dim(final)[1] == 15875

### speed in migration route ---------------------------
b_spe_mig <- final %>%
  dplyr::select(species, mig_cell, AnomVArr) %>% 
  filter(mig_cell == T) %>% 
  group_by(species) %>% 
  mutate(b_spemig_mea = mean(AnomVArr, na.rm = T),
         b_spemig_med = median(AnomVArr, na.rm = T),
         b_spemig_var = var(AnomVArr, na.rm = T)) %>% 
  dplyr::select(-mig_cell) %>% 
  dplyr::select(-AnomVArr) %>% 
  distinct()

final <- final %>% 
  left_join(., b_spe_mig, by = "species")

dim(final) ; dim(final)[1] == 15875

### speed in breeding route ---------------------------
b_spe_bre <- final %>%
  dplyr::select(species, breed_cell, AnomVArr) %>% 
  filter(breed_cell == T) %>% 
  group_by(species) %>% 
  mutate(b_spebre_mea = mean(AnomVArr, na.rm = T),
         b_spebre_med = median(AnomVArr, na.rm = T),
         b_spebre_var = var(AnomVArr, na.rm = T)) %>% 
  dplyr::select(-breed_cell) %>% 
  dplyr::select(-AnomVArr) %>% 
  distinct()

final <- final %>% 
  left_join(., b_spe_bre, by = "species")

dim(final) ; dim(final)[1] == 15875

final <- final %>% mutate(cell_type = ifelse(breed_cell == T & mig_cell == T, "both",
                                             ifelse(breed_cell == T & mig_cell == F, "bre",
                                                    ifelse(breed_cell == F & mig_cell == T, "mig","ops"))))
dim(final) ; dim(final)[1] == 15875


### body size and taxonomy (random effect for family and species nested) ---------------------------
mass_tax <- read_csv("data/source/gcb14540-sup-0001-supinfo_mass.csv") %>% 
  dplyr::select(-Radar) %>% 
  dplyr::select(-CommonName)

unique(final[which(final$species %in% pull(mass_tax[which(is.na(mass_tax$Body_mass_g)),1])),3])

final <- left_join(final, mass_tax, by = "species")
dim(final) ; dim(final)[1] == 15875

### body mass categorical
hist(pull(final %>% dplyr::select(Body_mass_g))) 

#(q1bm <- quantile(final %>% dplyr::select(Body_mass_g), c(0.33, 0.66), na.rm = T)[1])
#(q2bm <- quantile(final %>% dplyr::select(Body_mass_g), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final$Body_mass_g, na.rm = T), max(final$Body_mass_g, na.rm = T), 1)
#q1bm <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
#q2bm <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

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

dim(final) ; dim(final)[1] == 15875

### distance categorical
hist(pull(final %>% dplyr::select(Distance_m))) 

#(q1dm <- quantile(final %>% dplyr::select(Distance_m), c(0.33, 0.66), na.rm = T)[1])
#(q2dm <- quantile(final %>% dplyr::select(Distance_m), c(0.33, 0.66), na.rm = T)[2])
x <- seq(min(final$Distance_m, na.rm = T), max(final$Distance_m, na.rm = T), 1)
q1dm <- (split(x, sort((seq_along(x))%%3))$`1`)[1]
q2dm <- tail(split(x, sort((seq_along(x))%%3))$`1`)[6]

#q1dm <- 15
#q2dm <- 35

abline(v = q1dm, col = "red")
abline(v = q2dm, col = "red")

final <- final %>% 
  mutate(Distance_cat = ifelse(Distance_m < q1dm, 'Light', 
                               ifelse(Distance_m >= q1dm & Distance_m < q2dm, "Medium", 
                                      ifelse(Distance_m >= q2dm, "Heavy", "opps"))))

final$Distance_cat <- factor(final$Distance_cat, ordered = TRUE,
                             levels = c("Light","Medium","Heavy"))

dim(final) ; dim(final)[1] == 15875


### diet group ---------------------------
diet <- read_csv("data/source/jane13345-sup-0002-tables1_diet.csv") %>% 
  dplyr::select(Species, Diet) %>% 
  rename(species = Species)
unique(final[which(final$species %in% pull(diet[which(is.na(diet$Diet)),1])),3])

final <- left_join(final, diet, by = "species")
dim(final) ; dim(final)[1] == 15875

### anomaly on speed and data: variance(spread) within year and between year, mean and median (late or early?) for years -------------
final <- final %>% 
  group_by(species) %>% 
  mutate(b_anospe_var = var(AnomVArr, na.rm = T),
         b_anodat_var = var(arr_GAM_mean, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(species, year) %>% 
  mutate(b_anospeyr_mea = mean(AnomVArr, na.rm = T),
         b_anodatyr_mea = mean(arr_GAM_mean, na.rm = T),
         b_anospeyr_med = median(AnomVArr, na.rm = T),
         b_anodatyr_med = median(arr_GAM_mean, na.rm = T),
         b_anospeyr_var = var(AnomVArr, na.rm = T),
         b_anodatyr_var = var(arr_GAM_mean, na.rm = T)) %>% 
  ungroup() 

dim(final) ; dim(final)[1] == 15875

### taxonomic order
famcol <- read_csv("data/species_tax_ord.csv")
final <- left_join(final, famcol, by="species")
dim(final) ; dim(final)[1] == 15875

### lat categorical - for plotting purposes --------------------------
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
dim(final) ; dim(final)[1] == 15875


#------------------------------------------------------

### CHECK WITH DAVE speed anomaly and lag - if behind in cell A, they speed up in the next cell B up north? ---------------------------
#  if Anomlag is negative in the previous cell, does speed increases in relation to the previous cell?
#  tibble to establish NORTHERN neighbors
neis <- readRDS("data/cellnei.rds") %>% 
  dplyr::select(cell, cell_lat, adj)

neicell <- sort(unique(cells$cell)) %>% 
  as_tibble() %>% 
  rename(cell = value) %>% 
  left_join(., neis, by = "cell") %>% 
  mutate(nei = NA)
neicell$nei <- as.list(neicell$nei)

for(i in 1:nrow(neicell)){
  nc <- unlist(neicell$adj[i])
  lats <- neicell %>% 
    filter(cell %in% nc) %>% 
    dplyr::select(cell, cell_lat) 
  cellat <- neicell[i,1:2]
  lats <- lats[which(lats$cell_lat > as.numeric(cellat[,2])),1]
  if(length(pull(lats) != 0)) {neicell$nei[i] <- lats}
}

neicell <- neicell %>% 
  dplyr::select(cell, nei) %>% 
  rename(nornei = nei)

final <- left_join(final, neicell, by = "cell")

dim(final) ; dim(final)[1] == 15875




### diurnal vs nocturnal migrants (Newson 2016 in Schmaljohann) -----------------------------

### specialist vs. generalist -----------------------------
#(usui: Running counter to this prediction, however, previous comparative studies have found 
#   that generalist species are more responsive than specialists to climate change (Végvári et al. 2010;
#   Moussus et al. 2011; Hurlbert & Liang 2012).)





















# MODELS AND PLOTS -----------------------------
#   random effects for species nested on family and order, year, latitude, cell
#   (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)

## SPEED as response ----------------------------
### birds traits - what makes a species fast or slow? MODEL ---------------------------
# how far north they go to breed, how far south they come from, how sensitive they are,
#   early vs late arriver, body size, diet group, distance_m (?), taxonomy (not enough orders)
mod_a1 <- lm(data = final, AnomVArr ~ winlat + sensi + bre_lat_mea + 
               bre_lat_max + b_datea_mea + Body_mass_g + Distance_m ) 

mod_a2 <- lmer(data = final, AnomVArr ~ winlat + sensi + bre_lat_mea + 
                 bre_lat_max + b_datea_mea + Body_mass_g + Distance_m +
                 (1|Family:species) + 
                 #(1|Family) + 
                 #(1|species) +
                 (1|year) + 
                 (1|cell_lat) + 
                 (1|cell)) 

mod_a3 <- lm(data = final, AnomVArr ~ winlat + sensi + bre_lat_mea + 
               bre_lat_max + b_datea_mea + Body_mass_g + Distance_m +
               year + cell_lat)

head(aic_a <- AIC(mod_a1, mod_a2, mod_a3
) %>% arrange(AIC)) 

#summary(moda_1)
sjPlot::tab_model(
  get(rownames(aic_a)[1]),
  #mod1a,
  show.re.var= TRUE, digits = 3)

mod_a2g <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               AnomVArr ~ winlat + sensi + bre_lat_mea + 
                 bre_lat_max + b_datea_mea + Body_mass_g + Distance_m + 
                 s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 
summary(mod_a2g)

# Stepwise regression model
step.model_a2 <- stepcAIC(mod_a2, direction = "both", 
                          trace = FALSE, data = final)
summary(step.model_a2)

step.model_a1 <- stepAIC(mod_a1, direction = "both", 
                         trace = FALSE, data = final)
summary(step.model_a1)

step.model_a3 <- stepAIC(mod_a3, direction = "both", 
                         trace = FALSE, data = final)
summary(step.model_a3)

## with random body mass is the only significant
## how to do stepwise selection with random effects?


### birds traits - what makes a species fast or slow? PLOTS ---------------------------
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
                   y = AnomVArr, fill = sensi_mea),
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


#### - bre_lat_mea ---------------------------
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

mod_blm1 <- lmer(data = final, AnomVArr ~ bre_lat_mea + bre_lat_max +
                   (1|species) + (1|year) + (1|cell_lat) + (1|cell)) 

mod_blm1g <- gam(data = final %>% 
                   mutate(species = as.factor(species),
                          cell = as.factor(cell)),
                 AnomVArr ~ bre_lat_mea + bre_lat_max +
                   s(species, bs = 're') + s(year, bs = 're') + s(cell_lat, bs = 're') + s(cell, bs = 're')) 

head(aic_blm <- AIC(mod_blm1,mod_blm1g
) %>% arrange(AIC)) 

sjPlot::tab_model(
  #get(rownames(aic_blm)[1]),
  mod_blm1,
  show.re.var= TRUE, digits = 3)
summary(mod_blm1g)

#### - b_datea_mea ---------------------------

pdeam1 <- ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2, AnomVArr, FUN = median, na.rm = TRUE), 
                   y = AnomVArr, fill = b_datea_mea),
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
  filter(!is.na(b_datea_cat)) %>% 
  group_by(b_datea_cat) %>% 
  summarise(meanspe = mean(AnomVArr, na.rm = T))

pdeam2 <- ggplot(data = final %>% filter(!is.na(b_datea_cat))) +
  geom_boxplot(aes(x = b_datea_cat, 
                   y = AnomVArr, fill = b_datea_cat),
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
  geom_point(data = meansdeam, aes(x = b_datea_cat, y = meanspe))

pdeam3 <- ggplot(data = final %>% filter(!is.na(b_datea_cat))) +
  geom_point(aes(x = b_datea_mea, y = AnomVArr, col = b_datea_cat),
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
  geom_smooth(aes(x = b_datea_mea, y = AnomVArr), col = "black")

egg::ggarrange(pdeam3, pdeam2, pdeam1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

mod_deam1 <- lmer(data = final, AnomVArr ~ b_datea_mea + b_datea_med + b_datea_var +
                    (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)) 

mod_deam1g <- gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  AnomVArr ~ b_datea_mea + b_datea_med + b_datea_var +
                    s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 

head(taicdeam <- AIC(mod_deam1, mod_deam1g
) %>% arrange(AIC)) 

sjPlot::tab_model(
  #get(rownames(taicdeam)[1]),
  mod_deam1,
  show.re.var= TRUE, digits = 3)

summary(mod_deam1g)

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

mod_bm1 <- lmer(data = final, AnomVArr ~ Body_mass_g + 
                  (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)) 
mod_bm1g <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                AnomVArr ~ Body_mass_g + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 

sjPlot::tab_model(
  #get(rownames(aic_blm)[2]),
  mod_bm1,
  show.re.var= TRUE, digits = 3)
summary(mod_bm1g)

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

mod_dm1 <- lmer(data = final, AnomVArr ~ Distance_m + 
                  (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)) 
mod_dm1g <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                AnomVArr ~ Distance_m + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 

sjPlot::tab_model(
  #get(rownames(aic_blm)[2]),
  mod_dm1,
  show.re.var= TRUE, digits = 3)
summary(mod_dm1g)

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

mod_dm1 <- lmer(data = final, AnomVArr ~ Diet + 
                  (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell)) 

sjPlot::tab_model(
  #get(rownames(aic_blm)[2]),
  mod_dm1,
  show.re.var= TRUE, digits = 3)
sort(unique(final$Diet))

#####
### green-up - are birds tracking green up? --------------------------------
## this means aand medians are by year - so it is early and late years
mod_b1 <- lmer(data = final, AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                 gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell))

mod_b2 <- lm(data = final, AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
               gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
               gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo)

mod_b2g <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               AnomVArr ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                 gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 


head(aic_b <- AIC(mod_b1, mod_b2
) %>% arrange(AIC)) 

summary(mod_b2g)

#summary(moda_1)
sjPlot::tab_model(
  get(rownames(aic_b)[1]),
  #mod_b1,
  show.re.var= TRUE, digits = 3)

# Stepwise regression model
step.model_b1 <- stepcAIC(mod_b1, direction = "both", 
                          #groupCandidates = c("cell", "Family/species"),
                          #slopeCandidates = c("year", "cell_lat"),
                          trace = FALSE, data = final)
summary(step.model_b1)

step.model_b2 <- stepAIC(mod_b2, direction = "both", 
                         trace = FALSE, data = final)
summary(step.model_b2)

library(car)
avPlots(mod_b2)

## gr_mn - 
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = gr_mn, y = AnomVArr, col = species), alpha = 0.4) +
  geom_smooth(aes(x = gr_mn, y = AnomVArr), col = "black") +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")

## AnomDGr - 
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

## gu_spe_slo - positive effect: the more variation in green up, and the faster it gained speed with lat, the faster birds moved
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = gu_spe_slo, y = AnomVArr, col = species), alpha = 0.4) +
  geom_smooth(aes(x = gu_spe_slo, y = AnomVArr),# col = species), se = F
              col = "black"
  ) +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")

## gu_dat_slo - negative effect: the more variation in green up, and the faster it gained speed with lat, the faster birds moved (same as before?)
final %>% 
  filter(!is.na(cell_lat)) %>% 
  ggplot() +
  geom_point(aes(x = gu_dat_slo, y = AnomVArr, col = species), alpha = 0.4) +
  geom_smooth(aes(x = gu_dat_slo, y = AnomVArr),# col = species), se = F
              col = "black"
  ) +
  #facet_rep_wrap(~latcat) +
  theme_bw() +
  theme(legend.position = "none")

### speed and date anomaly and mean date - if it is a late year, or cell, birds speed up!
### lag - is being behind or ahead makes bird change speed?  ------------------------------
mod_c1 <- lmer(data = final, AnomVArr ~ AnomLag + lag_var + lag_mea + lag_med + I(AnomLag^2) +
                 (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell))

mod_c2 <- lm(data = final, AnomVArr ~ AnomLag + lag_var + lag_mea + lag_med + I(AnomLag^2))

head(aic_c <- AIC(mod_c1, mod_c2#, mod_c3, mod_c4
) %>% arrange(AIC)) 

mod_c2g <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               AnomVArr ~ AnomLag + lag_var + lag_mea + lag_med + I(AnomLag^2)+ 
                 s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 
#summary(moda_1)
sjPlot::tab_model(
  get(rownames(aic_c)[1]),
  #mod_b1,
  show.re.var= TRUE, digits = 3)

summary(mod_c2g)
### breeding vs migration route - how is speed changing?  -------------------
mod_ran1 <- lmer(data = final,
                 AnomVArr ~ cell_type +
                   (1|Family:species) + (1|year) + (1|cell_lat) + (1|cell))

mod_ran2 <- lmer(data = final,
                 AnomVArr ~ cell_type +
                   (1|species) + (1|year) + (1|cell_lat) + (1|cell))

mod_ran1g <- gam(data = final %>% 
                   mutate(cell_type = as.factor(cell_type),
                          species = as.factor(species),
                          cell = as.factor(cell)),
                 AnomVArr ~ cell_type +
                   s(species, bs = 're') + s(year, bs = 're') + s(cell_lat, bs = 're') + s(cell, bs = 're')) 

head(aic_ran <- AIC(mod_ran1, mod_ran2#, mod_ran1g#, mod_ran3#, mod_cran4
) %>% arrange(AIC)) 

#summary(moda_1)
sjPlot::tab_model(
  get(rownames(aic_ran)[1]),
  #mod_ran1,
  show.re.var= TRUE, digits = 3)

summary(mod_ran1g)

final %>% 
  filter(!is.na(cell_type)) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = AnomVArr, col = cell_type), alpha = 0.5) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr, col = cell_type)) +
  theme_bw()

### speed across latitude
mod_ll1 <- lmer(data = final, AnomVArr ~ cell_lat + I(cell_lat^2) +
                 (1|Family:species) + (1|year) + (1|cell))

mod_ll2 <- lm(data = final, AnomVArr ~ cell_lat + I(cell_lat^2))

mod_llg <- gam(data = final %>% 
                mutate(species = as.factor(species),
                       cell = as.factor(cell)), 
              AnomVArr ~ cell_lat + I(cell_lat^2) +
                s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 


head(aic_ll <- AIC(mod_ll1, mod_ll2
) %>% arrange(AIC)) 

summary(mod_llg)

#summary(moda_1)
sjPlot::tab_model(
  get(rownames(aic_ll)[1]),
  #mod_b1,
  show.re.var= TRUE, digits = 3)

### Lag across latitude  ---------------------------------------------------
mod_ll1 <- lmer(data = final, AnomLag ~ cell_lat + I(cell_lat^2) +
                  (1|Family:species) + (1|year) + (1|cell))

mod_ll2 <- lm(data = final, AnomLag ~ cell_lat + I(cell_lat^2))

mod_llg <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               AnomLag ~ cell_lat + I(cell_lat^2) +
                 s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 


head(aic_ll <- AIC(mod_ll1, mod_ll2
) %>% arrange(AIC)) 

summary(mod_llg)

#summary(moda_1)
sjPlot::tab_model(
  get(rownames(aic_ll)[1]),
  #mod_b1,
  show.re.var= TRUE, digits = 3)

#####
#####
#####
#####
#####



#### - faster are more flexible --------------------------------------

