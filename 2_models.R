## script to analyze how bird arrival date speed is affected by green-up and bird traits
pkgs <- c("here", "readr", "janitor", "mgcv", "gratia", "dplyr", "ggplot2",
          "ggrepel")
vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

library(mgcv)
library(tidyverse)
library('gratia')
library(ggplot2)
library(dplyr)
library(tidymv)
library(itsadug)

final <- read_rds(file = "data/final.rds")
annual <- read_rds(file = "data/annual.rds")
velG <- read_rds(file = "data/velocityG.rds") %>% as_tibble()

celco <- final %>% dplyr::select(cell, cell_lat) %>% distinct() %>% filter(!is.na(cell_lat))
for(i in 1:nrow(final)){
  if(is.na(final$cell_lat[i])){
    final$cell_lat[i] <- celco %>% 
      filter(cell == final$cell[i]) %>% 
      dplyr::select(cell_lat) %>% 
      pull() %>% 
      as.numeric()
  }
}

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(species) %>% 
  distinct() %>% nrow()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(cell) %>% 
  distinct() %>% nrow()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(species, cell) %>% 
  distinct() %>% dplyr::select(species) %>% table() %>% mean()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(species, cell) %>% 
  distinct() %>% dplyr::select(species) %>% table() %>% sd()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(species, cell) %>% 
  distinct() %>% dplyr::select(cell) %>% table() %>% mean()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(species, cell) %>% 
  distinct() %>% dplyr::select(cell) %>% table() %>% sd()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrMag) %>% pull() %>% max()
final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrMag) %>% pull() %>% min()
final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrMag) %>% pull() %>% mean()
final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrMag) %>% pull() %>% median()
final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrMag) %>% pull() %>% sd()

final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrAng) %>% mutate(vArrAng = vArrAng + 180) %>% pull() %>% mode()
final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrAng) %>% mutate(vArrAng = vArrAng + 180) %>% pull() %>% sd()
final %>% filter(!is.na(vArrMag)) %>% dplyr::select(vArrAng) %>% mutate(vArrAng = vArrAng + 180) %>% pull() %>% median()

###############################################################################################
# Annual bird mean speeds --------------------
mod_yrsp <- lm(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)),
                       AnomVArr ~ as.factor(year) - 1 ) 
summary(mod_yrsp)
#plot(mod_yrsp)
appraise(mod_yrsp, method = "simulate") 

# variances
bs_var <- final %>% dplyr::select(species, vArrMag) %>% group_by(species) %>% mutate(varspe = var(vArrMag, na.rm = T)) %>% distinct() 
mean(bs_var$varspe, na.rm = T)
median(bs_var$varspe, na.rm = T)

final %>% dplyr::select(species, AnomVArr) %>% group_by(species) %>% 
  mutate(meanspe = mean(AnomVArr, na.rm = T)) %>% 
  dplyr::select(species, meanspe) %>% distinct() %>% view()


spd_tab <- final %>% dplyr::select(cell_lat, vArrMag) %>% 
  mutate(cell_cat = case_when(cell_lat < -10 ~ -12.5,
                              -10 <= cell_lat & cell_lat < -5 ~ -7.5,
                              -5 <= cell_lat & cell_lat < 0 ~ -2.5,
                              0 <= cell_lat & cell_lat < 5 ~ 2.5,
                              5 <= cell_lat ~ 7.5),
         cell_cat = as.factor(cell_cat))

ggplot(data = spd_tab %>% filter(!is.na(cell_cat))) +
  #geom_hline(yintercept = 0, 
  #           color = "red", size=0.7) +
  geom_jitter(aes(cell_cat, log(vArrMag)), alpha = 0.08) +
  #geom_violin(aes(cell_cat, AnomLag), alpha = 0.6, width = 1) +
  geom_boxplot(aes(cell_cat, log(vArrMag)), alpha = 0.6, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Anomaly on speed (km/day)", x = "Latitude (degrees)") +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("(- ∞,-10]","(-10,-5]","(-5,0]","(0,5]","(5,10]")) +
  coord_flip() #+ ylim(-30,30)
###############################################################################################
# Annual anomalies on speed of birds and annual anomalies of GU --------------------
mod_yran <- lm(data = annual, ABS ~ AGS + AGD)

summary(mod_yran)
appraise(mod_yran, method = "simulate")

svg(glue("figures/mod_yran_AGD.svg"), 
    width = 5, height = 4)

jtools::effect_plot(mod_yran, pred = AGD, 
                    interval = TRUE#, rug = TRUE, rug.sides = "b"
                    ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)) +
  labs(x = "Anomaly on green-up arrival date (days)",
       y = "Anomaly on bird speed (km/day)") +
  scale_y_continuous(limits = c(-0.15,0.2))

dev.off()

svg(glue("figures/mod_yran_AGS.svg"), 
    width = 5, height = 4)
jtools::effect_plot(mod_yran, pred = AGS, 
                    interval = TRUE#, rug = TRUE, rug.sides = "b"
                    ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)) +
  labs(x = "Anomaly on green-up speed (km/day)",
       y = "Anomaly on bird speed (km/day)") +
  scale_y_continuous(limits = c(-0.15,0.2))
dev.off()

###############################################################################################
# Green-up effect on speed --------------------

mod_gu <- mgcv::gam(data = final %>% mutate(species = as.factor(species), 
                                            cell = as.factor(cell),
                                            #mig_cell = abs(mig_cell - 1),
                                            mig_cell = as.factor(mig_cell)
                                            ), 
                     log(vArrMag) ~ (AnomDGr + AnomVGr) * mig_cell + 
                       s(species, bs = "re") + 
                       s(year, bs = "re") + 
                       s(cell_lat, bs = "tp") +
                       s(cell, bs = "re"),
                    method = "REML") 

summary(mod_gu)
draw(mod_gu)
appraise(mod_gu, method = "simulate")
## plots

plot_smooth(mod_gu, view = "mig_cell") # mig_cel as numeric


svg(glue("figures/mod_gu_AnomDGr.svg"), 
    width = 4, height = 4)

plot_smooth(mod_gu, view = "AnomDGr", cond=list(mig_cell=F), ylim = c(3.7,4.3), rug = F) 

plot_smooth(mod_gu, view = "AnomDGr", cond=list(mig_cell=T), add = T, rug = F,lty = "dashed") 

dev.off()
# transformed axis:
exp(c(3.7, 3.8, 3.9, 4, 4.1))
a <- c(40, 40.44730,
       44.70118, 45, 
       49.40245, 50, 
       54.59815, 55, 
       60, 60.34029)
b <- log(c(40, 40.44730,
           44.70118, 45, 
           49.40245, 50, 
           54.59815, 55, 
           60, 60.34029
))
tab <- as.data.frame(cbind(a,b))

svg(glue("figures/mod_gu_AnomDGr_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
                     limits = c(3.6,4.2), 
                     breaks = log(c(40, 40.44730,
                                    44.70118, 45, 
                                    49.40245, 50, 
                                    54.59815, 55, 
                                    60, 60.34029)), 
                     labels = c(40, ".",
                                ".", 45, 
                                ".", 50, 
                                ".", 55, 
                                60, ".")) 
dev.off()


svg(glue("figures/mod_gu_AnomVGrF.svg"), 
    width = 4, height = 4)

plot_smooth(mod_gu, view = "AnomVGr", cond=list(mig_cell=T), lty = "dashed") 

plot_smooth(mod_gu, view = "AnomVGr", cond=list(mig_cell=F), add = T) 

dev.off()

# transformed axis:
exp(c(3.7, 3.8, 3.9, 4, 4.1, 4.2))
a <- c(40, 40.44730,
       44.70118, 45, 
       49.40245, 50, 
       54.59815, 55, 
       60, 60.34029,
       66.68633, 70)
b <- log(c(40, 40.44730,
           44.70118, 45, 
           49.40245, 50, 
           54.59815, 55, 
           60, 60.34029,
           66.68633, 70
))
tab <- as.data.frame(cbind(a,b))


svg(glue("figures/mod_gu_AnomVGr_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
                     limits = c(3.6,4.25), 
                     breaks = log(c(40, 40.44730,
                                    44.70118, 45, 
                                    49.40245, 50, 
                                    54.59815, 55, 
                                    60, 60.34029,
                                    66.68633, 70)), 
                     labels = c(40, ".",
                                ".", 45, 
                                ".", 50, 
                                ".", 55, 
                                60, ".",
                                ".", 70)) 

dev.off()


svg(glue("figures/mod_gu_migcell.svg"), 
    width = 4, height = 4)

jtools::effect_plot(mod_gu, pred = mig_cell, 
                    interval = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(#trans='log10', limits = c(3.6,4.2), 
                     breaks = log(c(40,45,50,55,60,65,70)), labels = c(40,45,50,55,60,65,70)) +
  scale_x_discrete(labels = c("Breeding\ncell","Migratory\ncell")) 

dev.off()
vis.gam(mod_gu)


# speed and bird traits --------------------
mod_tra <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ xi_mean + ea_lat + Diet + Body_mass_g + winlat + Time + 
                       s(cell_lat, bs = "tp") + s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"), 
                     method = "REML")
summary(mod_tra)
plot(mod_tra)
draw(mod_tra)
appraise(mod_tra, method = "simulate")
k.check(mod_tra)
acf(residuals(mod_tra))
rootogram(mod_tra) %>% 
  draw(warn_limits = FALSE, bar_fill = "black") +
  geom_hline(yintercept = -1, linetype="dotted", 
             color = "darkgray", size=0.5) +
  geom_hline(yintercept = 1, linetype="dotted", 
             color = "darkgray", size=0.5) 

# ea_lat
svg(glue("figures/mod_tra_ea_lat.svg"), 
    width = 4, height = 4)

plot_smooth(mod_tra, view = "ea_lat")

dev.off()

# transformed axis:
exp(c(3.2, 3.4,3.6,3.8, 4,4.2, 4.4))
a <- c(24.53253, 25,
       29.96410, 30,
       35, 36.59823,
       40, 44.70118,
       50, 54.59815,
       60, 66.68633,
       70,
       80, 81.45087,
       90
       )
b <- log(c(24.53253, 25,
           29.96410, 30,
           35, 36.59823,
           40, 44.70118,
           50, 54.59815,
           60, 66.68633,
           70,
           80, 81.45087,
           90
))
tab <- as.data.frame(cbind(a,b))

svg(glue("figures/mod_tra_ea_lat_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
    limits = c(3,4.5), 
    breaks = log(c(24.53253, 25,
                   29.96410, 30,
                   35, 36.59823,
                   40, 44.70118,
                   50, 54.59815,
                   60, 66.68633,
                   70,
                   80, 81.45087,
                   90)), 
    labels = c(".", 25,
               ".", 30,
               35, ".",
               40, ".",
               50, ".",
               60, ".",
               70,
               80, ".",
               90)) 

dev.off()

# diet

svg(glue("figures/mod_tra_Diet.svg"), 
    width = 5, height = 4)

jtools::effect_plot(mod_tra, pred = Diet, 
                    interval = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 13) 
        ) +
  scale_y_continuous(#trans='log10', 
    limits = c(3.2,4.9), 
    breaks = log(c(25, 35, 50, 75, 100, 125)), labels = c(25, 35, 50, 75, 100, 125)) +
  labs(y = "Bird speed (km/day, log scale)")

dev.off()

svg(glue("figures/mod_tra_Time.svg"), 
    width = 5, height = 4)

jtools::effect_plot(mod_tra, pred = Time, 
                    interval = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 13) 
  ) +
  scale_y_continuous(#trans='log10', 
    limits = c(2.7,4.6), 
    breaks = log(c(15, 20, 30, 50, 90)), labels = c(15, 20, 30, 50, 90)) +
  labs(y = "Bird speed (km/day, log scale)") +
  scale_x_discrete(labels = c("Diurnal","Nocturnal")) 

dev.off()


###############################################################################################
# anomalies on lag  --------------------
# speed before is the ea_lat in the southern most cell, and the cell where they are now
final_lag1 <- final %>% dplyr::select(species,
                                      year,
                                      cell, cell_lat, cell_lng,
                                      ea_lat, ea_lat_ano, ea_lat_yr, ea_lat_yr_ano,
                                      arr_GAM_mean, 
                                      vArrMag, AnomVArr,
                                      gr_mn, AnomDGr,
                                      vGrMag, AnomVGr,
                                      lag, AnomLag) %>% 
  filter(!is.na(arr_GAM_mean))%>% 
  filter(!is.na(ea_lat))

spslist <- sort(unique(final_lag1$species))

final_lag5 <- as.data.frame(matrix(NA, ncol = 4, nrow = 0))

for(s in 1:length(spslist)){
  final_lag2 <- final_lag1 %>% filter(species == spslist[s])
  years <- final_lag2 %>% dplyr::select(year) %>% arrange() %>% distinct() %>% pull
  for(y in 1:length(years)){
    final_lag3 <- final_lag2 %>% filter(year == years[y])
    cellsLoop <- final_lag3 %>% dplyr::select(cell) %>% arrange() %>% distinct() %>% pull
    # get minimum cell and date for that year
    date <- final_lag3 %>% dplyr::select(ea_lat) %>% pull() %>% first()
    place <- final_lag3 %>% dplyr::select(cell_lat, cell_lng) %>% arrange(cell_lat) %>% filter(row_number()==1)
    for(d in 1:length(cellsLoop)){
      final_lag4 <- final_lag3 %>% filter(cell == cellsLoop[d])
      date2 <- final_lag4 %>% dplyr::select(arr_GAM_mean) %>% pull()
      place2 <- final_lag4 %>% dplyr::select(cell_lat, cell_lng) %>% filter(row_number()==1)
      spe_bef <- (geodist::geodist(c(place2[1,2], place2[1,1]), c(place[1,2], place[1,1]), measure = 'geodesic' )/1000)/
        (date2 - date)
      if(spe_bef > 0) {final_lag5 <- rbind(final_lag5,
                          cbind(spslist[s], years[y], cellsLoop[d], spe_bef))} else {print("upa")}
    }
  }
}

colnames(final_lag5) <- c("species", "year", "cell", "past_spe")
final_lag5$year <- as.numeric(final_lag5$year)
final_lag5$cell <- as.numeric(final_lag5$cell)
final_lag5$past_spe <- as.numeric(final_lag5$past_spe)

final_lag <- left_join(final_lag1, final_lag5, by = c("species", "year", "cell")) %>% 
  mutate(species = as.factor(species),
         cell = as.factor(cell),
         past_spe_ano = scale(log(past_spe), scale = FALSE)) 

mod_lag <- mgcv::gam(data = final_lag, 
                      AnomLag ~ past_spe_ano + ea_lat_ano + AnomDGr + 
                        s(cell_lat, bs = "tp") + 
                       s(species, bs = "re"), method = "REML")


summary(mod_lag)
draw(mod_lag)
plot(mod_lag)
gam.check(mod_lag)
abline(a=0, b= 0, col = 'red')
appraise(mod_lag, method = "simulate")
k.check(mod_lag)
acf(residuals(mod_lag))
rootogram(mod_lag) %>% 
  draw(warn_limits = FALSE, bar_fill = "black") +
  geom_hline(yintercept = -1, linetype="dotted", 
             color = "darkgray", size=0.5) +
  geom_hline(yintercept = 1, linetype="dotted", 
             color = "darkgray", size=0.5) 
###
# lag z-score ------------------------------------
final_lag1 <- final %>% dplyr::select(species,
                                      year,
                                      cell, cell_lat, cell_lng,
                                      ea_lat, ea_lat_ano, ea_lat_yr, #ea_lat_yr_ano,
                                      arr_GAM_mean, 
                                      vArrMag, AnomVArr,
                                      gr_mn, AnomDGr,
                                      vGrMag, AnomVGr,
                                      lag, AnomLag, breed_cell) %>% 
  filter(!is.na(arr_GAM_mean))%>% 
  filter(!is.na(ea_lat)) %>% 
  filter(breed_cell == T)

spslist <- sort(unique(final_lag1$species))

final_lag5 <- as.data.frame(matrix(NA, ncol = 4, nrow = 0))

for(s in 1:length(spslist)){
  final_lag2 <- final_lag1 %>% filter(species == spslist[s])
  years <- final_lag2 %>% dplyr::select(year) %>% arrange() %>% distinct() %>% pull
  for(y in 1:length(years)){
    final_lag3 <- final_lag2 %>% filter(year == years[y])
    cellsLoop <- final_lag3 %>% dplyr::select(cell) %>% arrange() %>% distinct() %>% pull
    # get minimum cell and date for that year
    date <- final_lag3 %>% dplyr::select(ea_lat) %>% pull() %>% first()
    place <- final_lag3 %>% dplyr::select(cell_lat, cell_lng) %>% arrange(cell_lat) %>% filter(row_number()==1)
    for(d in 1:length(cellsLoop)){
      final_lag4 <- final_lag3 %>% filter(cell == cellsLoop[d])
      date2 <- final_lag4 %>% dplyr::select(arr_GAM_mean) %>% pull()
      place2 <- final_lag4 %>% dplyr::select(cell_lat, cell_lng) %>% filter(row_number()==1)
      spe_bef <- (geodist::geodist(c(place2[1,2], place2[1,1]), c(place[1,2], place[1,1]), measure = 'geodesic' )/1000)/
        (date2 - date)
      if(spe_bef > 0) {final_lag5 <- rbind(final_lag5,
                                           cbind(spslist[s], years[y], cellsLoop[d], spe_bef))} else {print("upa")}
    }
  }
}

colnames(final_lag5) <- c("species", "year", "cell", "past_spe")
final_lag5$year <- as.numeric(final_lag5$year)
final_lag5$cell <- as.numeric(final_lag5$cell)
final_lag5$past_spe <- as.numeric(final_lag5$past_spe)

final_lag <- left_join(final_lag1, final_lag5, by = c("species", "year", "cell"))
cellspec <- unique(final_lag[ ,c("cell","species")])

final2 <- matrix(NA, 0, (ncol(final_lag) + 3))

for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(final_lag, cellspec[a,1] == cell & cellspec[a,2] == species)
  if (nrow(dat.temp) >= 8){  # at least 8 years of data
    Anom <- apply(dat.temp[,c("past_spe","ea_lat_yr","gr_mn")], 2, 
                  function(x) scale(x, scale = T))
    colnames(Anom) <- c("past_spe_z","ea_lat_yr_z","gr_mn_z")
    dat.temp <- cbind(dat.temp, Anom)
    final2 <- rbind(final2, dat.temp)
  }
}
x1 <- sd(final$AnomDGr, na.rm=T)
final3 <- final2 %>% mutate(species = as.factor(species),
                  AnomDGr2 = AnomDGr/x1)
mod_lag <- mgcv::gam(data = final3, 
                     AnomLag ~ past_spe_z + ea_lat_yr_z + gr_mn_z + 
                       #s(cell_lat, bs = "tp") + 
                       s(cell, bs = "re") +
                       s(species, bs = "re"), method = "REML")

summary(mod_lag)

#summary(mod_lag2)

draw(mod_lag)

svg(glue("figures/mod_lag_past_spe_z.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lag, view = "past_spe_z") 


dev.off()

svg(glue("figures/mod_lag_ea_lat_yr_z.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lag, view = "ea_lat_yr_z") 

dev.off()

svg(glue("figures/mod_lag_gr_mn_z.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lag, view = "gr_mn_z") 

dev.off()


plot_smooth(mod_lag, view = "past_spe_z", ylim = c(-15,10)) 
plot_smooth(mod_lag, view = "gr_mn_z", add = T) 

# model for latitude --------------
mod_lag_lat2 <- mgcv::gam(data = final %>% mutate( 
  cell = as.factor(cell),
  #year = as.factor(year),
  species = as.factor(species)) %>% 
    filter (mig_cell == F),
  AnomLag ~  
    s(cell_lat, bs = "tp") + 
    s(year, bs = "re") + 
    s(cell, bs = "re") + 
    s(species, bs = "re"), 
  method = "REML")

plot_smooth(mod_lag_lat2, view = "cell_lat")
summary(mod_lag_lat2)

plot_smooth(mod_lag_lat, view = "cell_lat")

plot_smooth(mod_lag, view = "cell_lat")

mod_bspe_lat <- mgcv::gam(data = final %>% mutate( 
  cell = as.factor(cell),
  species = as.factor(species))  %>% filter (mig_cell == F),
  log(vArrMag) ~  
  #arr_GAM_mean ~
    s(cell_lat, bs = "tp") + 
    s(cell, bs = "re") + 
    s(year, bs = "re") +
    s(species, bs = "re"), 
  method = "REML")

summary(mod_bspe_lat)
plot_smooth(mod_bspe_lat, view = "cell_lat")

finalG <- read_rds(file = "data/birdgreen.rds") %>% 
  dplyr::select(cell, year, cell_lat,vGrMag, gr_mn, mig_cell, breed_cell) %>% 
  distinct()

mod_gre_lat <- mgcv::gam(data = finalG %>% 
                           mutate(cell = as.factor(cell)) %>% 
                           filter (mig_cell == F),
                     log(vGrMag) ~  
                     #gr_mn ~
                       s(cell_lat, bs = "tp") + 
                       s(cell, bs = "re") + 
                       s(year, bs = "re"), 
                     method = "REML")

summary(mod_gre_lat)
plot_smooth(mod_gre_lat, view = "cell_lat")

draw(mod_gre_lat)

###############################################################################################
# PANEL 3 -------------------------
ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) + # no estimates, no neighbors
  geom_boxplot(aes(x = reorder(species2,
                               #xi_mean, 
                               log(vArrMag),
                               FUN = median, na.rm = TRUE), 
                   y = log(vArrMag)#, fill = ea_lat
  ),
  width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Species first \narrival date\n") +
  labs(y = "Log(Speed)\n") +
  scale_y_continuous(trans='log10', breaks = c(10,100,1000,5000), labels = c(10,100,1000,5000))

final %>% filter(species != "Pipilo_erythrophthalmus") %>% 
  group_by(species) %>% 
  summarise(median = median(vArrMag, na.rm = T)) %>% 
  view()

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = winlat), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Wintering\nlatitude") +
  labs(y = "Log(Speed)\n") 

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = ea_lat), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "First\narrival date", option = "B") +
  labs(y = "Log(Speed)\n") 

ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = Distance_m), width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  #labs(title="Species") +
  scale_fill_viridis(name = "Migration\ndistance", option = "G") +
  labs(y = "Log(Speed)\n") 


###############################################################################################




