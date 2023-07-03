# 2_models.R -------------------------
#
# Code to analyze how bird arrival date speed is affected by green-up, bird traits and latitude
#   and which factors contribute to anomalies in lag
# models and plotting outputs
#
# Input:  data/final.rds
#         data/annual.rds
#         data/velocityG.rds
#         data/birdgreen.rds
#         data/velocityB.rds
#         data/cellcoor.rds
#
# Output:  
#
#
#

# Load packages ----------------------------
library(mgcv)
library(tidyverse)
library(gratia)
library(ggplot2)
library(dplyr)
library(tidymv)
library(itsadug)
library(glue)
library(viridis)
library(scales)
library(here)
library(readr)
library(janitor)
library(ggrepel)

# Load bird and green-up data -----------------------------
final <- read_rds(file = "data/final.rds")
#annual <- read_rds(file = "data/annual.rds")
velG <- read_rds(file = "data/velocityG.rds") %>% as_tibble()
finalG <- read_rds(file = "data/birdgreen.rds") %>% 
  dplyr::select(cell, year, cell_lat,vGrMag, gr_mn, mig_cell, breed_cell) %>% 
  distinct()
velocityB <- readRDS(file = "data/velocityB.rds")
cellcoor <- readRDS(file = "data/cellcoor.rds")

# make sure all rows have cell coordinates
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

# Summary results -------------------------
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

# Figure 1 - maps of migration speed and direction ----------------------------------
## maps with speed plot ----------------------------------
# maps are made with script "xxxxxxxxxx.R"




## histogram of migration direction ----------------------------------
preds <- left_join(velocityB, cellcoor[,1:3], by= "cell")
hist(preds$vArrAng, breaks = 30)
preds$vArrAng2 <- ifelse(preds$vArrAng > 270, preds$vArrAng - 360, preds$vArrAng)
hist(preds$vArrAng2, breaks = 30)

#regular
svg(glue("figures/Fig1/hist1.svg"), 
    width = 5, height = 3)
ggplot() +
  geom_histogram(aes(x = vArrAng2, y = (..count..)/sum(..count..)), data = preds) +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  theme_bw() +
  scale_x_continuous(breaks = c(-90, 0, 90, 180, 270),
                     labels = c("270\nEast",
                                "0, 360\nNorth",
                                "90\nWest",
                                "180\nSouth",
                                "270\nEast")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.35),
        panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.35),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        rect = element_rect(fill = "transparent"),
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0))) +
  xlab("Migration direction (degrees)") +
  ylab("Frequency")

dev.off()

# circular (radar chart)
svg(glue("figures/Fig1/hist2.svg"), 
    width = 4, height = 3)
ggplot() +
  geom_histogram(aes(x = vArrAng2, y = (..count..)/sum(..count..)), bins = 60, data = preds) +
  scale_y_continuous(labels=percent, limits = c(0,0.13), breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125)
                     ) +
  theme_bw() +
  scale_x_continuous(breaks = c(-90, -45, 0, 45, 90, 135, 180, 225, 270),
                     labels = c("270º\nE",
                                "315º",
                                "0º, 360º\nN",
                                "45º",
                                "90º\nW",
                                "135º",
                                "180º\nS",
                                "225º",
                                "270º\nE")) +
  theme(#axis.line = element_line(colour = "black", size = 0.35),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black", vjust = 5),
        axis.text.y = element_text(size = 8, colour = "black"),
        rect = element_rect(fill = "transparent"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  coord_polar(start = 67.5) 
dev.off()

# Figure 2 - gams of bird speed, green-up speed and lag varying with latitude ----------------------------------
## year ----------------------------
spse <- table(final$species) %>% sort(decreasing = T) 
spse <- names(spse[1:10])

my_breaks2 <- c(25, 50, 75, 100)
my_breaks1 <- log(my_breaks2)

final %>% filter(mig_cell == F, year > 2005) %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat2,
                  y = log(vArrMag), col = as.factor(year)
  ), se = F) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)) +
  labs(#title = "Lag", 
    y = "Lag (days)", x = "Latitude (degrees)") +
  scale_y_continuous(#trans='log10', 
    breaks = log(c(30, 40, 50, 60, 70)), labels = c(30, 40, 50, 60, 70)) +
  labs(y = "Bird speed (km/day, log scale)")

svg(glue("figures/lat_guspe_yr.svg"), 
    width = 5, height = 4)
final %>% 
  filter(mig_cell == F)%>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat2,
                  y = log(vGrMag), col = as.factor(year)), se = F) +
  #geom_hline(yintercept = 0, #linetype="dotted", 
  #           color = "black", size=0.5, alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)) +
  labs(#title = "Lag", 
    y = "Green-up speed (km/day, log scale)", x = "Latitude (degrees)") +
  scale_y_continuous(limits = c(3, 4.6), 
                     trans = "log",
                     breaks = my_breaks1, 
                     labels = my_breaks2) +
  scale_x_continuous(limits = c(26,47)) 

dev.off()

final %>% filter(mig_cell == F) %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat2,
                  y = lag, col = as.factor(year)), se = F) +
  geom_hline(yintercept = 0, #linetype="dotted", 
             color = "black", size=0.5, alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)) +
  labs(#title = "Lag", 
    y = "Lag (days)", x = "Latitude (degrees)") #+ facet_wrap(~year)

## species ----------------------------

svg(glue("figures/Fig2/lat_spsspe_10.svg"), 
    width = 5, height = 4)
final %>% 
  filter(species %in% spse,
         mig_cell == F) %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat2,
                  y = log(vArrMag), col = species), se = F) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(colour = "white"),
        axis.text = element_text(size = 13)) +
  labs(#title = "Lag", 
    y = "Bird speed (km/day, log scale)", x = "Latitude (degrees)"
    )  +
  scale_y_continuous(limits = c(3, 4.6), 
                     trans = "log",
                     breaks = my_breaks1, 
                     labels = my_breaks2) +
  scale_x_continuous(limits = c(26,47))
dev.off()

svg(glue("figures/Fig2/lat_spslag_10.svg"), 
    width = 5, height = 4)
final %>% 
  filter(species %in% spse,
         mig_cell == F) %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat2,
                  y = lag, col = species), se = F) +
  geom_hline(yintercept = 0, #linetype="dotted", 
             color = "black", size=0.5, alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(colour = "white"),
        axis.text = element_text(size = 13)) +
  labs(#title = "Lag", 
    y = "Lag (days)", x = "Latitude (degrees)") +
  scale_x_continuous(limits = c(26,47))
dev.off()


## GAM: latitude models ----------------------------
## bird speed
mod_lat_bspe <- mgcv::gam(data = final %>% 
                            mutate(cell = as.factor(cell),
                                   species = as.factor(species)) %>%
                            filter(mig_cell == T),
                          log(vArrMag) ~
                            s(cell_lat, bs = "tp") +
                            s(cell, bs = "re") + 
                            s(year, bs = "re") +
                            s(species, bs = "re"))

svg(glue("figures/Fig2/lat_spsspe_gam.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lat_bspe, view = "cell_lat") 

dev.off()

# green-up speed
mod_lat_guspe <- mgcv::gam(data = finalG %>% 
                           mutate(cell = as.factor(cell)) %>% 
                           filter(mig_cell == T),
                         log(vGrMag) ~  
                           s(cell_lat, bs = "tp") + 
                           s(cell, bs = "re") + 
                           s(year, bs = "re"))

svg(glue("figures/Fig2/lat_guspe_gam.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lat_guspe, view = "cell_lat") 

dev.off()

# lag
mod_lat_lag <- mgcv::gam(data = final %>%
                           mutate(cell = as.factor(cell),
                                  species = as.factor(species)) %>% 
                           filter (mig_cell == T),
                         AnomLag ~
                           s(cell_lat, bs = "tp") + 
                           s(year, bs = "re") + 
                           s(cell, bs = "re") + 
                           s(species, bs = "re"))

svg(glue("figures/Fig2/lat_lag_gam.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lat_lag, view = "cell_lat") 

dev.off()


# transformed axis:
a <- c(12.18249,15,
       20.08554,25,
       33.11545,
       50,54.59815)
b <- log(c(12.18249,15,
           20.08554,25,
           33.11545,
           50,54.59815))
tab <- as.data.frame(cbind(a,b))

svg(glue("figures/mod_lat_bs_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
    limits = c(2.3,4.2), 
    breaks = log(c(12.18249,15,
                   20.08554,25,
                   33.11545,50,
                   54.59815)), 
    labels = c(".",15,
               ".", 25,
               ".", 50, 
               ".")) 
dev.off()




# transformed axis:
exp(c(3,3.5,4,4.5,5,5.5))
a <- c(25,20.08554,
       33.11545, 50,
       54.59815, 75,
       90.01713, 100,
       148.41316, 150,
       244.69193, 250)
b <- log(c(25,20.08554,
           33.11545, 50,
           54.59815, 75,
           90.01713, 100,
           148.41316, 150,
           244.69193, 250))
tab <- as.data.frame(cbind(a,b))
svg(glue("figures/mod_lat_gs_axis.svg"), 
    width = 4, height = 4)

ggplot(data = tab, aes(b,b)) +
  geom_point()  +
  scale_y_continuous(#trans='log10', 
    limits = c(2.95,5.6), 
    breaks = log(c(25,20.08554,
                   33.11545, 50,
                   54.59815, 75,
                   90.01713, 100,
                   148.41316, 150,
                   244.69193, 250)), 
    labels = c(25,".",
               ".", 50,
               ".", 75,
               ".", 100,
               ".", 150,
               ".", 250)) 
dev.off()




# Figure 3 - boxplots of latitudes ---------------------------------------
pan2b_tab <- final %>% dplyr::select(cell_lat, AnomDArr, AnomDGr, AnomVArr, species) %>% 
  mutate(cell_cat = case_when(cell_lat < -10 ~ -12.5,
                              -10 <= cell_lat & cell_lat < -5 ~ -7.5,
                              -5 <= cell_lat & cell_lat < 0 ~ -2.5,
                              0 <= cell_lat & cell_lat < 5 ~ 2.5,
                              5 <= cell_lat ~ 7.5),
         cell_cat = as.factor(cell_cat))

vars2 <- pan2b_tab %>% 
  group_by(cell_cat) %>% 
  summarise(sdADAr = sd(AnomDArr, na.rm = T),
            sdADGr =sd(AnomDGr, na.rm = T),
            sdVAr = sd(AnomVArr, na.rm = T))

pan2b_ba <- ggplot(data = pan2b_tab %>% filter(!is.na(cell_cat))) +
  geom_jitter(aes(cell_cat, AnomDArr), alpha = 0.08, col = "orange") +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  geom_boxplot(aes(cell_cat, AnomDArr), alpha = 1, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Anomaly in bird arrival date", x = "Latitude (degrees)\n") +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
  coord_flip() + ylim(-30,30)

pan2b_ga <- ggplot(data = pan2b_tab %>% filter(!is.na(cell_cat))) +
  geom_jitter(aes(cell_cat, AnomDGr), alpha = 0.1, col = "orange") +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  geom_boxplot(aes(cell_cat, AnomDGr), alpha = 0.6, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        #axis.text.x=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Anomaly in green-up arrival date", x = "Latitude (degrees)\n") +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
  coord_flip() #+ ylim(-30,30)

pan2b_bs <- ggplot(data = pan2b_tab %>% filter(!is.na(cell_cat))) +
  geom_jitter(aes(cell_cat, AnomVArr), alpha = 0.1, col = "orange") +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  geom_boxplot(aes(cell_cat, AnomVArr), alpha = 0.6, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Anomaly in bird speed", x = "Latitude (degrees)\n") +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
  coord_flip() #+ ylim(-30,30)

svg(glue("figures/fig3/fig3.svg"), 
    width = 4, height = 7)
egg::ggarrange(pan2b_ba, pan2b_ga, pan2b_bs, ncol = 1)
dev.off()

# Figure 5 - Green-up effect on bird speed (model X) --------------------
mod_gu <- mgcv::gam(data = final %>% mutate(species = as.factor(species), 
                                            cell = as.factor(cell),
                                            #mig_cell = abs(mig_cell - 1),
                                            mig_cell = as.factor(mig_cell)
                                            ), 
                     log(vArrMag) ~ (AnomDGr + AnomVGr) * mig_cell + 
                       s(species, bs = "re") + 
                       s(year, bs = "re") + 
                       s(cell_lat, bs = "tp") +
                       s(cell, bs = "re")) 

summary(mod_gu)

# PLOTS
plot_smooth(mod_gu, view = "mig_cell") # mig_cel as numeric

## effect of green-up date on bird speed plot ------------------------
svg(glue("figures/Fig5/mod_gu_AnomDGr.svg"), 
    width = 4, height = 4)

plot_smooth(mod_gu, view = "AnomDGr", cond=list(mig_cell=F), 
            ylim = exp(c(3.65,4.3)), rug = F, transform = "exp", log = "y",
            ylab = "Bird speed (km/day, log scale)", xlab = "Anomaly on green-up date (days)",
            col = "orchid", lwd = 2) 

plot_smooth(mod_gu, view = "AnomDGr", cond=list(mig_cell=T), add = T, 
            rug = F,lty = "dashed", transform = "exp", log = "y",
            col = "darkgreen", lwd = 2) 

dev.off()

## effect of green-up speed on bird speed plot ------------------------
svg(glue("figures/Fig5/mod_gu_AnomVGr.svg"), 
    width = 4, height = 4)

plot_smooth(mod_gu, view = "AnomVGr", cond=list(mig_cell=T), 
            lty = "dashed", rug = F, transform = "exp", log = "y",
            ylab = NA, xlab = "Anomaly on green-up speed (km/day)",
            col = "darkgreen", lwd = 2) 

plot_smooth(mod_gu, view = "AnomVGr", cond=list(mig_cell=F), 
            add = T, rug = F, transform = "exp", log = "y",
            col = "orchid", lwd = 2) 

dev.off()

## effect of migration or breeding route on bird speed plot ------------------------
newdata <- data.frame(AnomDGr = rep(0,2),
                      AnomVGr = rep(0,2), # mean(final$AnomVGr, na.rm = T),
                      mig_cell = as.factor(c(T,F)))

newdata <- newdata %>% distinct()
newdata$species <- 1
newdata$year <- 1
newdata$cell_lat <- 1
newdata$cell <- 1

(yhat.incgu <- predict(mod_gu, newdata = newdata, se.fit = TRUE, iterms.type=2))#, re.form=NA)

mig_efftab <- as.data.frame(matrix(ncol = 3,
                                   data = c(yhat.incgu$fit, 
                                            yhat.incgu$fit + yhat.incgu$se.fit,
                                            yhat.incgu$fit - yhat.incgu$se.fit),
                                   byrow = F)) 

colnames(mig_efftab) <- c("mean", "up","low")
mig_efftab$ran <- c("mig", "bree")

svg(glue("figures/Fig5/mod_gu_migcell.svg"), 
    width = 3, height = 2.8)

ggplot(aes(x = ran, y = mean), data = mig_efftab) +
  geom_errorbar(aes(ymin=low, ymax=up, color = factor(ran)), width=.1,
                position=position_dodge(.9), data = mig_efftab) +
  geom_point(aes(x = ran, y = mean, col = ran), data = mig_efftab, size = 3) +
  scale_color_manual(values = c("mig" = "darkgreen", "bree" = "orchid")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.35),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle=90, hjust=0.5, size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        legend.position = "none",
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.35),
        rect = element_rect(fill = "transparent")) +
  scale_y_continuous(trans='log10', limits = c(3.88,4.17), 
                     breaks = log(c(40,50,55,60,65)), labels = c(40,50,55,60,65)) +
  scale_x_discrete(labels = c("Breeding\nrange","Migratory\nrange")) 

dev.off()

# Figure 6 - boxplots with lag and latitude -----------------------------------------
lag_tab <- final %>% dplyr::select(cell_lat, lag, cell_lat2) %>% 
  mutate(cell_cat = case_when(cell_lat < -10 ~ -12.5,
                              -10 <= cell_lat & cell_lat < -5 ~ -7.5,
                              -5 <= cell_lat & cell_lat < 0 ~ -2.5,
                              0 <= cell_lat & cell_lat < 5 ~ 2.5,
                              5 <= cell_lat ~ 7.5),
         cell_cat = as.factor(cell_cat))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == -12.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == -7.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))


sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == -2.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == 2.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

sort(unique(as.numeric(lag_tab %>% 
                         filter(cell_cat == 7.5) %>% 
                         dplyr::select(cell_lat2) %>%
                         pull())))

lag_tab %>% dplyr::select(cell_cat, lag) %>% filter(cell_cat == 7.5) %>% 
  mutate(Var = var(lag, na.rm = T))

lag_tab %>% dplyr::select(cell_cat, lag) %>% filter(cell_cat == -12.5) %>% 
  mutate(Var = var(lag, na.rm = T))

(lag1 <- ggplot(data = lag_tab %>% filter(!is.na(cell_cat))) +
    geom_jitter(aes(cell_cat, lag), alpha = 0.08, col = "orange") +
    geom_hline(yintercept = 0, 
               color = "black", size=0.7) +
    #geom_violin(aes(cell_cat, AnomLag), alpha = 0.6, width = 1) +
    geom_boxplot(aes(cell_cat, lag), alpha = 0.6, width = 0.25) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 10),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          #axis.text.y=element_blank(),
          legend.title.align = 0.5,
          plot.title = element_text(hjust = 0.5, size=10)) +
    labs(y = "Lag (days)", x = "Latitude (degrees)\n") +
    scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                     labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
    coord_flip() #+ ylim(-30,30)
)

lagA_tab <- final %>% dplyr::select(cell_lat, AnomLag, cell_lat2) %>% 
  mutate(cell_cat = case_when(cell_lat < -10 ~ -12.5,
                              -10 <= cell_lat & cell_lat < -5 ~ -7.5,
                              -5 <= cell_lat & cell_lat < 0 ~ -2.5,
                              0 <= cell_lat & cell_lat < 5 ~ 2.5,
                              5 <= cell_lat ~ 7.5),
         cell_cat = as.factor(cell_cat))

lagA_tab %>% dplyr::select(cell_cat, AnomLag) %>% filter(cell_cat == 7.5) %>% 
  mutate(Var = var(AnomLag, na.rm = T))

lagA_tab %>% dplyr::select(cell_cat, AnomLag) %>% filter(cell_cat == -12.5) %>% 
  mutate(Var = var(AnomLag, na.rm = T))

lag2 <- ggplot(data = lagA_tab %>% filter(!is.na(cell_cat))) +
  geom_jitter(aes(cell_cat, AnomLag), alpha = 0.08, col = "orange") +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  #geom_violin(aes(cell_cat, AnomLag), alpha = 0.6, width = 1) +
  geom_boxplot(aes(cell_cat, AnomLag), alpha = 0.6, width = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(y = "Anomaly on lag (days)"#, x = "Latitude (degrees)"
  ) +
  scale_x_discrete(breaks = c(-12.5, -7.5, -2.5, 2.5, 7.5),
                   labels = c("[25,30)","[30,-5)","[35,40)","[40,45)","[45.50)")) +
  coord_flip() #+ ylim(-30,30)

svg(glue("figures/fig6/fig6.svg"), 
    width = 4, height = 5)
egg::ggarrange(lag1, lag2, ncol = 1)
dev.off()

# Figure 7 - Anomaly on lag and z-score (model X) ------------------------------------
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
                       s(species, bs = "re")) 

summary(mod_lag)

draw(mod_lag)

# PLOTS
## bird past speed plot -----------------
svg(glue("figures/Fig7/mod_lag_past_spe_z.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lag, view = "past_spe_z", 
            ylim = c(-15,10), xlab = "Bird past speed (z-score)",
            ylab = "Anomaly on Lag") 

dev.off()

## bird first arrival date plot -----------------
svg(glue("figures/Fig7/mod_lag_ea_lat_yr_z.svg"), 
    width = 4, height = 4)

plot_smooth(mod_lag, view = "ea_lat_yr_z", 
            ylim = c(-15,10), xlab = "Bird first arrival date (z-score)") 

dev.off()

## green-up date plot ---------------------
svg(glue("figures/Fig7/mod_lag_gr_mn_z.svg"), 
    width = 4, height = 4)
plot_smooth(mod_lag, view = "gr_mn_z", 
            ylim = c(-15,10), xlab = "Green-up date (z-score)")

dev.off()


# Figure 4 - individual species speeds ----------------------------------
## species speeds histogram -------------------------
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

ggplot(final, aes(x = log(vArrMag))) +
  geom_histogram(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  theme_bw() +
  scale_x_continuous(trans='log10', 
                     breaks = log(c(10,50,250,1000)), 
                     labels = c(10,50,250,1000)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

### sensitivity squares -------------------------
ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = xi_mean), width=0.5, alpha = 0.7) +
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
  scale_fill_viridis(name = "Sensitivity", option = "G") +
  labs(y = "Log(Speed)\n") 

### first arrival date squares ------------------------
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

### diet squares ---------------------------
ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = Diet), width=0.5, alpha = 0.7) +
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
  labs(y = "Log(Speed)\n") 

### body mass squares ---------------------------
ggplot(data = final %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = Body_mass_g), width=0.5, alpha = 0.7) +
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
  scale_fill_viridis(name = "Body mass (g)", option = "B") +
  labs(y = "Log(Speed)\n") 

### wintering latitude squares ------------------------
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

### Migration distance squares ------------------------
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

## bird speed and traits (model) --------------------
mod_tra <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ xi_mean + ea_lat + Diet + Body_mass_g + winlat + Time + 
                       s(cell_lat, bs = "tp") + s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
summary(mod_tra)

# PLOTS:
### ea_lat ----------------------------------------------------
svg(glue("figures/Fig4/mod_tra_ea_lat.svg"), 
    width = 4, height = 4.3)

plot_smooth(mod_tra, view = "ea_lat",
            rug = F, transform = "exp", log = "y",
            ylab = "Bird speed (km/day, log scale)", xlab = "First arrival date (days)",
            lwd = 2)

dev.off()
### effect of diet on migration speed -----------------------------
newdata <- data.frame(xi_mean = rep(mean(final$xi_mean, na.rm = T),4),
                      ea_lat = rep(mean(final$ea_lat, na.rm = T),4),
                      Body_mass_g = rep(mean(final$Body_mass_g, na.rm = T),4),
                      winlat = rep(mean(final$winlat, na.rm = T),4),
                      Diet = unique(final$Diet), 
                      Time = rep(c("nocturnal"),4))

newdata$species <- 1
newdata$year <- 1
newdata$cell_lat <- 1
newdata$cell <- 1

(yhat.inctr <- predict(mod_tra, newdata = newdata, se.fit = TRUE, iterms.type=2))#, re.form=NA)

tra_efftab <- as.data.frame(matrix(ncol = 3,
                                   data = c(yhat.inctr$fit, 
                                            yhat.inctr$fit + yhat.inctr$se.fit,
                                            yhat.inctr$fit - yhat.inctr$se.fit),
                                   byrow = F)) 

colnames(tra_efftab) <- c("mean", "up","low")
tra_efftab$diet <- unique(final$Diet)

svg(glue("figures/Fig4/mod_tra_Diet.svg"), 
    width = 4.3, height = 3)

ggplot(aes(x = diet, y = mean), data = tra_efftab) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin=low, ymax=up), width=.1,
                position=position_dodge(.9), data = tra_efftab) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.35),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle=90, hjust=0.5, size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        legend.position = "none",
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.35),
        rect = element_rect(fill = "transparent")) +
  scale_y_continuous(trans='log10', 
                     limits = c(3.4,4.4), 
                     breaks = log(c(30, 40, 50, 60, 70, 80)), labels = c(30, 40, 50, 60, 70, 80)) +
  labs(y = "Bird speed (km/day, log scale)")

dev.off()

### time of migration effect on speed -----------------------
newdata <- data.frame(xi_mean = rep(mean(final$xi_mean, na.rm = T),2),
                      ea_lat = rep(mean(final$ea_lat, na.rm = T),2),
                      Body_mass_g = rep(mean(final$Body_mass_g, na.rm = T),2),
                      winlat = rep(mean(final$winlat, na.rm = T),2),
                      Diet = rep(c("Insectivore"),2), 
                      Time = na.omit(unique(final$Time)))

newdata$species <- 1
newdata$year <- 1
newdata$cell_lat <- 1
newdata$cell <- 1

(yhat.inctr2 <- predict(mod_tra, newdata = newdata, se.fit = TRUE, iterms.type=2))#, re.form=NA)

time_efftab <- as.data.frame(matrix(ncol = 3,
                                    data = c(yhat.inctr2$fit, 
                                             yhat.inctr2$fit + yhat.inctr2$se.fit,
                                             yhat.inctr2$fit - yhat.inctr2$se.fit),
                                    byrow = F)) 

colnames(time_efftab) <- c("mean", "up","low")
time_efftab$time <- na.omit(unique(final$Time))

svg(glue("figures/Fig4/mod_tra_Time.svg"), 
    width = 3.5, height = 3)

ggplot(aes(x = time, y = mean), data = time_efftab) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin=low, ymax=up), width=.1,
                position=position_dodge(.9), data = time_efftab) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.35),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle=90, hjust=0.5, size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        legend.position = "none",
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.35),
        rect = element_rect(fill = "transparent")) +
  scale_y_continuous(#trans='log10', 
    limits = c(3.85,4.4), 
    breaks = log(c(50, 60, 70, 80)),
    labels = c(50, 60, 70, 80)) +
  labs(y = "Bird speed (km/day, log scale)") +
  scale_x_discrete(labels = c("Diurnal","Nocturnal")) 

dev.off()






