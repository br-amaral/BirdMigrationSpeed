#? *********************************************************************************
#? -------------------------------   3_ModelPlots.R  -------------------------------
#? *********************************************************************************
#
# Code to analyze how bird arrival date speed is affected by green-up, bird traits and latitude,
#    which factors contribute to anomalies in lag, and how green-up varies according to latitude
#    models and plotting outputs

#! Source ---------------------------------------------
#           - code/5_Maps.R:
#
#! Input ----------------------------------------------
#           - data/final.rds : tibble with bird and green-up information, plus species traits
#           - data/cellcoor.rds : tibble with XY coordinates of each cell, and a cell id number
#           - data/cellnumbs.rds : tibble with cell numbers that start at 1
#           - data/velocityB.rds : tibble with calculated bird speeds
#
#! Output ----------------------------------------------
#           - data/res/mod_gu.rds : model one results
#           - data/res/mod_lat_guspe.rds : model two results
#           - data/res/mod_tra.rds : model three results
#           - data/res/mod_lag.rds : model four results
#
#! Figures ---------------------------------------------
#           - figure 1 - maps of migration speed
#           - figure 2 - individual species speeds
#           - figure 3a - speed and green-up date (model 1)
#           - figure 3b - speed and green-up speed (model 1)
#           - figure 3c - speed and range type (model 1)
#           - figure 4a - speed and latitude (model 1 and 2)
#           - figure 4b - anomaly on lag and z-score (model 4)
#           - figure 5 - first arrival and bird speed (model 3)
#           - figure S3 - histogram of migration direction

# Color-blind friendly colors:
# Green-up: #009E73 (green)
# Bird: #CC79A7 (pink)
# Migratory range: #E69F00 (orange)
# Breeding range: #F0E442 (yellow)

# detach packages and clear workspace
if(!require(freshr)){install.packages('freshr')}
freshr::freshr()

#! Load packages ---------------------------------------
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
#
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(scales::alpha)
# conflicts_prefer(scales::alpha)

#! Source code -----------------------------------------
# Figure 1 - maps of migration speed -------------------
# maps are made sourcing script 5_Maps_230125.R
source("code/5_Maps.R")

#! Make functions --------------------------------------
colanmes <- colnames
lenght <- length
`%!in%` <- Negate(`%in%`)

#! Import data -----------------------------------------
## file paths
CELL_NUMB_PATH <- "data/cellnumbs.rds"
CELL_COOR_PATH <- "data/cellcoor.rds"
FINAL_DATA_PATH <- "data/final.rds"
VELB_PATH <- "data/velocityB.rds"

## read files
cellnumbs <- readRDS(file = CELL_NUMB_PATH)
cells <- readRDS(file = CELL_COOR_PATH)

final2 <- readRDS(FINAL_DATA_PATH) %>% 
  mutate( species = as.factor(species), 
          cell = as.factor(cell),
          mig_cell = as.factor(mig_cell),
          sps_cell = as.factor(glue("{species}_{cell}"))
  )

finalG <- final2 %>% 
  dplyr::select(cell, year, cell_lat2, vGrMag, gr_mn, mig_cell, breed_cell) %>% 
  distinct() %>% 
  mutate( cell = as.factor(cell),
          mig_cell = as.factor(mig_cell))

# Model 1 - bird speed varying with green-up --------------------------------------------------------
mod_gu <- 
  mgcv::gam(data = final2 %>% mutate(species = as.factor(species)), 
            log(vArrMag) ~ (AnomDGr + AnomVGr) * mig_cell + 
              s(year, bs = "re") + 
              s(cell_lat2, bs = "tp") + 
              s(sps_cell, bs = "re") + 
              s(species, bs = "re")
  ) 

summary(mod_gu)

# mod_gu <- readRDS(file = "data/res/mod_gu.rds")
# write_rds(mod_gu, file = "data/res/mod_gu.rds")

## Figure 3a - speed and green-up date (model 1) --------------------------------------------------------
svglite::svglite(glue("figures/Fig2/fig2_date.svg"), 
    width = 4, height = 4)

plot_smooth(mod_gu, view = "AnomDGr", cond=list(mig_cell=F), 
            ylim = exp(c(3.7,4.6)), 
            rug = F, transform = "exp", log = "y",
            ylab = "Bird speed (km/day, log scale)", 
            xlab = "Anomaly on green-up date (days)",
            col = "#F0E442", lwd = 3, rm.ranef = TRUE) 

plot_smooth(mod_gu, view = "AnomDGr", cond=list(mig_cell=T), add = T, 
            rug = F,lty = "dashed", transform = "exp", log = "y",
            col = "#E69F00", lwd = 3, rm.ranef = TRUE) 

dev.off()

## Figure 3b - speed and green-up speed (model 1) --------------------------------------------------------
svglite::svglite(glue("figures/Fig2/fig2_speed.svg"), 
    width = 4, height = 4)

plot_smooth(mod_gu, view = "AnomVGr", cond=list(mig_cell=T), 
            lty = "dashed", 
            rug = F, transform = "exp", log = "y",
            ylab = NA, xlab = "Anomaly on green-up speed (km/day)",
            col = "#E69F00", lwd = 3, ylim = exp(c(3.7, 4.6)), 
            rm.ranef = TRUE)

plot_smooth(mod_gu, view = "AnomVGr", cond=list(mig_cell=F), 
            add = T, rug = F, transform = "exp", log = "y",
            col = "#F0E442", lwd = 3, rm.ranef = TRUE) 

dev.off()

### latitude on birds speed (mig and breeding) -------------------
plot_smooth(mod_gu, view = "cell_lat2", cond=list(mig_cell=F), 
            ylim = exp(c(3.2,4.7)), 
            xlim = c(30,46),
            rug = F, transform = "exp", log = "y",
            ylab = "Bird speed (km/day, log scale)", xlab = "Latitude",
            col = "#F0E442", lwd = 3, rm.ranef = TRUE) 

plot_smooth(mod_gu, view = "cell_lat2", cond=list(mig_cell=T), add = T, 
            rug = F,lty = "dashed", transform = "exp", log = "y",
            col = "#E69F00", lwd = 3, rm.ranef = TRUE) 
legend(35, 20, legend=c("Breeding cell", "Migratory cell"),
        col=c("#F0E442", "#E69F00"), lty = 1, cex=1, lwd = 2)

### range on bird speed
newdata <- data.frame(AnomDGr = rep(0,2),
                      AnomVGr = rep(0,2), # mean(final$AnomVGr, na.rm = T),
                      mig_cell = as.factor(c(T,F)))

newdata <- newdata %>% distinct()
newdata$species <- 1
newdata$year <- 1
newdata$cell_lat2 <- 1
newdata$cell <- 1
newdata$sps_cell <- 1

yhat.incgu <- predict(mod_gu,
                      newdata = newdata,
                      se.fit = TRUE, 
                      iterms.type=2, 
                      re.form=NA,
                      exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)"))

mig_efftab <- as.data.frame(matrix(ncol = 3,
                                  data = c(yhat.incgu$fit, 
                                            yhat.incgu$fit + yhat.incgu$se.fit,
                                            yhat.incgu$fit - yhat.incgu$se.fit),
                                  byrow = F)) 

colnames(mig_efftab) <- c("mean", "up","low")
mig_efftab$ran <- c("mig", "bree")

mig_efftabx <- mig_efftab %>% 
  mutate(mean = exp(mean),
        up = exp(up),
        low = exp(low))

## Figure 3c - speed and range type (model 1) --------------------------------------------------------
svglite::svglite(glue("figures/Fig2/fig2_range.svg"), 
    width = 3, height = 2.8)

ggplot(aes(x = ran, y = mean), data = mig_efftab) +
  geom_errorbar(aes(ymin=low, ymax=up, color = factor(ran)), width=.1,
                position=position_dodge(.9), data = mig_efftab) +
  geom_point(aes(x = ran, y = mean, col = ran), data = mig_efftab, size = 2) +
  scale_color_manual(values = c("mig" = "#E69F00", "bree" = "#F0E442")) +
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
  scale_y_continuous(trans='log10', limits = c(3.7, 4.6), 
                      breaks = log(c(40,50,60,70,80,90)), labels = c(40,50,60,70,80,90)) +
  scale_x_discrete(labels = c("Breeding\nrange","Migratory\nrange")) 

dev.off()

# Model 2 - Green-up speed varying according to latitude --------------------------------------------------------
cellnumbs2 <- cellnumbs %>% 
  as_tibble() %>% 
  mutate(cell2 = as.factor(cell2))
colnames(cellnumbs2) <- c("cell2", "cell")
finalG <- left_join(finalG, cellnumbs2)
cells2 <- cells[,1:2]
colnames(cells2) <- c("cell", "cell_lat2")
cells2 <- cells2 %>% 
  mutate(cell = as.factor(cell))
finalG <- left_join(finalG, cells2[,1:2])

mod_lat_guspe <- 
  mgcv::gam(data = finalG,
            log(vGrMag) ~ 
              s(cell_lat2, bs = "tp") + 
              s(cell, bs = "re") +
              s(year, bs = "re")
  )

# mod_lat_guspe <- readRDS(file = "data/res/mod_lat_guspe.rds")
# write_rds(mod_lat_guspe, file = "data/res/mod_lat_guspe.rds")

summary(mod_lat_guspe)

## Figure 4a - speed and latitude (model 1 and 2) --------------------------------------------------------
svglite::svglite(glue("figures/Fig4/fig4a_both.svg"), 
    width = 4.2, height = 3.9)
plot_smooth(mod_lat_guspe, view = "cell_lat2",
            rug = F, transform = "exp", log = "y",
            #rm.ranef = T,
            #add = T,
            col = "#009E73", 
            #ylab = "Green-up speed (km/day, log-scale)",
            ylab = "Speed (km/day, log-scale)",
            xlab = "Latitude (degrees)",
            lwd = 2,
            ylim = c(15,305),
            xlim = c(30,48),
            xaxt='n', 
            rm.ranef = TRUE)
# define a custom axis
axis(side = 1, at = c(30, 35, 40, 45, 50))

plot_smooth(mod_gu, view = "cell_lat2", 
            transform = "exp", log = "y",
            rug = F, 
            #rm.ranef = TRUE,
            col = "#CC79A7", 
            ylab = "Bird migratory speed (km/day, log-scale)",
            xlab = "Latitude (degrees)",
            lwd = 2,
            add = T,
            xlim = c(30,48),
            xaxt='n', 
            rm.ranef = TRUE)
legend(42.5, 295, legend=c("Green-up", "Bird"),
        col=c("#009E73", "#CC79A7"), lty = 1, cex=0.6, lwd = 1.5)
dev.off()

# Model 3 - Species traits --------------------------------------------------------
hwicol <- read_csv("data/source/traits/birds_HWI.csv") %>% 
  dplyr::select(species, `HWI`)  ## used the IUCN names
final4 <- final2 %>% left_join(hwicol, by = "species") %>% 
  mutate(species = as.factor(species),
          xi_mean = as.numeric(scale(xi_mean)),    # sensitivity
          ea_lat2 = ea_lat,
          ea_lat = as.numeric(scale(ea_lat)),
          Body_mass_g = as.numeric(scale(Body_mass_g)),
          winlat = as.numeric(scale(winlat)),
          HWI = as.numeric(scale(HWI))) # as.numeric to not mess up the predictions!

mod_tra <-
  mgcv::gam(data = final4,
            log(vArrMag) ~ xi_mean + 
              ea_lat + 
              Body_mass_g + 
              winlat + 
              HWI +
              s(year, bs = "re") + 
              s(cell_lat2, bs = "tp") +   
              s(sps_cell, bs = "re") + 
              s(species, bs = "re")
  )

# mod_tra <- readRDS(file = "data/res/mod_tra.rds")
# write_rds(mod_tra, file = "data/res/mod_tra.rds")

summary(mod_tra)

### First arrival date plot ---------------------------------------------------------------------------------------------------------
# fit lme4 model for plot 
t2 <- lme4::lmer(log(vArrMag) ~ xi_mean + 
                  ea_lat + 
                  Body_mass_g + 
                  winlat + 
                  HWI +
                  cell_lat2 +
                  (1 | species) +
                  (1 | sps_cell) +
                  (1 | year), data = final4)

summary(t2)

# data for prediction 
newdata_d <- data.frame(xi_mean = rep(mean(final4$xi_mean, na.rm = T),3),
                        ea_lat = c(final4$ea_lat %>% min(na.rm = T),
                                    final4$ea_lat %>% mean(na.rm = T),
                                    final4$ea_lat %>% max(na.rm = T)) %>% 
                          as.numeric(),
                        Body_mass_g = rep(mean(final4$Body_mass_g, na.rm = T),3),
                        winlat = rep(mean(final4$winlat, na.rm = T),3),
                        HWI = rep(mean(final4$HWI, na.rm = T),3),  
                        cell_lat2 = rep(mean(final4$cell_lat2), 3))

newdata_d$species <- 1
newdata_d$year <- 1
newdata_d$sps_cell <- 1

predict(t2, newdata = newdata_d, re.form = NA, se.fit = TRUE)

(p2 <- jtools::predict_merMod(t2, 
                              newdata = newdata_d, 
                              se.fit = TRUE, 
                              iterms.type=2, 
                              exclude = list("(1|year)","(1|cell_lat)","(1|sps_cell)"),
                              re.form = NA
))

ea_lat_tab <- as.data.frame(matrix(ncol = 3,
                                    data = c(exp(p2$fit), 
                                            exp(p2$fit + (p2$se.fit*1.96)),
                                            exp(p2$fit - p2$se.fit*1.96)),
                                    byrow = F)) 

colnames(ea_lat_tab) <- c("mean", "up","low")
ea_lat_tab$ea_lat <- newdata_d$ea_lat
ea_lat_tab$ea_lat2 <- ea_lat_tab$ea_lat * sd(final4$ea_lat2, na.rm = T) + mean(final4$ea_lat2, na.rm = T)
ea_lat_tab$date <- NA

ea_lat_tab <- as_tibble(ea_lat_tab)

date2 <- as.POSIXlt("2009-02-10")

ea_lat_tab <- ea_lat_tab %>% 
  mutate(date = update(date2, year=2016, yday=ea_lat2 %>% round()) %>% 
            as.Date(format = "%b %d") )
as.Date(ea_lat_tab$date, "%d")
vars <- format(as.Date(ea_lat_tab$date,format="%Y-%m-%d"), format = "%d") 
vis <- month.abb[as.numeric(format(as.Date(ea_lat_tab$date,format="%Y-%m-%d"), format = "%m"))]

ea_lat_tab$date2 <- format(paste(vars, vis, sep=" "), format = "%d %b")
format(ea_lat_tab$date2, format = "%d %b")

date2[1] <- update(date2, year=2016, yday=round(ea_lat_tab$ea_lat2[1],0))

base::as.Date("May 12, 2017", format = "%B %d, %Y")

## Figure 5 - first arrival and bird speed (model 3) --------------------------------------------------------

svglite::svglite(glue("figures/Fig5/fig5.svg"), 
                  width = 4.6, height = 4.2)

ggplot(aes(x = date, y = mean), data = ea_lat_tab) +
  geom_ribbon(aes(ymin=low, ymax=up), fill = "#CC79A7", alpha = 0.2,
              position=position_dodge(.9), data = ea_lat_tab) +
  geom_line(size = 1, col = "#CC79A7") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.35),
        panel.border = element_blank(),
        panel.background = element_blank(),
        #axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        axis.text.y = element_text(angle=90, hjust=0.5, size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 11, colour = "black"),
        legend.position = "none",
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks = element_line(colour = "black", size = 0.35),
        rect = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(#trans='log10', 
                      limits = c(5, 87),
                      breaks = c(10,20,30,40,50,60,70,80), labels = c(10,20,30,40,50,60,70,80)
                      ) +
  scale_x_date(#trans='log10', 
    limits = as.Date(c("2016-03-15", "2016-05-5"),format="%Y-%m-%d")
  ) +
  labs(y = "Bird speed (km/day, log scale) \n", x = "\n Calendar date") +
  ggtitle("First arrival date")

dev.off()

## Figure 2 - individual species speeds ----------------------------------
### species speeds histogram -------------------------
ggplot(data = final4 %>% filter(species != "Pipilo_erythrophthalmus")) + # no estimates, no neighbors
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

ggplot(final2, aes(x = log(vArrMag))) +
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

### sensitivity squares ------------------------------------------------------------------------------------------------
ggplot(data = final2 %>% filter(species != "Pipilo_erythrophthalmus")) +
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

### first arrival date squares -----------------------------------------------------------------------------------------------
ggplot(data = final2 %>% filter(species != "Pipilo_erythrophthalmus")) +
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

### diet squares --------------------------------------------------------------------------------------------------
ggplot(data = final2 %>% filter(species != "Pipilo_erythrophthalmus")) +
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

### body mass squares --------------------------------------------------------------------------------------------------
ggplot(data = final2 %>% filter(species != "Pipilo_erythrophthalmus")) +
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

### wintering latitude squares -----------------------------------------------------------------------------------------------
ggplot(data = final2 %>% filter(species != "Pipilo_erythrophthalmus")) +
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

### Migration distance squares -----------------------------------------------------------------------------------------------
ggplot(data = final2 %>% filter(species != "Pipilo_erythrophthalmus")) +
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

### HWI squares --------------------------------------------------------------------------------------------------
ggplot(data = final4 %>% filter(species != "Pipilo_erythrophthalmus")) +
  geom_tile(aes(x = reorder(species2,log(vArrMag),FUN = median, na.rm = TRUE), 
                y = 1, fill = HWI), width=0.5, alpha = 0.7) +
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
  scale_fill_viridis(name = "hand-wing index (HWI)", option = "B") +
  labs(y = "Log(Speed)\n") 

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
      spe_bef <- (geodist::geodist(c(place2[1,2], place2[1,1]), c(place[1,2], place[1,1]), measure = 'geodesic' )/1000)/
        (date2 - date)
      if(spe_bef > 0) {final_lag5 <- rbind(final_lag5,
                                            cbind(as.data.frame(spslist[s]), years[y], cellsLoop[d], spe_bef))} else {print("upa")}
    }
  }
}

colnames(final_lag5) <- c("species", "year", "cell", "past_spe")
final_lag5$year <- as.numeric(final_lag5$year)
final_lag5$cell <- as.numeric(final_lag5$cell)
final_lag5$past_spe <- as.numeric(final_lag5$past_spe)

final_lag <- left_join(final_lag1, final_lag5, by = c("species", "year", "cell"))
cellspec <- unique(final_lag[ ,c("cell","species")])

final3 <- matrix(NA, 0, (ncol(final_lag) + 3))

## scale past speed, green-up day and sps arrival date
for (a in 1:nrow(cellspec)){  # loop in a cell and species
  dat.temp <- subset(final_lag, cellspec[a,1] == cell & cellspec[a,2] == species)
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

mod_lag <- 
  mgcv::gam(data = final3, 
            AnomLag ~ past_spe_z + ea_lat_yr_z + gr_mn_z + 
              s(cell, bs = "re") +
              s(sps_cell, bs = "re")
  ) 

mod_lag_gu <- 
  mgcv::gam(data = final3, 
            AnomLag ~ gr_mn_z + 
              s(cell, bs = "re") +
              s(sps_cell, bs = "re")
  ) 

mod_lag_bi <- 
  mgcv::gam(data = final3, 
            AnomLag ~ past_spe_z + ea_lat_yr_z + 
              s(cell, bs = "re") +
              s(sps_cell, bs = "re")
  ) 
mod_lag_null <- 
  mgcv::gam(data = final3, 
            AnomLag ~ 1 + 
              s(cell, bs = "re") +
              s(sps_cell, bs = "re")
  ) 

AIC(mod_lag, mod_lag_gu, mod_lag_null, mod_lag_bi)
BIC(mod_lag, mod_lag_gu, mod_lag_null, mod_lag_bi)

# write_rds(mod_lag, file = "data/res/mod_lag.rds")
# mod_lag <- readRDS(file = "data/res/mod_lag.rds")

summary(mod_lag)

## Figure 4b - anomaly on lag and z-score (model 4) ------------------------------------
svglite::svglite(glue("figures/Fig3/fig3b.svg"), 
                  width = 4, height = 3.8)
# add a legend for each line on Inkscape
### bird past speed plot -----------------------------------------------------------------------
plot_smooth(mod_lag, view = "past_spe_z", 
            ylim = c(-15,10), # xlab = "Bird past speed (z-score)",
            xlab = "Z-score",
            ylab = "Anomaly on Relative Arrival", col = "#CC79A7", 
            rug = F, lwd = 2, lty = 3, 
            rm.ranef = FALSE) 

plot_smooth(mod_lag, view = "ea_lat_yr_z", 
            xlab = "Bird first arrival date (z-score)",
            add = T, rug = F, col = "#56B4E9", 
            lwd = 2, lty = 3, 
            rm.ranef = FALSE) 

plot_smooth(mod_lag, view = "gr_mn_z", 
            xlab = "Green-up date (z-score)", add = T,
            col = "#009E73", rug = F, lwd = 2, lty = 3, 
            rm.ranef = FALSE)

legend(-3, 10, legend=c("Green-up date", "Bird speed prior to arrival", "Bird first arrival date"),
        col=c("#009E73", "#CC79A7","#56B4E9"), lty = 3, cex=0.6, lwd = 2)

dev.off()

# FIGURES ---------------------------------------------------------------------------

# Supplementary materials

## Figure S3 - histogram of migration direction ----------------------------------
velocityB <- read_rds(VELB_PATH)

preds <- left_join(velocityB, cells[,1:3], by= "cell")
hist(preds$vArrAng, breaks = 30)
preds$vArrAng2 <- ifelse(preds$vArrAng > 270, preds$vArrAng - 360, preds$vArrAng)
hist(preds$vArrAng2, breaks = 30)

# regular
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
  scale_y_continuous(labels=percent, 
                      limits = c(0,0.13), 
                      breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125)) +
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

## species mean speeds
sps_meds <- final2 %>% 
  group_by(species) %>%  
  summarise(mean = mean(vArrMag, na.rm = T)) %>% 
  arrange(mean) %>% 
  filter(mean > 1)
dim(sps_meds)
sps_meds[1,]
sps_meds[29,]
sps_meds[55,]

mean(final2$vArrMag, na.rm = T)

cell_sdsD <- final2 %>% 
  group_by(cell) %>%  
  summarise(sd = sd(AnomDGr, na.rm = T)) %>% 
  arrange(sd) 
cell_sdsD[round((nrow(cell_sdsD)/2),1),]

cell_sdsV <- final2 %>% 
  group_by(cell) %>%  
  summarise(sd = sd(AnomVGr, na.rm = T)) %>% 
  arrange(sd) 
cell_sdsV[round((nrow(cell_sdsV)/2),1),]

cat("\n\n DONE!!! \n\n\n")
