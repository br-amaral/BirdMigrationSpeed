### pheno mismatch data exploration! ###

library(lme4)
library(ggforce)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(glue)
library(effects)
library(lme4)
library(lemon)
library(RColorBrewer)
library(corrplot)
library(tidyverse)

# prepare data ----------------------
## full data set or late years selection ---------------------
dat_year <- "late"

ifelse(dat_year == "late", 
       # only late years
       bg.mat <- read_rds("data/birdgreenLATE.rds") %>% 
         mutate(vArrMag_log = log(vArrMag),
                vGrMag_log = log(vGrMag),
                cell_type = ifelse(mig_cell == T & breed_cell == F, "mig",
                                   ifelse(mig_cell == F & breed_cell == T, "bree",
                                          ifelse(mig_cell == T & breed_cell == T, "both", "ops"))))%>% 
         rename(cell_lat_s = cell_lat) %>% 
         dplyr::select(-cell_lng),
       # full data set
       bg.mat <- read_rds("data/birdgreen.rds") %>% 
         mutate(vArrMag_log = log(vArrMag),
                vGrMag_log = log(vGrMag)) %>% 
         rename(cell_lat_s = cell_lat) %>% 
         dplyr::select(-cell_lng)
       )

# add cell coordinates
cells <- read_rds("data/cellcoor.rds")
bg.mat2 <- left_join(bg.mat, cells, by="cell")

bg.mat <- bg.mat2 %>% 
  mutate(cell = as.factor(cell),
         lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps"))),
         year_g = ifelse(year %in% c(2010,2012), "early", (ifelse(year %in% c(2011,2015,2017), "ave", ifelse(year %in% c(2009,2013,2014,2016), "late", "ops")))),
         fast_g = ifelse(year %in% c(2009,2010,2011,2013,2014),"fast", ifelse(year %in% c(2012,2015,2016,2017), "slow", "ops")))


## select species ----------
bg.mat %>% 
  group_by(species) %>% 
  summarise(mean_spe = mean(abs(AnomVArr), na.rm = T)) %>% 
  View()

spsvec <- c(# migration length
            "Setophaga_pinus", "Sayornis_phoebe", "Dolichonyx_oryzivorus", "Empidonax_traillii",
            # sensitivity to lat
            "Setophaga_dominica","Parkesia_motacilla","Tachycineta_bicolor","Oreothlypis_ruficapilla", 
            # variability in speed between years
            "Setophaga_discolor", "Dumetella_carolinensis", "Setophaga_pinus","Contopus_virens"
            )
table(spsvec %in% bg.mat$species)

who <- c("short", "short", "long", "long",
         "notsenlat", "notsenlat", "senlat", "senlat",
         "notvar", "notvar", "var", "var")

who_n <- c("mig", "mig", "mig", "mig",
           "senlat", "senlat", "senlat", "senlat",
           "var", "var", "var", "var")

sps_who <- as_tibble(cbind(spsvec, who, who_n)) %>% 
  rename(species = spsvec)
sps_who$who_n <- factor(sps_who$who_n, levels = c("mig","senlat","var" ))

bg.mat <- left_join(bg.mat, sps_who, by = "species")

## green up data -------------
bg.matG <- bg.mat %>% 
  dplyr::select(year, cell, cell_lat, cell_lng, cell_lat_s, gr_mn, gr_ncell,
                NGr, vGrMag, vGrAng, angG, xG, yG, vGrMag_log,
                AnomVGr, AnomDGr, AnomLag) %>% 
  mutate(cell = as.factor(cell),
         lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps"))), 
         year_g = ifelse(year %in% c(2010,2012), "early", (ifelse(year %in% c(2011,2015,2017), "ave", ifelse(year %in% c(2009,2013,2014,2016), "late", "ops")))),
         fast_g = ifelse(year %in% c(2009,2010,2011,2013,2014),"fast", ifelse(year %in% c(2012,2015,2016,2017), "slow", "ops"))) %>% 
  unique()

bg.matG %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vGrMag_log)) +
  geom_smooth(aes(x = cell_lat, y = vGrMag_log),
              method = "lm") +
  theme_bw()+
  facet_rep_wrap(~year, scales = "free_y")

bg.matG %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = vGrMag), #method = "lm", 
             se = F, col = "darkolivegreen")  +
  geom_smooth(aes(x = cell_lat, y = vArrMag), data = bg.mat,
              #method = "lm", 
              se = F, color = "gray") +
  theme_bw()+
  facet_rep_wrap(~year, scales = "free_y")


bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = AnomVArr), #method = "lm", 
              se = F, col = "gray") +
  theme_bw()+
  facet_rep_wrap(~year, scales = "free_y")

#
g <- bg.mat %>% 
  group_by(cell) %>% 
  mutate(medABV = median(AnomVArr, na.rm = T),
         medBV = median(vArrMag, na.rm = T)) %>% 
  ungroup()

h <- bg.matG %>% 
  group_by(cell) %>% 
  mutate(medAGV = median(AnomVGr, na.rm = T),
         medGV = median(vGrMag, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::select(cell, year, medGV, medAGV)
g <- left_join(g, h, by = c("cell", "year"))

ggplot(g) +
  geom_point(aes(x = medGV, y = medBV)) + theme_bw()

ggplot(g) +
  geom_point(aes(x = medAGV, y = medABV)) + theme_bw()

# arrival dates ----------
# lat versus day for GU and B's mean - and some selected species
unique(bg.mat$who_n)
bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", 
              se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(who_n == "mig"),
              aes(x = cell_lat, y = arr_GAM_mean, colour = who, group = species), 
              method = "lm",
              se = F) +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +
  ylab("date") + xlab("Latitude") + ggtitle("Bird date")


bg.mat %>% 
  ggplot() +
  geom_boxplot(data = bg.mat %>% filter(who_n == "mig"),
              aes(x = who, y = vArrMag))  + coord_cartesian(ylim = c(0, 1000))

bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", 
              se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(who_n == "senlat"),
              aes(x = cell_lat, y = arr_GAM_mean, colour = who, group = species), 
              method = "lm",
              se = F) +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +
  ylab("date") + xlab("Latitude") + ggtitle("Bird date") #+ facet_rep_wrap(~year)

bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", 
              se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(who_n == "var"),
              aes(x = cell_lat, y = arr_GAM_mean, colour = who, group = species), 
              method = "lm",
              se = F) +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +
  ylab("date") + xlab("Latitude") + ggtitle("Bird date") #+ facet_rep_wrap(~year)


bg.mat %>% 
  ggplot() +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(who_n == "var"),
              aes(x = cell_lat, y = arr_GAM_mean, colour = species, group = species), 
              method = "lm",
              se = F) +
  geom_smooth(data = bg.mat %>% filter(who_n == "var"),
              aes(x = cell_lat, y = lag, colour = species, group = species), 
              method = "lm",
              se = F) +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") #+ facet_rep_wrap(~year)

bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", 
              se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +
  ylab("date") + xlab("Latitude") + ggtitle("Bird date") +
  facet_rep_wrap(~year)



ggplot(bg.mat) + 
  geom_smooth(method = "lm",aes(x = cell_lat, y = vArrMag_log, color = species), show.legend = FALSE, se = FALSE) +
  geom_smooth(method = "lm",aes(x = cell_lat, y = vGrMag_log, size = 1), color = "black", show.legend = FALSE, se = FALSE)+
  facet_rep_wrap(~year)

#

bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", 
              se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(who_n == "senlat"),
              aes(x = cell_lat, y = arr_GAM_mean, colour = who, group = species), 
              method = "lm",
              se = F) +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +   facet_rep_wrap(~year)

bg.mat %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "gray")  +
  geom_hline(yintercept = 0.1, color = "gray", linetype = "dashed")  +
  geom_hline(yintercept = -0.1, color = "gray", linetype = "dashed")  +
  geom_smooth(aes(x = cell_lat, y = AnomVArr), 
              method = "lm", 
              se = F, colour = "black") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw()  +   facet_rep_wrap(~year)

bg.mat %>% 
  ggplot() +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = vArrMag, 
                  alpha = 0.1, colour = species), 
              method = "lm", 
              show.legend = FALSE,
              se = F, alpha=0.3, size=0.5, span=0.5) +
  geom_smooth(aes(x = cell_lat, y = vArrMag), 
              method = "lm",
              colour = "black",
              show.legend = FALSE,
              se = F, legend = F) +
  facet_rep_wrap(~year)


bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
 # geom_hline(yintercept = 0, color = "black")  +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean, col = who), 
              method = "lm", 
               se = F) +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +
  #ylab("speed") + xlab("Latitude") + ggtitle("bird and GU speed") +
  facet_rep_wrap(~year)

bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log, col = who)) 

bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = vArrMag, color = who), 
              method = "lm", 
              se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = vGrMag), 
              method = "lm", 
              se = F, colour = "darkolivegreen") +
 # ylab("speed") + xlab("Latitude") + ggtitle("bird and GU speed anomaly") +
  facet_rep_wrap(~year)


bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vArrMag, color = who)) 

bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = lag), 
              #method = "lm", 
              se = F, colour = "plum3") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0, color = "black")  +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed")  +
  geom_hline(yintercept = -0.5, color = "gray", linetype = "dashed")  +
  facet_rep_wrap(~year
                 #, scales = "free_y"
                 )


bg.mat %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", se = F, colour = "gray") +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(who_n == "senlat"),
              aes(x = cell_lat, y = arr_GAM_mean, colour = who, group = species), 
              method = "lm", se = F) +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", se = F, colour = "darkolivegreen") +
  ylab("date") + xlab("Latitude") + ggtitle("Sensitivity to latitude")

# slow migration year?
bg.mat %>% 
  ggplot() +
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean), 
              method = "lm", se = F, colour = "gray") +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              method = "lm", se = F, colour = "darkolivegreen") +
  facet_rep_wrap(~year) +
  ylab("date") + xlab("Latitude") 

# how does the size of the slope affects the correlation between 
#   green up and arrival? how green up speed affects 
bg.matX <- bg.mat #%>% 
  #filter(species %in% sps_who$species)
# green up speed
gpslopes <- c(
  as.numeric(lm(data = bg.matX %>% filter(year == 2009), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2010), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2011), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2012), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2013), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2014), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2015), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2016), gr_mn ~ cell_lat)$coef[2]),
  as.numeric(lm(data = bg.matX %>% filter(year == 2017), gr_mn ~ cell_lat)$coef[2])
)

cors <- c(
  as.numeric(cor((bg.matX %>% filter(year == 2009) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2010) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2011) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2012) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2013) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2014) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2015) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2016) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2017) %>% dplyr::select(gr_mn, arr_GAM_mean)), use = "complete.obs")[2,1])
)

cors <- c(
  as.numeric(cor((bg.matX %>% filter(year == 2009) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2010) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2011) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2012) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2013) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2014) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2015) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2016) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2017) %>% dplyr::select(vGrMag, vArrMag)), use = "complete.obs")[2,1])
)
  
cors <- c(
  as.numeric(cor((bg.matX %>% filter(year == 2009) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2010) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2011) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2012) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2013) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2014) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2015) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2016) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1]),
  as.numeric(cor((bg.matX %>% filter(year == 2017) %>% dplyr::select(AnomVGr, AnomVArr)), use = "complete.obs")[2,1])
)

guslo <- as_tibble(cbind(seq(2009,2017,1), gpslopes)) %>% 
  rename(year = V1) %>% 
  mutate(Cor = cors) 

ggplot(guslo) +
  geom_point(aes(x = gpslopes, y = Cor, shape = as.factor(year))) +
  theme_bw() + 
  scale_shape_manual(values=seq(0,9))
# the bigger the slope (slow) of green up, the bigger the correlation

## anomalies for the species groups
bg.mat$who_n <- factor(bg.mat$who_n, levels = c("mig","senlat","var" ), ordered = TRUE)

bg.mat %>% 
  filter(species %in% sps_who$species) %>% 
  ggplot() +
    geom_boxplot(aes(x = reorder(who, as.numeric(who_n), na.rm = T), y = AnomVArr, fill = who_n)) +
    ggtitle("Anomaly variation") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## lag for the species groups
bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "red")  +
  geom_boxplot(aes(x = reorder(who, as.numeric(who_n), na.rm = T), y = lag, fill = who)) +
  ggtitle("Lag") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  #geom_hline(yintercept = 0, color = "red")  +
  geom_boxplot(aes(x = reorder(who, as.numeric(who_n), na.rm = T), y = arr_GAM_mean, fill = who)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot(aes(x = cell_lat, y = lag, col = who, shape = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

  
bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "black")  +
  geom_point(aes(x = cell_lat, y = lag, col = who)) +
  ggtitle("Lag") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# title ----
## subtitle --------
### sub sub title ------- 
#### hhhfhfhfhfh ------------






