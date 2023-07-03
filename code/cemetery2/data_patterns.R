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
dat_year <- "all"

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
         year_g = ifelse(year %in% c(2010,2012), "early", (ifelse(year %in% c(2011,2015), "ave", ifelse(year %in% c(2013,2014), "late", "ops")))),
         fast_g = ifelse(year %in% c(2009,2010,2011,2013,2014),"fast", ifelse(year %in% c(2012,2015,2016,2017), "slow", "ops")))

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

bg.mat <- bg.mat %>% 
  dplyr::select(-c(gr_mn, gr_ncell,
                   NGr, vGrMag, vGrAng, angG, xG, yG, vGrMag_log,
                   AnomVGr, AnomDGr, AnomLag))

#bg.mat <- left_join(bg.mat, bg.matG[,-c(18:20)], by =c('year', 'cell', 'cell_lat', 'cell_lng', 'cell_lat_s') )
bg.mat$lat_g <- factor(bg.mat$lat_g, levels = c("low", "mid", "high"))
bg.mat$year_g <- factor(bg.mat$year_g, levels = c("early", "ave", "late"))
bg.mat$fast_g <- factor(bg.mat$fast_g, levels = c("slow", "fast"))

# LATITUDINAL MEANS ---------------------------------------------
## dates  -------------------------
ggplot() +
  geom_smooth(data = bg.matG,
              aes(x = cell_lat, y = gr_mn), col = "darkolivegreen", se = F) +
  theme_bw() +
  geom_smooth(data = bg.mat,
              aes(x = cell_lat, y = arr_GAM_mean), col = "plum3", se = F) +
  ylab("Date") + xlab("Latitude") 

ggplot() +
  geom_hline(yintercept = 0, col = "darkgray") +
  geom_smooth(data = bg.matG,
              aes(x = cell_lat, y = AnomDGr), col = "darkolivegreen", se = F) +
  theme_bw() +
  geom_smooth(data = bg.mat,
              aes(x = cell_lat, y = AnomDArr), col = "plum3", se = F) +
  ylab("Anomaly arrival date") + xlab("Latitude") +facet_rep_wrap(~year)

ggplot() +
  geom_smooth(data = bg.matG,
              aes(x = cell_lat, y = gr_mn), col = "darkolivegreen", se = F) +
  theme_bw() +
  geom_smooth(data = bg.mat,            
              aes(x = cell_lat, y = arr_GAM_mean, col = species), se = F, show.legend = F) +
  geom_smooth(data = bg.mat,
              aes(x = cell_lat, y = arr_GAM_mean), col = "black", se = F, size = 2) +
  ylab("Date") + xlab("Latitude") 

for(i in 2002:2017){
  print(ggplot() +
          geom_smooth(data = bg.matG %>% filter(year == i),
                      aes(x = cell_lat, y = gr_mn), col = "darkolivegreen", se = F) +
          theme_bw() +
          geom_smooth(data = bg.mat %>% filter(year == i)
                      , aes(x = cell_lat, y = arr_GAM_mean), col = "plum3", se = F) +
          ylab("Date") + xlab("Latitude") + facet_rep_wrap(~year))
}

## speed  -------------------------
ggplot() +
  geom_smooth(data = bg.matG,
              aes(x = cell_lat, y = vGrMag), col = "darkolivegreen", se = F) +
  theme_bw() +
  geom_smooth(data = bg.mat,
              aes(x = cell_lat, y = vArrMag), col = "plum3", se = F) +
  ylab("Speed") + xlab("Latitude") 

ggplot() +
  theme_bw() +
  geom_smooth(data = bg.mat,
              aes(x = cell_lat, y = vArrMag), col = "plum3", se = F) + coord_cartesian(ylim = c(0,1500))+
  ylab("Speed") + xlab("Latitude") + facet_rep_wrap(~species, scales = "free_y") + 
  theme(strip.text = element_text(size=6))

ggplot() +
  #geom_smooth(data = bg.matG,
  #            aes(x = cell_lat, y = vGrMag), col = "darkolivegreen", se = F) +
  theme_bw() +
  geom_smooth(data = bg.mat %>% filter(!is.na(vArrMag_log)),
              aes(x = cell_lat, y = vArrMag_log, color = species), 
              se = F, show.legend = F) +
  geom_smooth(data = bg.mat %>% filter(!is.na(vArrMag_log)),
              aes(x = cell_lat, y = vArrMag_log), color = "black", size = 2, 
              se = F, show.legend = F) +
  ylab("Log speed") + xlab("Latitude")

for(i in 2002:2017){
  print(ggplot() +
          geom_smooth(data = bg.matG %>% filter(year == i),
                      aes(x = cell_lat, y = vGrMag), col = "darkolivegreen", se = F) +
          theme_bw() +
          geom_smooth(data = bg.mat %>% filter(year == i)
                      , aes(x = cell_lat, y = vArrMag), col = "plum3", se = F) +
          ylab("Speed") + xlab("Latitude") + facet_rep_wrap(~year, scales= "free_y") #+ 
    #coord_cartesian(ylim = c(0, 4000))
  )
}
  
for(i in 2002:2017){
  print(ggplot() +
          geom_smooth(data = bg.matG %>% filter(year == i),
                      aes(x = cell_lat, y = AnomDGr), col = "darkolivegreen", se = F) +
          theme_bw() +
          geom_smooth(data = bg.mat %>% filter(year == i)
                      , aes(x = cell_lat, y = AnomVArr), col = "plum3", se = F) +
          ylab("Speed") + xlab("Latitude") + facet_rep_wrap(~year, scales= "free_y") #+ 
        #coord_cartesian(ylim = c(0, 4000))
  )
}

for(i in 2002:2017){
    print(ggplot() +
            theme_bw() +
            geom_point(data = bg.mat %>% filter(year == i)
                        , aes(x = cell_lat, y = vArrMag), col = "plum3") +
            ylab("Speed") + xlab("Latitude") + facet_rep_wrap(~year, scales= "free_y") #+ 
          #coord_cartesian(ylim = c(0, 4000))
    )
}

for(i in 2002:2017){
  print(ggplot() +
          geom_smooth(data = bg.matG %>% filter(year == i),
                      aes(x = gr_mn, y = vGrMag), col = "darkolivegreen", se = F) +
          theme_bw() +
          geom_smooth(data = bg.mat %>% filter(year == i)
                      , aes(x = arr_GAM_mean, y = vArrMag), col = "plum3", se = F) +
          #ylab("Speed") + xlab("Latitude") + 
          facet_rep_wrap(~year) #+ 
  )
}

# ANNUAL MEANS -------------------------------------------------- 
###### means for bird and green up for all and individual years

(a <- bg.mat %>% 
  summarise(meanBD = mean(arr_GAM_mean, na.rm = T),
            sdBD = sd(arr_GAM_mean, na.rm = T)))
(ay <- bg.mat %>% 
  group_by(year) %>% 
  summarise(meanBD = mean(arr_GAM_mean, na.rm = T),
            sdBD = sd(arr_GAM_mean, na.rm = T)))
sd(ay$meanBD, na.rm = T)

ggplot(ay) +
  geom_hline(yintercept = a$meanBD, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanBD, ymin=meanBD-sdBD, ymax=meanBD+sdBD), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanBD), col = "plum3", size = 2) +
  theme_bw() +
  ylab("Bird arrival date") +
  xlab("Year") + coord_cartesian(ylim = c(100, 142)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

(b <- bg.mat %>% 
  summarise(meanBV = mean(vArrMag_log, na.rm = T),
            sdBV = sd(vArrMag_log, na.rm = T)))
(by <- bg.mat %>% 
  group_by(year) %>% 
  summarise(meanBV = mean(vArrMag_log, na.rm = T),
            sdBV = sd(vArrMag_log, na.rm = T)))
sd(by$meanBV, na.rm = T)

ggplot(by) +
  geom_hline(yintercept = b$meanBV, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanBV, ymin=meanBV-sdBV, ymax=meanBV+sdBV), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanBV), col = "plum3", size = 2) +
  theme_bw() +
  ylab("Bird speed (log)") +
  xlab("Year") + coord_cartesian(ylim = c(4, 7.4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

(c <- bg.matG %>% 
    summarise(meanGD = mean(gr_mn, na.rm = T),
              sdGD = sd(gr_mn, na.rm = T)))
(cy <- bg.matG %>% 
    group_by(year) %>% 
    summarise(meanGD = mean(gr_mn, na.rm = T),
              sdGD = sd(gr_mn, na.rm = T)))
sd(cy$meanGD)

ggplot(cy) +
  geom_hline(yintercept = c$meanGD, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanGD, ymin=meanGD-sdGD, ymax=meanGD+sdGD), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanGD), col = "darkgreen", size = 2) +
  theme_bw() +
  ylab("Green up date") +
  xlab("Year") + coord_cartesian(ylim = c(99, 142)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

(d <- bg.matG %>% 
    summarise(meanGV = mean(vGrMag_log, na.rm = T),
              sdGV = sd(vGrMag_log, na.rm = T)))

(dy <- bg.matG %>% 
    group_by(year) %>% 
    summarise(meanGV = mean(vGrMag_log, na.rm = T),
              sdGV = sd(vGrMag_log, na.rm = T)))
sd(dy$meanGV)

ggplot(dy) +
  geom_hline(yintercept = d$meanGV, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanGV, ymin=meanGV-sdGV, ymax=meanGV+sdGV), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanGV), col = "darkgreen", size = 2) +
  theme_bw() +
  ylab("Green up speed (log)") +
  xlab("Year") + coord_cartesian(ylim = c(4, 7.4)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

cor(ay$meanBD,cy$meanGD, use = "complete.obs")
cor(by$meanBV,dy$meanGV, use = "complete.obs")
cor(ay$meanBD,by$meanBV, use = "complete.obs")
cor(cy$meanGD,dy$meanGV, use = "complete.obs")
cor(cy$meanGD,by$meanBV, use = "complete.obs")


rm(ay,by,cy,dy,a,b,c,d)
## same means for anomaly
(a2 <- bg.mat %>% 
    summarise(meanBD = mean(AnomDArr, na.rm = T),
              sdBD = sd(AnomDArr, na.rm = T)))
(ay2 <- bg.mat %>% 
    group_by(year) %>% 
    summarise(meanBD = mean(AnomDArr, na.rm = T),
              sdBD = sd(AnomDArr, na.rm = T)))
sd(ay2$meanBD, na.rm = T)

ggplot(ay2) +
  geom_hline(yintercept = a$meanBD, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanBD, ymin=meanBD-sdBD, ymax=meanBD+sdBD), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanBD), col = "plum3", size = 2) +
  theme_bw() +
  ylab("Bird arrival date anomaly") +
  xlab("Year") + coord_cartesian(ylim = c(-0.12, 0.08)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

(b2 <- bg.mat %>% 
    summarise(meanBV = mean(AnomVArr, na.rm = T),
              sdBV = sd(AnomVArr, na.rm = T)))
(by2 <- bg.mat %>% 
    group_by(year) %>% 
    summarise(meanBV = mean(AnomVArr, na.rm = T),
              sdBV = sd(AnomVArr, na.rm = T)))
sd(by2$meanBV, na.rm = T)

ggplot(by2) +
  geom_hline(yintercept = b$meanBV, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanBV, ymin=meanBV-sdBV, ymax=meanBV+sdBV), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanBV), col = "plum3", size = 2) +
  theme_bw() +
  ylab("Bird speed anomaly") +
  xlab("Year") + coord_cartesian(ylim = c(-2, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

(c2 <- bg.matG %>% 
    summarise(meanGD = mean(AnomDGr, na.rm = T),
              sdGD = sd(AnomDGr, na.rm = T)))
(cy2 <- bg.matG %>% 
    group_by(year) %>% 
    summarise(meanGD = mean(AnomDGr, na.rm = T),
              sdGD = sd(AnomDGr, na.rm = T)))
sd(cy2$meanGD)

ggplot(cy2) +
  geom_hline(yintercept = c$meanGD, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanGD, ymin=meanGD-sdGD, ymax=meanGD+sdGD), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanGD), col = "darkgreen", size = 2) +
  theme_bw() +
  ylab("Green up date anomaly") +
  xlab("Year") + coord_cartesian(ylim = c(-0.135, 0.08)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

(d2 <- bg.matG %>% 
    summarise(meanGV = mean(AnomVGr, na.rm = T),
              sdGV = sd(AnomVGr, na.rm = T)))
(dy2 <- bg.matG %>% 
    group_by(year) %>% 
    summarise(meanGV = mean(AnomVGr, na.rm = T),
              sdGV = sd(AnomVGr, na.rm = T)))
sd(dy2$meanGV)

ggplot(dy2) +
  geom_hline(yintercept = d$meanGV, col = "darkgray") +
  geom_errorbar(aes(x = as.factor(year), y = meanGV, ymin=meanGV-sdGV, ymax=meanGV+sdGV), width=.2) +
  geom_point(aes(x = as.factor(year), y = meanGV), col = "darkgreen", size = 2) +
  theme_bw() +
  ylab("Green up speed anomaly") +
  xlab("Year") + coord_cartesian(ylim = c(-2, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

cor(ay2$meanBD,cy2$meanGD, use = "complete.obs")
cor(by2$meanBV,dy2$meanGV, use = "complete.obs")
cor(ay2$meanBD,by2$meanBV, use = "complete.obs")
cor(cy2$meanGD,dy2$meanGV, use = "complete.obs")
cor(cy2$meanGD,by2$meanBV, use = "complete.obs")

cor(ay2$meanBD,by$meanBV, use = "complete.obs")
plot(ay2$meanBD,by$meanBV, ylab = "mean bird speed", xlab = "mean bird date anomaly")
abline(lm(by$meanBV ~ ay2$meanBD), col = 4, lwd = 3)

cor(cy2$meanGD,dy$meanGV, use = "complete.obs")
plot(cy2$meanGD,dy$meanGV, ylab = "mean green up speed", xlab = "mean green up date anomaly")
abline(lm(dy$meanGV ~ cy2$meanGD), col = 4, lwd = 3)

cor(cy2$meanGD,by$meanBV, use = "complete.obs")
plot(cy2$meanGD,by$meanBV, ylab = "mean bird speed", xlab = "mean green up date anomaly")
abline(lm(by$meanBV~ cy2$meanGD), col = 4, lwd = 3)

cor(dy$meanGV,by$meanBV, use = "complete.obs")

summary(lm(data = bg.matBG, 
           vArrMag_log ~ vGrMag_log))
cor(bg.matBG$vArrMag_log, bg.matBG$vGrMag_log, use = "complete.obs")
ggplot(bg.matBG) + geom_smooth(aes(y =vArrMag_log, x = vGrMag_log), method = 'lm')+ theme_bw()

summary(lm(data = bg.matBG, 
           vArrMag_log ~ AnomDArr))
summary(lm(data = bg.mat2, 
           vArrMag_log ~ AnomDGr))
summary(lm(data = bg.mat2, 
           vGrMag_log ~ AnomDGr))
ggplot(bg.mat2) + geom_smooth(aes(y =vGrMag_log, x = AnomDGr), method = 'lm')+ theme_bw()

# SPECIES OUTPUTS
bg.mat %>% 
  group_by(species) %>% 
  summarise(mean_spe = mean(abs(AnomVArr), na.rm = T)) %>% 
  View()

spsvec1 <- c(
  # migration length
  # LONG
  "Sayornis_phoebe", "Setophaga_caerulescens", "Archilochus_colubris", "Tachycineta_bicolor",
  "Coccyzus_americanus","Hirundo_rustica", "Passerina_cyanea", "Vireo_olivaceus",
  # SHORT
  "Dolichonyx_oryzivorus", "Empidonax_traillii", # short in the paper
  "Pipilo_erythrophthalmus", "Spizella_passerina", "Troglodytes_aedon", "Vireo_griseus",#short!
  "Setophaga_discolor", "Setophaga_dominica", "Setophaga_fusca", "Setophaga_tigrina")#medium
spsvec2 <- c(  # sensitivity to lat
  # nsen
  "Parkesia_motacilla", "Piranga_rubra", "Archilochus_colubris","Setophaga_cerulea","Icterus_spurius",
  # sen
  "Tachycineta_bicolor","Oreothlypis_ruficapilla", 'Sayornis_phoebe', 'Setophaga_pinus', 
  'Pipilo_erythrophthalmus', "Setophaga_dominica")
spsvec3 <- c(
  # variability in speed between years
  # HIGH
  'Setophaga_pinus','Contopus_virens','Icteria_virens','Parkesia_motacilla','Empidonax_minimus',
  'Sayornis_phoebe','Geothlypis_formosa','Dolichonyx_oryzivorus','Tyrannus_tyrannus','Protonotaria_citrea',
  # LOW
  'Dumetella_carolinensis','Setophaga_discolor','Vireo_solitarius','Setophaga_cerulea','Setophaga_tigrina',
  'Setophaga_petechia','Setophaga_virens','Setophaga_citrina','Vermivora_cyanoptera','Petrochelidon_pyrrhonota'
)
table(spsvec1 %in% bg.mat$species)
table(spsvec2 %in% bg.mat$species)
table(spsvec3 %in% bg.mat$species)

who1 <- c(rep("long",8), rep("short",10))
who2 <- c(rep("nsen",5), rep("sen", 6))
who3 <- c(rep("hivar",10), rep("lovar", 10))

who_n1 <- rep("mig",18)
who_n2 <- rep("senlat",11)
who_n3 <- rep("var",20)

sps_who1 <- as_tibble(cbind(spsvec1, who1, who_n1)) %>% 
  rename(species = spsvec1)
sps_who2 <- as_tibble(cbind(spsvec2, who2, who_n2)) %>% 
  rename(species = spsvec2)
sps_who3 <- as_tibble(cbind(spsvec3, who3, who_n3)) %>% 
  rename(species = spsvec3)

dim(bg.mat)
bg.mat <- left_join(bg.mat, sps_who1, by = "species")
dim(bg.mat)
bg.mat <- left_join(bg.mat, sps_who3, by = "species")
dim(bg.mat)
bg.mat <- left_join(bg.mat, sps_who2, by = "species")
dim(bg.mat)

# short and long distance migrants ---------------------
bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
#  geom_point(aes(x = cell_lat, y = vArrMag, col = who, alpha = 0.1)) +
  geom_smooth(aes(x = cell_lat, y = vArrMag, col = who1), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = vGrMag), 
              col = "darkolivegreen", se = F, linetype = "dashed") #+ facet_rep_wrap(~year,
                                                                    #                scales = "free_y")
bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
  #  geom_point(aes(x = cell_lat, y = vArrMag, col = who, alpha = 0.1)) +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean, col = who1), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              col = "darkolivegreen", se = F, linetype = "dashed") 
  
bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkgray")  +
  geom_smooth(aes(x = cell_lat, y = AnomVArr, col = who1), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = AnomVGr), 
              col = "darkolivegreen", se = F, linetype = "dashed") 

bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkgray")  +
  #geom_smooth(aes(x = cell_lat, y = AnomVArr, col = who), se = F) +
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = lag, 
              col = who1), se = F, linetype = "dashed") 

bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_boxplot(aes(x = who1, y = vArrMag_log, fill = who1)) +
  #facet_rep_wrap(~who1) +
  theme_bw()

summary(lm(data = bg.mat %>% 
             filter(who_n1 == "mig"), vArrMag_log ~ who1))

summary(lm(data = bg.mat %>% 
             filter(who_n2 == "senlat"), vArrMag_log ~ who2))
summary(lm(data = bg.mat %>% 
             filter(who_n3 == "var"), vArrMag_log ~ who3))

bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_boxplot(aes(x = who1, y = vArrMag_log, fill = who1)) +
  geom_point(aes(x = who1, y = vArrMag_log, fill = who1)) +
  facet_rep_wrap(~lat_g) +
  theme_bw()

bg.mat %>% 
  filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = vArrMag_log, fill = lat_g)) +
  facet_rep_wrap(~who1) +
  theme_bw()

bg.matT <- left_join(bg.mat, bg.matG %>% dplyr::select(year, cell, vGrMag) %>% unique, by = c('year', 'cell'))
summary(lm(data = bg.matT %>% filter(who_n1 == "mig"),
           vArrMag_log ~ vGrMag + who1))
# sensitivity ---------------------
bg.mat %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  #  geom_point(aes(x = cell_lat, y = vArrMag, col = who, alpha = 0.1)) +
  geom_smooth(aes(x = cell_lat, y = vArrMag, col = who2), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = vGrMag), 
              col = "darkolivegreen", se = F, linetype = "dashed") +
  facet_rep_wrap(~year, scales = "free_y")

bg.mat %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  #  geom_point(aes(x = cell_lat, y = vArrMag, col = who, alpha = 0.1)) +
  geom_smooth(aes(y = lag, x = vArrMag, col = who2), se = F) +
  theme_bw() +
  
  facet_rep_wrap(~year#, scales = "free_y"
                 )

bg.matBG %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  geom_point(aes(x = vGrMag_log, y = vArrMag_log, col = who2), alpha = 0.3) +
  geom_smooth(aes(x = vGrMag_log, y = vArrMag_log, col = who2), se = F) +
  theme_bw() 
 #facet_rep_wrap(~year, scales = "free_y")

summary(lm(data = bg.matBG %>% filter(who_n2 == "senlat"), 
           vArrMag_log ~ vGrMag_log + who2))

bg.mat %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkgray")  +
  #geom_smooth(aes(x = cell_lat, y = AnomVArr, col = who), se = F) +
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = lag, 
                  col = who2), se = F, linetype = "dashed") 

bg.mat %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  #  geom_point(aes(x = cell_lat, y = vArrMag, col = who, alpha = 0.1)) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr, col = who2), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = AnomVGr), 
              col = "darkolivegreen", se = F, linetype = "dashed") 

bg.mat %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = vArrMag_log, fill = lat_g)) +
  facet_rep_wrap(~who2) +
  theme_bw()

bg.mat %>% 
  filter(who_n2 == "senlat") %>% 
  ggplot() +
  geom_boxplot(aes(x = who2, y = vArrMag_log, fill = who2)) +
  #facet_rep_wrap(~who1) +
  theme_bw()

# variability ---------------------
bg.mat %>% 
  filter(who_n3 == "var") %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vArrMag, col = who3, alpha = 0.1)) +
  geom_smooth(aes(x = cell_lat, y = vArrMag, col = who3)) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = vGrMag), 
              col = "darkolivegreen", se = F, linetype = "dashed") #+
#facet_rep_wrap(~year, scales = "free_y")

bg.mat %>% 
  filter(who_n3 == "var") %>% 
  ggplot() +
  geom_boxplot(aes(x = who3, y = vArrMag_log, fill = who3)) +
  #facet_rep_wrap(~who1) +
  theme_bw()

bg.mat %>% 
  filter(who_n3 == "var") %>% 
  ggplot() +
  #geom_smooth(aes(x = AnomVArr, y = vArrMag, col = who3)) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr, col = who3), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = AnomVGr), 
              col = "darkolivegreen", se = F, linetype = "dashed")
bg.mat %>% 
  filter(who_n3 == "var") %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkgray")  +
  #geom_smooth(aes(x = cell_lat, y = AnomVArr, col = who), se = F) +
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = lag, 
                  col = who3), se = F, linetype = "dashed") 

bg.mat %>% 
  filter(who_n3 == "var") %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = vArrMag_log, fill = lat_g)) +
  facet_rep_wrap(~who3) +
  theme_bw()

bg.mat %>% 
  filter(who_n3 == "var") %>% 
  ggplot() +
  #  geom_point(aes(x = cell_lat, y = vArrMag, col = who, alpha = 0.1)) +
  geom_smooth(aes(x = cell_lat, y = arr_GAM_mean, col = who3), se = F) +
  theme_bw() +
  geom_smooth(data = bg.matG, aes(x = cell_lat, y = gr_mn), 
              col = "darkolivegreen", se = F, linetype = "dashed") 


# mig vs breeding range ---------------------
bg.matBG <- bg.matG %>% dplyr::select(year, cell, vGrMag_log,
                                     vGrMag, gr_mn) %>% unique()
bg.matBG <- bg.matBG[!duplicated(bg.matBG[1:2]),]
bg.matBG <- left_join(bg.mat, 
                      bg.matBG,
                      by = c("year","cell"))
bg.matBG %>% 
  filter(mig_cell == T) %>% 
  ggplot(aes(x = vGrMag_log, y = vArrMag_log)) +
  #geom_point() +
  geom_smooth( se = F) + theme_bw() 

bg.matBG %>% 
  filter(breed_cell == T) %>% 
  ggplot(aes(x = vGrMag_log, y = vArrMag_log)) +
  #geom_point() +
  geom_smooth( se = F, col = "red") + theme_bw() 

ggplot() +
  #geom_point() +
  #geom_point(data = bg.matBG %>% filter(breed_cell == T),
  #            aes(x = vGrMag, y = vArrMag), col = "red") +
  #geom_point(data = bg.matBG %>% filter(mig_cell == T,),
  #           aes(x = vGrMag, y = vArrMag), col = "blue") + theme_bw() +
  geom_smooth(data = bg.matBG %>% filter(breed_cell == T),
             aes(x = vGrMag, y = vArrMag),  col = "red") +
  geom_smooth(data = bg.matBG %>% filter(mig_cell == T,),
             aes(x = vGrMag, y = vArrMag)) + theme_bw()

ggplot() +
  geom_smooth(data = bg.mat %>% filter(breed_cell == T, !is.na(mig_cell)),
              aes(x = cell_lat, y = vArrMag), color = "red",
              #se = F
              ) +
  geom_smooth(data = bg.mat %>% filter(mig_cell == T, !is.na(mig_cell)),
             aes(x = cell_lat, y = vArrMag), color = "blue",
             #se = F
  ) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,2000)) +
  facet_rep_wrap(~year)

ggplot() +
  geom_smooth(data = bg.mat %>% filter(mig_cell == T),
              aes(x = cell_lat, y = lag), color = "blue",
              se = F) +
  geom_smooth(data = bg.mat %>% filter(breed_cell == T, !is.na(mig_cell)),
              aes(x = cell_lat, y = lag), color = "red",
              se = F) +
  theme_bw() 
  #coord_cartesian(ylim = c(0,800)) +
  #facet_rep_wrap(~year)

## anomaly according to lat
bg.matG %>% 
  ggplot() +
  geom_smooth(aes(x = cell_lat, y = AnomVGr), se = F) + theme_bw()

bg.matG$lat_g <- factor(bg.matG$lat_g, levels = c("low", "mid", "high"))
bg.matG %>% 
  ggplot() +
  geom_hline(yintercept = 0, col = "black") +
  geom_boxplot(aes(x = lat_g, y = AnomVGr)) + theme_bw() + 
facet_rep_wrap(~year)

bg.mat$lat_g <- factor(bg.mat$lat_g, levels = c("low", "mid", "high"))
bg.mat %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = AnomVArr), se = F) + theme_bw() + 
  facet_rep_wrap(~species)

## early and late years ---------------------
bg.matBG %>% 
  filter(!is.na(year_g), 
         #!is.na(who1)
         ) %>% 
  ggplot() +
  #geom_point(aes(x = lat_g, y = vArrMag_log), col = "yellow") + theme_bw() +
  geom_boxplot(aes(x = lat_g, y = vArrMag_log, fill = lat_g)) + theme_bw() +
  facet_rep_wrap(~who3)

bg.matBG %>% 
  filter(!is.na(year_g)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year_g, y = vGrMag_log, fill = year_g)) + theme_bw()

bg.matBG %>% 
  filter(who_n1 == "mig") %>% 
  filter(!is.na(year_g)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year_g, y = vArrMag_log)) + theme_bw() 

bg.matBG %>% 
  filter(!is.na(year_g)) %>% 
  #filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vArrMag_log, col = year_g),alpha = 0.4) + theme_bw() +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log, col = year_g), se = F) + theme_bw() 

bg.matBG %>% 
  filter(!is.na(year_g)) %>% 
  #filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = AnomVArr, col = year_g),alpha = 0.4) + theme_bw() +
  geom_smooth(aes(x = cell_lat, y = AnomVArr, col = year_g), se = F) + theme_bw() 

bg.matBG %>% 
  filter(!is.na(year_g)) %>% 
  #filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_hline(yintercept = 0, col = "darkgray")+
  geom_smooth(aes(x = cell_lat, y = lag, col = year_g), se = F) + theme_bw() +
  facet_rep_wrap(~ year_g)

bg.matBG %>% 
  filter(!is.na(year_g)) %>% 
  #filter(who_n1 == "mig") %>% 
  ggplot() +
  geom_hline(yintercept = 0, col = "darkgray")+
  geom_smooth(aes(x = cell_lat, y = AnomVArr, col = year_g), se = F) + theme_bw() +
  facet_rep_wrap(~ year_g)

## fast and slow years
bg.matBG %>% 
  filter(!is.na(fast_g)) %>% 
  ggplot() +
  geom_boxplot(aes(x = fast_g, y = vArrMag_log)) + theme_bw()


## breeding range
bg.mat %>% 
  group_by(species) %>% 
  filter(breed_cell == T) %>% 
  mutate(med_lat = median(cell_lat, na.rm = T),
         mean_vel = mean(vArrMag_log, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = med_lat, y = mean_vel)) +
  theme_bw() +
  geom_smooth(aes(x = med_lat, y = mean_vel), method = "lm", se = F)+
  ylab("Mean bird speed") + xlab("Median latitude of breeding range")

## Anomalies ----------------
bg.matG %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = AnomVGr)) + theme_bw()

bg.matG %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = AnomVGr)) + theme_bw() +
  geom_smooth(aes(x = cell_lat, y = AnomVGr))

bg.mat %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = AnomVArr)) + theme_bw() +
  geom_smooth(aes(x = cell_lat, y = AnomVArr))

bg.mat2 %>% 
  mutate(lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps")))) %>% 
  ggplot() +
  geom_point(aes(x = AnomVGr, y = AnomVArr, col=lat_g)) + theme_bw()

bg.mat2 %>% 
  mutate(lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps")))) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vArrMag, col=lat_g)) + theme_bw() +
  facet_rep_wrap(~lat_g)

bg.mat %>% 
  mutate(lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps")))) %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = AnomVArr)) + theme_bw() 

## green up anomaly and bird spped
bg.matG2 <- bg.matG %>% 
  group_by(cell, year) %>% 
  mutate(AnomDGr2 = mean(AnomDGr)) %>% 
  ungroup() %>% 
  dplyr::select(cell, AnomDGr2, year) %>% 
  unique()

bg.matBG3 <- left_join(bg.matBG2, bg.matG2)

bg.matBG3 %>% 
  #filter(!is.na(glue("{who1}{who2}{who3}"))) %>% 
  ggplot()+
  geom_smooth(aes(x = AnomDGr2, y = vArrMag_log), se=F) +
  theme_bw() #+
  #facet_rep_wrap(~species)

bg.matBG3 %>% 
  filter(!is.na(who1)) %>% 
  ggplot()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(x = AnomDGr2,  y = vArrMag, col = who1)) +
  theme_bw() +
  facet_rep_wrap(~species, scales = "free_y")

bg.matBG3 %>% 
  #filter(!is.na(glue("{who1}{who2}{who3}"))) %>% 
  ggplot()+
  geom_vline(xintercept = 0) +
  geom_smooth(aes(x = AnomDGr2, y = vArrMag)) +
  theme_bw() +
  facet_rep_wrap(~who2)

bg.matBG3 %>% 
  #filter(!is.na(glue("{who1}{who2}{who3}"))) %>% 
  ggplot()+
  geom_vline(xintercept = 0) +
  geom_smooth(aes(x = AnomDGr2, y = AnomVArr)) +
  theme_bw() +
  facet_rep_wrap(~who3)


summary(lm(data = bg.matBG3%>% 
               filter(!is.na(who3)), vArrMag_log ~ AnomDGr2 + who3 ))

bg.matBG3%>% 
  filter(!is.na(who3)) %>% 
ggplot(aes(y = vArrMag_log, x= AnomDGr2, col = who3)) +
  geom_point(alpha = 0.1)+
  geom_smooth(method = "lm") + theme_bw() + ylim(c(4, 8)) 


bg.matBG3%>% 
  filter(!is.na(who3)) %>% 
  ggplot() +
  geom_point(aes(y = vArrMag_log, x= AnomDGr2, col = who3), alpha = 0.1)+
  geom_smooth(aes(y = vArrMag_log, x= AnomDGr2,
                  col = who3), method = "lm") + theme_bw()


bg.matBG3%>%
  mutate(cell_type = ifelse(mig_cell == T & breed_cell == F, "mig",
                            ifelse(mig_cell == F & breed_cell == T, "bree",
                                   ifelse(mig_cell == T & breed_cell == T, "both", "ops"))))%>% 
  filter(cell_type %in% c("mig", "bree")) %>% 
  ggplot(aes(y = vArrMag_log, x= AnomDGr2, col = cell_type)) +
  geom_point()+
  geom_smooth(method = "lm") + theme_bw()

## do early lag causes speed up?  ----------------------------
anla <- bg.mat2 %>% 
  dplyr::select(AnomLag, species, cell, year, vArrMag, cell_lat) %>% 
  mutate(cell = as.factor(cell)) %>% 
  unique()

earlag1 <- anla %>% 
  filter(!is.na(vArrMag)) %>% 
  group_by(species, year) %>% 
  slice_min(cell_lat, n = 5) %>% 
  mutate(earlag = mean(AnomLag, na.rm = T)) %>% 
  ungroup() %>% 
  select(year, species, cell, earlag)

bg.matBG4 <- left_join(bg.matBG3, earlag1, by = c('year', 'species', 'cell'))

bg.matBG4 %>% 
 # filter(earlag > 0) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  geom_point(aes(x = earlag, y = vArrMag_log), show.legend = F) +
  geom_smooth(aes(x = earlag, y = vArrMag_log), show.legend = F, se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme_bw()#+
  #facet_rep_wrap(~species#, scales = "free_y"
  #               )

bg.matBG4 %>% 
  # filter(earlag > 0) %>% 
  #filter(species == "Vireo_griseus") %>% 
  ggplot() +
  geom_point(aes(x = earlag, y = AnomVArr)) +
  geom_smooth(data = bg.matBG4 %>% filter(earlag > 0),
              aes(x = earlag, y = AnomVArr)) +
  geom_smooth(data = bg.matBG4 %>% filter(earlag < 0),
              aes(x = earlag, y = AnomVArr)) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() #+
  #facet_rep_wrap(~species, scales = "free_y")

hist(bg.matBG4$earlag)
x <- 0.75
bg.matBG4 %>% 
   filter(earlag < -x) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  #geom_point(aes(x = earlag, y = vArrMag_log)) +
  geom_point(aes(x = cell_lat, y = AnomVArr)) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr), se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() #+coord_cartesian(ylim = c(4, 8))

bg.matBG4 %>% 
  filter(earlag > -x,
         earlag < x) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  #geom_point(aes(x = earlag, y = vArrMag_log)) +
  geom_point(aes(x = cell_lat, y = AnomVArr)) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr), se = F) +  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() #+coord_cartesian(ylim = c(4, 8))

bg.matBG4 %>% 
  filter(
         earlag > x) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  #geom_point(aes(x = earlag, y = vArrMag_log)) +
  geom_point(aes(x = cell_lat, y = AnomVArr)) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr), se = F) +  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() #+coord_cartesian(ylim = c(4, 8))


bg.matBG4 %>% 
  filter(!is.na(year_g)) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  #geom_point(aes(x = cell_lat, y = vArrMag_log, col = year_g), show.legend = F) +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log, col = year_g)) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() +
  facet_rep_wrap(~who1, scales = "free_y")
 

bg.matBG4 %>% 
  filter(!is.na(year_g)) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vGrMag_log, col = year_g), show.legend = F) +
  geom_smooth(aes(x = cell_lat, y = vGrMag_log, col = year_g), se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw()

## do gu anomalies causes speed up?  ----------------------------

hist(bg.matBG4$AnomDGr2, breaks = 32)

bg.matBG4 %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr2, y = AnomVArr), alpha = 0.3, show.legend = F) +
  geom_smooth(aes(x = AnomDGr2, y = AnomVArr), show.legend = F, se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  theme_bw()#+
#facet_rep_wrap(~species#, scales = "free_y"
#               )
bg.matBG4 %>% 
  filter(AnomDGr2 < -0.05) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  geom_point(aes(y = AnomVArr, x = cell_lat),alpha = 0.3, show.legend = F) +
  geom_smooth(aes(y = AnomVArr, x = cell_lat), show.legend = F, se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() + coord_cartesian(ylim = c(-3,4))

bg.matBG4 %>% 
  filter(AnomDGr2 > -0.05,
         AnomDGr2 < 0.05) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  geom_point(aes(y = AnomVArr, x = cell_lat),alpha = 0.3, show.legend = F) +
  geom_smooth(aes(y = AnomVArr, x = cell_lat), show.legend = F, se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() + coord_cartesian(ylim = c(-3,4)) + facet_rep_wrap(~species)

bg.matBG4 %>% 
  filter(AnomDGr2 > 0.05) %>% 
  #filter(species == "Setophaga_ruticilla") %>% 
  ggplot() +
  geom_point(aes(y = AnomVArr, x = cell_lat),alpha = 0.3, show.legend = F) +
  geom_smooth(aes(y = AnomVArr, x = cell_lat), show.legend = F, se = F) +
  #geom_hline(yintercept = 0, color = "black") +
  theme_bw() + coord_cartesian(ylim = c(-3,4))



ggplot(data = (final %>%
                 mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                                           ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                                  ifelse(winlat > 13, "Short", "opps"))))) %>% 
         filter(species != "Setophaga_tigrina",
                !is.na(mig_cell), 
                #winlatcat != "Long"
                )) +
  geom_point(aes(x = cell_lat, 
                  y = log(vArrMag), alpha = 0.9#, col = winlatcat
  )) +
  geom_smooth(aes(x = cell_lat, 
                  y = log(vArrMag)#, col = winlatcat
                  ), formula = y ~ splines::bs(x, 3), 
              se = FALSE, show.legend = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  #labs(title="Species", x = "Latitude (degrees)", y = "Speed anomaly") +
  scale_color_viridis(discrete=TRUE, 
                      name = "Wintering\nLatitude\n(degrees)\n") +
  facet_rep_wrap(~mig_cell + winlatcat, scales = "free_y")

