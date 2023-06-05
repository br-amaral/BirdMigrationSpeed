library(tidyverse)
library(RColorBrewer)
library(glue)

# source 'base' maps ----------------------
# create map
source("~/Documents/GitHub/BirdMigrationSpeed/map.R")
rm(list= ls()[!(ls() %in% c('rr', 'rrb', 'pp'))])

# import data ----------------------
velB <- read_rds("data/velocityB.rds")
velG <- read_rds("data/velocityG.rds")

# full data set
bg.mat <- read_rds("data/birdgreen.rds") %>% 
  mutate(vArrMag_log = log(vArrMag),
         vGrMag_log = log(vGrMag)) %>% 
  rename(cell_lat_s = cell_lat) %>% 
  dplyr::select(-cell_lng)

# only late years
bg.mat <- read_rds("data/birdgreenLATE.rds") %>% 
  mutate(vArrMag_log = log(vArrMag),
         vGrMag_log = log(vGrMag))%>% 
  rename(cell_lat_s = cell_lat) %>% 
  dplyr::select(-cell_lng)

## cell coordinates
cells <- read_rds("data/cellcoor.rds")

bg.mat <- left_join(bg.mat, cells, by="cell")

## plot green up
plot_mapvelG <- function(yearx, maptype){ # maptype = map hex hexn
  
  bg.mat2 <- bg.mat %>% 
    dplyr::select(year, cell, cell_lat, cell_lng, gr_mn, gr_ncell,
                  NGr, vGrMag, vGrAng, angG, xG, yG, vGrMag_log) %>% 
    unique()
  
  if(yearx == "all") {preds2 <- bg.mat2}
  
  if(yearx != "all") {preds2 <- bg.mat2 %>% 
    filter(year == yearx)}
  
  cell_sps <- sort(unique(velB$cell))
  
  ## to plot, all arrows should be thesame size
  preds2 <- preds2 %>% 
    mutate(
      # x = (cell_lng + 10) * cos(ang * pi / 180),
      # y = (cell_lat + 10) * sin(ang * pi / 180)
      xP = cell_lng + 2.5 * cos(angG * pi / 180),
      yP = cell_lat + 2.5 * sin(angG * pi / 180)
    )
  
  
  preds3 <- as.data.frame(matrix(data = NA, ncol = 10, nrow = length(cell_sps)))
  colnames(preds3) <- c("year","cell","cell_lng","cell_lat","xG","yG","xP","yP","vGrMag")
  
  for(i in 1:length(cell_sps)){
    celll <- cell_sps[i]
    pred_loop <- preds2 %>% filter(cell == celll)
    if(nrow(pred_loop) == 1) {
      preds3$year[i] <- pred_loop$year
      preds3$cell[i] <- pred_loop$cell
      preds3$cell_lng[i] <- pred_loop$cell_lng
      preds3$cell_lat[i] <- pred_loop$cell_lat
      preds3$xG[i] <- pred_loop$xG
      preds3$yG[i] <- pred_loop$yG
      preds3$xP[i] <- pred_loop$xP
      preds3$yP[i] <- pred_loop$yP
      preds3$vGrMag[i] <- pred_loop$vGrMag
    } else {
      preds3$year[i] <- pred_loop$year[1]
      preds3$cell[i] <- pred_loop$cell[1]
      preds3$cell_lng[i] <- pred_loop$cell_lng[1]
      preds3$cell_lat[i] <- pred_loop$cell_lat[1]
      preds3$xG[i] <- mean(x = pred_loop$xG)
      preds3$yG[i] <- mean(x = pred_loop$yG)
      preds3$xP[i] <- mean(x = pred_loop$xP)
      preds3$yP[i] <- mean(x = pred_loop$yP)
      preds3$vGrMag[i] <-  mean(x = pred_loop$vGrMag, na.rm = T)
    }
  }
  
  preds4 <- preds3 
  
  if(maptype == "hexn") {base_map <- rr}
  
  if(maptype == "hex") {base_map <- rrb}
  
  if(maptype == "map") {base_map <- pp}
  
  base_map +
    geom_segment(data = preds4, aes(x = cell_lng, y = cell_lat, 
                                    xend = xP, yend = yP,
                                    group = cell, colour = log(vGrMag)),
                 arrow = arrow(length = unit(0.1, "cm")), size = 1.2) +
    scale_colour_continuous(type = "viridis", trans = "log"
    ) +
    #scale_fill_binned(type = "viridis") +
    #scale_colour_gradientn(colors=viridis(3)) +
    ggtitle(glue("Green-up {yearx}")) +
    theme_bw() +
    theme(#legend.position = "none",
      panel.grid.major = element_line(color = alpha('black', 0.2),
                                      size = 0.5),
      panel.ontop = TRUE,
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill = NA),
      legend.title=element_text(size=13),
      legend.spacing.y = grid::unit(0.5, "cm"),
      legend.text=element_text(size=rel(0.8)),
      legend.key.height=grid::unit(0.9,"cm"),
      legend.title.align=0.5,
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.border = element_blank()) 
}
  
plot_mapvelG("all","hex")
plot_mapvelG("2011","hex")
plot_mapvelG("2014","hex")
plot_mapvelG("2017","hex")

ggplot(data = bg.mat, aes(x = year, y = AnomVGr), col = "black") +
  #geom_point() +
  geom_boxplot(aes(x = factor(year), y = AnomVGr), fill = "darkolivegreen4") +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  ggtitle(glue("Green-up date anomaly"))



sps <- "Sayornis_phoebe" # late
sps <- "Tachycineta_bicolor" # late
sps <- "Parkesia_motacilla"
sps <- "Setophaga_americana"
sps <- "Myiarchus_crinitus" # middle
sps <- "Setophaga_tigrina" # middle
sps <- "Setophaga_striata" # early
sps <- "Coccyzus_americanus" # early

sps <- "Contopus_virens"  # early
sps <- "Setophaga_dominica"  # late
sps <- "Vireo_olivaceus"  # middle

bg.matX <- bg.mat %>% 
  filter(species == sps) %>% 
  mutate(cell = as.factor(cell),
         AnomVArr = scale(log(vArrMag), scale = FALSE),
         AnomLag = scale(log(lag), scale = FALSE))#%>% 
  #filter(year == 2010)

bg.matX %>% 
  filter(year == 2017) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
    #geom_point(aes(x = cell_lat, y = vArrMag_log)) + geom_smooth(aes(x = cell_lat, y = vArrMag_log)) +
    geom_point(aes(x = cell_lat, y = AnomVArr)) + geom_smooth(aes(x = cell_lat, y = AnomVArr)) + geom_point(aes(x = cell_lat, y = AnomVGr)) + geom_smooth(aes(x = cell_lat, y = AnomVGr)) + 

    #geom_point(aes(x = cell_lat, y = lag)) + geom_smooth(aes(x = cell_lat, y = lag)) + 
    geom_hline(yintercept = 0, color = "red")  +
    #ylim(c(-1,2)) +
    theme_bw() 

(bsl1 <- lmer(data = bg.matX, vArrMag_log ~ cell_lat + (1|cell) + (1|year)))
(bsl1.5<-lmer(data = bg.matX, vArrMag_log ~ cell_lat + I(cell_lat^2) + (1|cell) + (1|year)))
(bsl2 <- lmer(data = bg.matX, vArrMag_log ~ cell_lat + (1|cell)))
(bsl3 <- lmer(data = bg.matX, vArrMag_log ~ cell_lat + (1|year)))
(bsl8 <-   lm(data = bg.matX, vArrMag_log ~ cell_lat))
(bsl9 <- lmer(data = bg.matX, vArrMag_log ~ cell_lat + (1|cell) + (1|year) + AnomLag))

(taic3 <- AIC(bsl1,bsl1.5,bsl2,bsl3,bsl8, bsl9) %>% arrange(AIC))  
sjPlot::tab_model(get(rownames(taic3)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Latitude", "Lag"),
                  dv.labels= glue("Bird Migration Velocity - model {rownames(taic3)[1]}"),
                  digits = 3)


plot(Effect(c("cell_lat"), get(rownames(taic3)[1])))
plot(Effect(c("AnomLag"), get(rownames(taic3)[1])))
## 

(bal1 <- lmer(data = bg.matX, abs(AnomVArr) ~ cell_lat_s + (1|cell) + (1|year)))
(bal1.5<-lmer(data = bg.matX, abs(AnomVArr) ~ cell_lat_s + I(cell_lat_s^2) + (1|cell) + (1|year)))
(bal2 <- lmer(data = bg.matX, abs(AnomVArr) ~ cell_lat_s + (1|cell)))
(bal3 <- lmer(data = bg.matX, abs(AnomVArr) ~ cell_lat_s + (1|year)))
(bal8 <-   lm(data = bg.matX, abs(AnomVArr) ~ cell_lat_s))
(bal9 <- lmer(data = bg.matX, abs(AnomVArr) ~ cell_lat_s + (1|cell) + (1|year) + AnomLag))

(taic4 <- AIC(bal1,bal1.5,bal2,bal3,bal8, bal9) %>% arrange(AIC))  
sjPlot::tab_model(get(rownames(taic4)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Latitude", "Lag"),
                  dv.labels= glue("Bird Migration speed anomaly - model {rownames(taic3)[1]}"),
                  digits = 3)

##

(bll1 <- lmer(data = bg.matX, lag ~ cell_lat + (1|cell) + (1|year)))
(bll1.5<-lmer(data = bg.matX, lag ~ cell_lat + I(cell_lat^2) + (1|cell) + (1|year)))
(bll2 <- lmer(data = bg.matX, lag ~ cell_lat + (1|cell)))
(bll3 <- lmer(data = bg.matX, lag ~ cell_lat + (1|year)))
(bll8 <-   lm(data = bg.matX, lag ~ cell_lat))
(bll9 <- lmer(data = bg.matX, lag ~ cell_lat + (1|cell) + (1|year) + AnomLag))

(taic3 <- AIC(bll1,bll1.5,bll2,bll3,bll8, bll9) %>% arrange(AIC))  
sjPlot::tab_model(get(rownames(taic3)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Latitude", "Lag"),
                  dv.labels= glue("Bird Migration Velocity - model {rownames(taic3)[1]}"),
                  digits = 3)


###

bg.mat %>% 
  dplyr::select(year, cell, cell_lat, cell_lng, gr_mn, gr_ncell,
                NGr, vGrMag, vGrAng, angG, xG, yG, vGrMag_log,
                AnomVGr, AnomDGr, AnomLag) %>% 
  unique() %>% 
  #filter(year == 2017) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = AnomDGr)) +
  #geom_boxplot(aes(x = factor(year), y = AnomVArr)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,2)) +
  theme_bw() +
  ggtitle(glue("{bg.matX$species[1]} speed anomaly")) +
  facet_wrap(~year)





ggplot(data = bg.matX, aes(x = year, y = AnomVArr), col = "black") +
  #geom_point() +
  geom_boxplot(aes(x = factor(year), y = AnomVArr)) +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw() 


bg.matX %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  #geom_point() +
  geom_boxplot(aes(x = factor(year), y = AnomDArr)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,2)) +
  theme_bw() +
  ggtitle(glue("{bg.matX$species[1]} arrival date anomaly"))

bg.matX %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  #geom_point(aes(x = cell_lat, y = vArrMag_log)) +
  geom_boxplot(aes(x = as.factor(year), y = AnomVArr)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  ggtitle(glue("{bg.matX$species[1]} speed")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))# +
  #geom_smooth(aes(x = cell_lat, y = vArrMag_log), method = "lm") 
  #facet_grid(~year)

bg.matX %>% 
  filter(year == 2013) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vArrMag_log)) +
  #geom_boxplot(aes(x = cell_lat, y = vArrMag_log)) +
  geom_hline(yintercept = 0, color = "red")  +
  ylim(c(0,8)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log))#, method = "lm")


bg.matX %>% 
  filter(year == 2013) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = lag)) +
  #geom_boxplot(aes(x = cell_lat, y = vArrMag_log)) +
  geom_hline(yintercept = 0, color = "red")  +
  ylim(c(-30,30)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = cell_lat, y = lag))#, method = "lm")


myPalette <- colorRampPalette(brewer.pal(11, "PuRd"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(25,50))

bg.matX %>% 
  filter(year %in% c(2009,2012)) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = AnomVGr, y = AnomVArr, col = cell_lat)) +
  #scale_colour_gradient2(low = "red", high = "blue", mid = 'green', na.value = NA) +  #geom_boxplot(aes(x = cell_lat, y = vArrMag_log)) +
  geom_hline(yintercept = 0, color = "red")  +
  geom_vline(xintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~year, ncol = 1) + sc 
  #geom_abline(intercept = 0, slope = 1, size = 0.5)
  #geom_smooth(aes(x = AnomDGr, y = AnomDArr, col = cell_lat), method = "lm")

bg.matX %>% 
  filter(year %in% c(2009,2012)) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = AnomVArr, y = lag, col = as.factor(year))) +
  #scale_colour_gradient2(low = "red", high = "blue", mid = 'green', na.value = NA) +  #geom_boxplot(aes(x = cell_lat, y = vArrMag_log)) +
  geom_hline(yintercept = 0, color = "red")  +
  #geom_vline(xintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = AnomVArr, y = lag, group = as.factor(year),
                  col = as.factor(year)), se=F, method = "lm") +
  facet_wrap(~year, ncol = 1)

bg.matX[-which(bg.matX$year == 2014 & bg.matX$AnomDGr < 0),] %>% 
#bg.matX %>% 
  #filter(year %in% c(2009,2010)) %>% 
  filter(between(lag,-20,20)) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr, y = lag, col = as.factor(year))) +
  #scale_colour_gradient2(low = "red", high = "blue", mid = 'green', na.value = NA) +  #geom_boxplot(aes(x = cell_lat, y = vArrMag_log)) +
  geom_hline(yintercept = 0, color = "red")  +
  #geom_vline(xintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = AnomDGr, y = lag, group = as.factor(year)),
                  #col = as.factor(year)), 
              se=F, method = "lm") +
  facet_wrap(~year, ncol = 3)

bg.matX %>% 
  #filter(year %in% c(2009,2010)) %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = AnomDGr, y = lag)) +
  geom_hline(yintercept = 0, color = "red")  +
  #geom_vline(xintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = AnomDGr, y = lag, group = as.factor(year)),
              #col = as.factor(year)), 
              se=F, method = "lm") +
  facet_wrap(~year, ncol = 3)


bg.matX %>% 
  #filter(year == 2010) %>% 
  ggplot() +
  geom_point(aes(x = arr_GAM_mean, y = AnomVArr)) +
  #geom_boxplot(aes(x = cell_lat, y = vArrMag_log)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  #ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = arr_GAM_mean, y = AnomVArr), method = "lm")+
  facet_wrap(~year,ncol = 5)

bg.matX %>% 
ggplot() +
  geom_point(aes(x = cell_lat, y = AnomDArr)) +
  #geom_boxplot(aes(x = factor(year), y = AnomVArr)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,2)) +
  theme_bw() +
  facet_wrap(~year)


bg.matX %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>% 
  ggplot() +
  geom_point(aes(x = AnomVGr, y = AnomVArr)) +
  #geom_boxplot(aes(x = as.factor(cell_lat), y = vArrMag)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  ggtitle(glue("{bg.matX$species[1]} lag")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_smooth(aes(x = AnomVGr, y = AnomVArr), method = "lm")

bg.matX %>% 
  ggplot() +
  geom_point(aes(y = vArrMag_log, x = cell_lat)) +
  geom_smooth(aes(y = vArrMag_log, x = cell_lat), method="lm")

# model with bird speed and arrival date as a effect

colnames(bg.mat)

bg.mat.arr <- bg.mat %>%
  group_by(species, year) %>% 
  mutate(fir_arri_med = median(as.numeric(arr_GAM_mean), na.rm = TRUE)) %>% 
  slice(which.min(cell_lat)) %>% 
  ungroup()

bg.mat.arr %>% 
  #mutate(AnomVArr = scale(vArrMag)) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(species, -arr_GAM_mean, FUN = median, na.rm = TRUE), y = arr_GAM_mean)) +
  #geom_point(aes(x = reorder(species, -fir_arri_med, FUN = median, na.rm = TRUE), y = fir_arri_med), col = "red") +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  ggtitle(glue("arrival dates")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  #geom_smooth(aes(x = cell_lat, y = lag), method = "lm")

bg.mat %>% 
  filter(cell_lat < 38) %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(species, -arr_GAM_mean, FUN = median, na.rm = TRUE), y = arr_GAM_mean)) +
  geom_hline(yintercept = 0, color = "red")  +
  #ylim(c(-1,1000)) +
  theme_bw() +
  ggtitle(glue("arrival dates anomaly")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#geom_smooth(aes(x = cell_lat, y = lag), method = "lm")
# Bird speed and Green-up Speed -----------------
# only latitude is significant - faster north
(bgs1 <- lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + (1|cell)  + (1|year)))
(bgs2 <- lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + (1|cell)))
(bgs4 <- lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + (1|year)))
(bgs6 <- lm(data = bg.matX, vArrMag_log ~ vGrMag_log))
(bgs9 <- lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + cell_lat + (1|cell) ))
(bgs10 <-lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + cell_lat + (1|cell)  + (1|year)))
(bgs11 <-lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + cell_lat + (1|cell)   + (1|year) + AnomLag))
(bgs12 <-lmer(data = bg.matX, vArrMag_log ~ vGrMag_log + (1|cell)   + AnomLag))

head(taic1 <- AIC(bgs1,bgs2,bgs4,bgs6,bgs9,bgs10,bgs11,bgs12) %>% arrange(AIC))  # bgs1 best, but bgs2 is close enough and simpler
sjPlot::tab_model(get(rownames(taic1)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Green-up speed","Latitude", "Lag"),
                  #pred.labels =c("Intercept", "Green-up speed"),
                  dv.labels= glue("Bird Migration Speed - model {rownames(taic1)[1]}",),
                  digits = 3)

get(rownames(taic1)[1])

# lattice::xyplot(vArrMag_log~vGrMag_log | species,  data=bg.matX, type=c('p','r'), auto.key=F)
lattice::xyplot(fitted(get(rownames(taic1)[1]))~eval(parse(text = paste(names(fixef(get(rownames(taic1)[1])))[-1]))) | species, 
                data=bg.matX, type=c('r'), auto.key=F, cex=0.1,
                par.strip.text=list(cex=0.5), 
                xlab = names(fixef(get(rownames(taic1)[1])))[-1],
                ylab = "vArrMag_log")

plot(Effect(c("vGrMag_log"), get(rownames(taic1)[1])))
plot(Effect(c("cell_lat"), get(rownames(taic1)[1])))
plot(Effect(c("AnomLag"), get(rownames(taic1)[1])))

# Bird Speed and Latitude ------------------
bg.matX1 <- bg.matX[-which(bg.matX$year == 2013 &
                          bg.matX$cell == 47 &
                          bg.matX$species == "Vireo_olivaceus"),]
# latitude is significant - faster north
(bsl1 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat + (1|cell)   + (1|year)))
(bsl1.5<-lmer(data = bg.matX1, vArrMag_log ~ cell_lat + I(cell_lat^2) + (1|cell)   + (1|year)))
(bsl2 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat + (1|cell)  ))
(bsl3 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat + (1|cell) + (1|year)))
(bsl4 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat   + (1|year)))
(bsl5 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat + (1|cell) ))
(bsl6 <- lm(data = bg.matX1, vArrMag_log ~ cell_lat ))
(bsl7 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat + (1|year)))
(bsl9 <- lmer(data = bg.matX1, vArrMag_log ~ cell_lat + (1|cell)   + (1|year) + AnomLag))
(bsl10 <-   lm(data = bg.matX1, vArrMag_log ~ AnomLag + cell_lat))
(bsl11 <-   lm(data = bg.matX1, vArrMag_log ~ I(AnomLag^2) + cell_lat))
(bsl12 <-   lmer(data = bg.matX1, vArrMag_log ~ AnomLag + cell_lat+ (1|year)))
(bsl13 <-   lmer(data = bg.matX1, vArrMag_log ~ AnomLag + cell_lat+ (1|cell)))



(taic3 <- AIC(bsl1,bsl1.5,bsl2,bsl3,bsl4,bsl5,bsl6,bsl7, bsl9, bsl10,bsl11, bsl12,bsl13
              ) %>% arrange(AIC))  
sjPlot::tab_model(get(rownames(taic3)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Latitude", "Lag"),
                  dv.labels= glue("Bird Migration Velocity - model {rownames(taic3)[1]}"),
                  digits = 3)

rownames(taic3)[1]



lattice::xyplot(fitted(get(rownames(taic3)[1]))~eval(parse(text = 
                                                             paste(names(fixef(get(rownames(taic3)[1])))[-1]))) | species, 
                data=bg.matX, type=c('r'), auto.key=F, cex=0.1,
                par.strip.text=list(cex=0.5), 
                xlab = names(fixef(get(rownames(taic3)[1])))[-1],
                ylab = "vArrMag_log")

plot(Effect(c("cell_lat"), get(rownames(taic3)[1])))
plot(Effect(c("AnomLag"), get(rownames(taic3)[1])))

bg.matX1 %>% 
  ggplot() +
  geom_point(aes(y = vArrMag_log, x =cell_lat, col = as.factor(year))) +
  geom_smooth(aes(y = vArrMag_log, x =cell_lat, col = as.factor(year)), method = "lm", se = F) +
  facet_rep_wrap(~year, ncol = 3, repeat.tick.labels = T)  

bg.matX1 %>% 
  ggplot() +
  geom_point(aes(x = vGrMag_log, y = AnomLag, col = as.factor(year))) +
  #geom_smooth(aes(y = vArrMag_log, x =AnomLag, col = as.factor(year)), method = "lm", se = F) +
  #geom_smooth(aes(y = vArrMag_log, x =AnomLag, col = as.factor(year)), se = F) +
  facet_rep_wrap(~year, ncol = 3, repeat.tick.labels = T)  

# is lag anomaly because of birds or grren up? :(
(al1 <- lmer(data = bg.matX1, AnomLag ~ vGrMag_log + (1|cell)   + (1|year)))
(al2 <-lmer(data = bg.matX1, AnomLag ~ vArrMag_log + I(cell_lat^2) + (1|cell)   + (1|year)))

AIC(al1,al2)
plot(Effect(c("vGrMag_log"), al1))

bg.matX1 %>% 
  ggplot() +
  geom_point(aes(x = AnomVGr, y = AnomLag, col = as.factor(year))) +
  facet_rep_wrap(~year, ncol = 3, repeat.tick.labels = T)  
  




bg.matX1 %>% 
  ggplot() +
  geom_point(aes(y = vArrMag_log, x =cell_lat, col = as.factor(year))) 


# ANOMALIES  -----------------------------
# Bird speed with Lag --------------------------
# nothing is significant with log lag
(bsl1 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr + (1|cell)   + (1|year)))
(bsl2 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr + (1|cell)  ))
(bsl3 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr + (1|cell) + (1|year)))
(bsl4 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr   + (1|year)))
(bsl5 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr + (1|cell) ))
(bsl6 <- lm(data = bg.matX, AnomVArr ~ AnomDGr  ))
(bsl7 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr + (1|year)))
(bsl8 <- lm(data = bg.matX, AnomVArr ~ AnomDGr))
(bsl9 <- lmer(data = bg.matX, AnomVArr ~ AnomDGr + cell_lat + (1|cell) ))

(taic5 <- AIC(bsl1,bsl2,bsl3,bsl4,bsl5,bsl6,bsl7,bsl8,bsl9) %>% arrange(AIC))  
sjPlot::tab_model(get(rownames(taic5)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Lag"),
                  dv.labels= glue("Bird speed - model {rownames(taic5)[1]}"),
                  digits = 3)

rownames(taic5)[1]

plot(Effect(c("AnomVGr"), get(rownames(taic5)[1])))

lattice::xyplot(fitted(get(rownames(taic5)[1]))~eval(parse(text = 
                                                             paste(names(fixef(get(rownames(taic5)[1])))[-1]))) | species, 
                data=bg.matX, type=c('r'), auto.key=F, cex=0.1,
                par.strip.text=list(cex=0.5), 
                xlab = names(fixef(get(rownames(taic5)[1])))[-1],
                ylab = "vArrMag_log")

# Bird speed and Green-up Speed anomaly-----------------
# only latitude is significant - faster north
(bgs1 <- lmer(data = bg.matX, AnomVArr ~ AnomVGr + (1|cell)  + (1|year)))
(bgs2 <- lmer(data = bg.matX, AnomVArr ~ AnomVGr + (1|cell)))
(bgs4 <- lmer(data = bg.matX, AnomVArr ~ AnomVGr + (1|year)))
(bgs6 <- lm(data = bg.matX, AnomVArr ~ AnomVGr))
(bgs9 <- lmer(data = bg.matX, AnomVArr ~ AnomVGr + cell_lat + (1|cell) ))
(bgs10 <-lmer(data = bg.matX, AnomVArr ~ AnomVGr + cell_lat + (1|cell)  + (1|year)))
(bgs11 <-lmer(data = bg.matX, AnomVArr ~ AnomVGr + cell_lat + (1|cell)   + (1|year) + AnomLag))
(bgs12 <-lmer(data = bg.matX, AnomVArr ~ AnomVGr + (1|cell)   + AnomLag))

head(taic1 <- AIC(bgs1,bgs2,bgs4,bgs6,bgs9,bgs10,bgs11,bgs12) %>% arrange(AIC))  # bgs1 best, but bgs2 is close enough and simpler
sjPlot::tab_model(get(rownames(taic1)[1]),
                  show.re.var= TRUE, 
                  #pred.labels =c("Intercept", "Green-up speed","Latitude", "Lag"),
                  #pred.labels =c("Intercept", "Green-up speed"),
                  dv.labels= glue("Bird Speed Anomaly- model {rownames(taic1)[1]}",),
                  digits = 3)

get(rownames(taic1)[1])

# lattice::xyplot(vArrMag_log~vGrMag_log | species,  data=bg.matX, type=c('p','r'), auto.key=F)
lattice::xyplot(fitted(get(rownames(taic1)[1]))~eval(parse(text = paste(names(fixef(get(rownames(taic1)[1])))[-1]))) | species, 
                data=bg.matX, type=c('r'), auto.key=F, cex=0.1,
                par.strip.text=list(cex=0.5), 
                xlab = names(fixef(get(rownames(taic1)[1])))[-1],
                ylab = "vArrMag_log")

plot(Effect(c("AnomDGr"), get(rownames(taic1)[1])))
plot(Effect(c("cell_lat"), get(rownames(taic1)[1])))
plot(Effect(c("AnomLag"), get(rownames(taic1)[1])))








