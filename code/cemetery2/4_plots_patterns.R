library(egg)
library(ggplot2)
library(tidyverse)
library(viridis)
library("glue")
library(lemon)
library(lme4)

final <- readRDS("data/birdgreen.rds")
cells <- read_rds("data/cellcoor.rds")
velocityG <- readRDS("data/velocityG.rds") %>% 
  left_join(., cells, by = "cell")

## medians of arrival dates and fast and slow dates instead of categories
finalG <- final %>% 
  select(year, cell, cell_lat, gr_mn, vGrMag, AnomDGr, AnomVGr) %>% 
  group_by(year) %>% 
  summarise(medarr = median(gr_mn, na.rm = T),
            med_spe = median(vGrMag, na.rm = T))

final <- final %>% 
  left_join(., finalG, by = "year")

## Which birds are fast and slow?  --------------------------

(p2 <- ggplot(data = final) +
  #geom_jitter(aes(x = reorder(species, log(vArrMag), FUN = median, na.rm = TRUE), 
  #                y = log(vArrMag), 
  #                color = migdist, alpha = 0.9), height = 0.25, stroke=0) +
  geom_boxplot(aes(x = reorder(species2, log(vArrMag), FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = winlat),
               width=0.5, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        #legend.position = "none",
        #legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, face = "italic")) +
  labs(title="Species") +
   scale_fill_viridis(name = "Wintering\nLatitude\n(degrees)\n") 
 )

final2 <- final %>% 
  mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                            ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                   ifelse(winlat > 13, "Short", "opps"))))
final2$winlatcat <- factor(final2$winlatcat, ordered = TRUE,
                      levels = c("Short","Medium","Long"))

(p1 <- ggplot(data = final2) +
  geom_boxplot(aes(x = winlatcat, 
                   y = log(vArrMag), fill = winlatcat),
               width=0.4, alpha = 0.7) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        #legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_x_continuous(breaks = seq(-3, 1, 0.5),
  #                   limits = c(-3, 1, 0.5)) +
  labs(title="Migration distance", y="Log(Bird speed)")) 

svg(glue("figures/mig_speed_sps_dist.svg"), 
    width = 10, height = 4)

egg::ggarrange(p1, p2, ncol = 2, 
               top = "Migration speed", widths = c(2, 12))

dev.off()

## how speed is changing across latitude?   ------------------------------

(p3 <- ggplot(data = (final %>%
                        mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                                                  ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                                         ifelse(winlat > 13, "Short", "opps")))))) +
    #geom_point(aes(x = cell_lat, 
    #                 y = log(vArrMag), col = species),
    #           alpha = 0.3, stroke=0, show.legend = FALSE) +
    geom_smooth(aes(x = cell_lat, 
                    y = log(vArrMag), col = winlatcat), 
                    #se = FALSE,
                show.legend = TRUE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title=element_text(size=10),
          #legend.position = "none",
          #legend.justification = "right",
          #legend.margin=margin(0,0,0,0),
          #legend.box.margin=margin(-5,0,-5,-7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title.align = 0.5,
          plot.title = element_text(hjust = 0.5, size=10)) +
    #scale_x_continuous(breaks = seq(-3, 1, 0.5),
    #                   limits = c(-3, 1, 0.5)) +
    labs(title="Group mean", x = "Latitude (degrees)", y = "Log(Bird Speed)") +
    scale_color_viridis(discrete=TRUE, 
                        name = "Wintering\nLatitude\n(degrees)\n") +
    ylim(c(4,7)) + 
    guides(color=guide_legend(override.aes=list(fill=NA)))
  )

(p4 <- ggplot(data = (final %>%
                        mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                                                  ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                                         ifelse(winlat > 13, "Short", "opps"))))) %>% 
                filter(species != "Setophaga_tigrina")) +
    #geom_point(aes(x = cell_lat, 
    #                 y = log(vArrMag), col = species),
    #           alpha = 0.3, stroke=0, show.legend = FALSE) +
    geom_smooth(aes(x = cell_lat, 
                    y = log(vArrMag), col = species), 
                se = FALSE, show.legend = TRUE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #legend.title = element_blank(),
          legend.position = "none",
          #legend.justification = "right",
          #legend.margin=margin(0,0,0,0),
          #legend.box.margin=margin(-5,0,-5,-7),
          axis.title.x = element_text(hjust = 0.7),
          #axis.title.y = element_blank(),
          legend.title.align = 0.5,
          plot.title = element_text(hjust = 0.5, size=10)) +
    labs(title="Species", x = "Latitude (degrees)", y = "Log(Bird Speed)") +
    scale_color_viridis(discrete=TRUE, 
                        name = "Wintering\nLatitude\n(degrees)\n") +
    facet_rep_wrap(~winlatcat) +
    ylim(c(4,7))
)

svg(glue("figures/mig_speed_latitude.svg"), 
    width = 11, height = 4)

egg::ggarrange(p4, p3, ncol = 2, 
               top = "Migration speed across latitude", 
               widths = c(10, 3))

dev.off()

## are sensitive birds faster? ------------------

p5 <- ggplot(final %>% 
        mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                                  ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                         ifelse(winlat > 13, "Short", "opps")))) %>% 
        filter(species != "Setophaga_tigrina")) +
  geom_point(aes(x = sensi, y = log(vArrMag), col = winlat, stroke=0), 
             alpha = 0.7, size = 2.3) +
  theme_bw() +
  scale_color_viridis(name = "Wintering\nLatitude\n(degrees)\n") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=9),
        axis.title.x = element_text(hjust = 1.85),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
 # facet_rep_wrap(~winlatcat) + 
  labs(title="Species", x = "Species sensitivity", y = "Log(Bird Speed)")
  
p6 <- ggplot(final %>% 
  mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                            ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                   ifelse(winlat > 13, "Short", "opps")))) %>% 
  filter(species != "Setophaga_tigrina")) +
  theme_bw() +
  geom_smooth(aes(x = sensi, 
                  y = log(vArrMag), col = winlatcat)) +
  scale_color_viridis(discrete=TRUE, name = "Migrant\nGroups") + 
  guides(color=guide_legend(override.aes=list(fill=NA)))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  labs(title="Group models", x = "Species sensitivity", y = "Log(Bird Speed)")

svg(glue("figures/mig_speed_sensi.svg"), 
    width = 10, height = 4)

egg::ggarrange(p5, p6, ncol = 2, 
               top = "Migration speed and sensitivity to green-up", 
               widths = c(4, 4))

dev.off()
## color regression line according to migration latitude 
# average for each sensitivity?
ggplot(final %>% 
         mutate(winlatcat = ifelse(winlat < (-5), 'Long', 
                                   ifelse(winlat > (-5) & winlat < 13, "Medium", 
                                          ifelse(winlat > 13, "Short", "opps")))) %>% 
         filter(species != "Setophaga_tigrina") %>% 
         filter(!is.na(vArrMag)) %>% 
         mutate(pred = predict(loess(log(vArrMag)~sensi)))) +
  geom_line(aes(x = sensi, 
                y = pred, col = winlat), size = 2) +
  scale_color_viridis(name = "Wintering\nLatitude\n(degrees)\n") + 
  #guides(color=guide_legend(override.aes=list(fill=NA)))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5, size=10)) +
  theme_bw() +
  labs(title="Group models", x = "Species sensitivity", y = "Log(Bird Speed)")

dev.off()

## green up! --------------
## does green up moves faster than birds?
velocityG %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = log(vGrMag), col = as.factor(year)), alpha = 0.5) + 
  theme_bw() +
  geom_smooth(aes(x = cell_lat, y = log(vGrMag), col = as.factor(year)), se = FALSE) +
  geom_smooth(aes(x = cell_lat, y = log(vGrMag)), col = "black", size = 1.5) +
  labs(title="Green-up speed", x = "Latitude", y = "Log(Green-up speed)")  +
  scale_color_viridis(discrete=TRUE, 
                      name = "Year")
final %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = log(vGrMag), col = year_g), alpha = 0.5) +
  geom_smooth(aes(x = cell_lat, y = log(vGrMag), col = year_g)) +
  labs(title="Early and late years", y = "Log(Green-up speed)", x = "Latitude") +
  #scale_color_viridis(name = "Latitude\n(degrees)\n") +
  theme_bw()

## are birds following green-up speed
final %>% 
  ggplot() +
  geom_point(aes(y = log(vArrMag), x = log(vGrMag), col = winlat), alpha = 0.5) +
  geom_smooth(aes(y = log(vArrMag), x = log(vGrMag)), col = "black") +
  labs(title="Speed", y = "Log(Bird speed)", x = "Log(Green-up speed)")  +
  scale_color_viridis(name = "Wintering\nLatitude\n(degrees)\n") +
  facet_rep_wrap(~year) +
  theme_bw()

final2 %>% 
  ggplot() +
  geom_point(aes(y = log(vArrMag), x = log(vGrMag), col = winlatcat), alpha = 0.5) +
  geom_smooth(aes(y = log(vArrMag), x = log(vGrMag), col = winlatcat)) +
  labs(title="Speed", y = "Log(Bird speed)", x = "Log(Green-up speed)")  +
  scale_color_viridis(discrete=TRUE, 
                      name = "Year") +
  theme_bw()

## years - fast/slow and early/late
## green up arrival date and speed
final2 %>% 
  ggplot() +
  geom_point(aes(x = gr_mn, y = log(vGrMag), col = cell_lat), alpha = 0.5) +
  geom_smooth(aes(x = gr_mn, y = log(vGrMag)), col = "black") +
  labs(title="Green-up", y = "Log(Green-up speed)", x = "Green-up arrival date")  +
  scale_color_viridis(name = "Latitude\n(degrees)\n") +
  facet_rep_wrap(~year) +
  theme_bw()

## early and late years and bird speed
final2 %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = log(vArrMag), col = year_g), alpha = 0.5) +
  geom_smooth(aes(x = cell_lat, y = log(vArrMag), col = year_g)) +
  labs(title="Early and late years", y = "Log(Bird speed)", x = "Latitude") +
  #scale_color_viridis(name = "Latitude\n(degrees)\n") +
  facet_rep_wrap(~winlatcat) +
  theme_bw()

final3 <- rbind(
  final2 %>% 
    filter(winlatcat == "Medium") %>% 
    filter(!is.na(vArrMag)) %>% 
    mutate(pred = predict(loess(log(vArrMag)~cell_lat))),
  final2 %>% 
    filter(winlatcat == "Short") %>% 
    filter(!is.na(vArrMag)) %>% 
    mutate(pred = predict(loess(log(vArrMag)~cell_lat))),
  final2 %>% 
    filter(winlatcat == "Long") %>% 
    filter(!is.na(vArrMag)) %>% 
    mutate(pred = predict(loess(log(vArrMag)~cell_lat)))
)

ggplot(final3) +
  geom_point(aes(x = cell_lat, y = log(vArrMag), col = medarr), alpha = 0.5, size = 2, stroke = 0) +
  geom_line(aes(x = cell_lat, y = pred, col = medarr), size = 2) +
  labs(title="Early and late years", y = "Log(Bird speed)", x = "Latitude") +
  scale_color_viridis(name = "Median GU\nArrival date") +
  facet_rep_wrap(~winlatcat) +
  theme_bw()

moda_1 <- lmer(data = final2, log(vArrMag) ~ cell_lat + winlat + gr_mn + log(vGrMag) +
                 (1|species) + (1|year))
sjPlot::tab_model(moda_1,
                  show.re.var= TRUE, 
                  digits = 3)

## 
final2 %>% 
  filter(!is.na(vArrMag)) %>% 
  mutate(pred = predict(loess(log(vArrMag)~cell_lat))) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = log(vArrMag), col = winlat), alpha = 0.5, stroke = 0, size = 1.5) +
  geom_line(aes(x = cell_lat, y = pred#, col = winlat
                ), size = 2) +
  labs(title="Early and late years", y = "Log(Bird speed)", x = "Latitude") +
  scale_color_viridis(name = "Wintering\nLatitude\n(degrees)\n") +
  theme_bw()


