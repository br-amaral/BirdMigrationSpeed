### Migration time ---------------------------------------------------------------------------------------------------------
newdata <- data.frame(xi_mean = rep(mean(final4$xi_mean, na.rm = T),2),
                      ea_lat = rep(mean(final4$ea_lat, na.rm = T),2),
                      Body_mass_g = rep(mean(final4$Body_mass_g, na.rm = T),2),
                      winlat = rep(mean(final4$winlat, na.rm = T),2),
                      Diet = rep(c("Insectivore"),2), 
                      Time = na.omit(unique(final4$Time)),
                      HWI = rep(mean(final4$HWI, na.rm = T),2))

newdata$species <- 1
newdata$year <- 1
newdata$cell_lat2 <- 1
newdata$cell <- 1
newdata$sps_cell <- 1


(yhat.inctr2 <- predict(mod_tra, newdata = newdata, se.fit = TRUE, iterms.type=2, newdata.guaranteed=TRUE,
                        #type=c("terms"),
                        exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)"
                                        #"xi_mean","ea_lat","Body_mass_g","winlat","Diet","HWI")#, re.form=NA)
                        )))

time_efftab <- as.data.frame(matrix(ncol = 3,
                                    data = c(yhat.inctr2$fit, 
                                             yhat.inctr2$fit + yhat.inctr2$se.fit,
                                             yhat.inctr2$fit - yhat.inctr2$se.fit),
                                    byrow = F)) 

colnames(time_efftab) <- c("mean", "up","low")
time_efftab$time <- na.omit(unique(final4$Time))

# svg(glue("figures/Fig4/mod_tra_Time.svg"), 
#     width = 3.5, height = 3)

ggplot(aes(x = time, y = mean), data = time_efftab) +
  geom_point(size = 2.2, col = "#CC79A7",) +
  geom_errorbar(aes(ymin=low, ymax=up), width=.1, col = "#CC79A7",
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
  scale_y_continuous(trans='log10', 
                     limits = c(3,5), 
                     breaks = log(c(25, 50, 75, 100, 150)),
                     labels = c(25, 50, 75, 100, 150)) +
  labs(y = "Bird speed (km/day, log scale)") +
  scale_x_discrete(labels = c("Diurnal","Nocturnal")) 


### effect of diet on migration speed -----------------------------
newdata_d <- data.frame(xi_mean = rep(mean(final4$xi_mean, na.rm = T),4),
                      ea_lat = rep(mean(final4$ea_lat, na.rm = T),4),
                      Body_mass_g = rep(mean(final4$Body_mass_g, na.rm = T),4),
                      winlat = rep(mean(final4$winlat, na.rm = T),4),
                      Diet = unique(final4$Diet), 
                      Time = rep(c("nocturnal"),4),
                      HWI = rep(mean(final4$HWI, na.rm = T),2))

newdata_d$species <- 1
newdata_d$year <- 1
newdata_d$cell_lat2 <- 1
newdata_d$cell <- 1
newdata_d$sps_cell <- 1

(yhat.inctr <- predict(mod_tra, newdata = newdata_d, se.fit = TRUE, iterms.type=2, newdata_d.guaranteed=TRUE,
                                    #type=c("terms"),
                                    exclude = list("s(year)","s(cell_lat2)","s(sps_cell)", "s(cell)"
                                                   #"xi_mean","ea_lat","Body_mass_g","winlat","Diet","HWI")#, re.form=NA)
                                    )))

tra_efftab <- as.data.frame(matrix(ncol = 3,
                                   data = c(yhat.inctr$fit, 
                                            yhat.inctr$fit + yhat.inctr$se.fit,
                                            yhat.inctr$fit - yhat.inctr$se.fit),
                                   byrow = F)) 

colnames(tra_efftab) <- c("mean", "up","low")
tra_efftab$diet <- unique(final4$Diet)


ggplot(aes(x = diet, y = mean), data = tra_efftab) +
  geom_point(size = 2.2, col = "#CC79A7") +
  geom_errorbar(aes(ymin=low, ymax=up), width=.1, col = "#CC79A7",
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
                     limits = c(2.9,5.2), 
                     breaks = log(c(25, 50, 100, 150)), labels = c(25, 50, 100, 150)) +
  labs(y = "Bird speed (km/day, log scale)")







