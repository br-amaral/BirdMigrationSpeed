## body mass and speed ----------------------------
mod_a2 <- lmer(data = final, log(vArrMag) ~ winlat + 
                 sensi + bre_lat_mea + 
                 #bre_lat_max + 
                 b_datea_mea + 
                   Body_mass_g + Distance_m +
                 (1|species) + (1|year) + 
                 cell_lat + (1|cell)
               ) 

sjPlot::tab_model(mod_a2, show.re.var= TRUE, digits = 3)

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
  geom_smooth(aes(x = Body_mass_g, y = log(vArrMag)), col = "black") +
  geom_smooth(aes(x = Body_mass_g, y = log(vArrMag)), col = "gray", method = "lm")

egg::ggarrange(pbm3, pbm2, pbm1, ncol = 3, 
               top = "Migration speed", widths = c(3, 2, 12))

## bird speed and green up ---------------------------------------
mod_b1 <- lmer(data = final, log(vArrMag) ~ #gr_mn + 
                 AnomDGr + #gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 #log(vGrMag) + AnomVGr + gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 (1|species) + #(1|year) + 
                 cell_lat + (1|cell))
mod_b2 <- lm(data = final, AnomVArr ~ #gr_mn + 
                 AnomDGr + #gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 log(vGrMag) + AnomVGr + #gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 #(1|species) + (1|year) + 
                 cell_lat #+ (1|cell)
               )
sjPlot::tab_model(mod_b2, show.re.var= TRUE, digits = 3)

AIC(mod_b1,mod_b2)
# gr_mn positive effect - later in season, the faster they are (proxy for time or latitude?)
# AnomDGr negative effect - early years they move faster, in later years they move slower
mod_b1g <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               log(vArrMag) ~ gr_mn + log(vGrMag) + AnomDGr + #gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 AnomVGr + #gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 s(species, bs = "re") + 
                 #s(year, bs = "re") + 
                 cell_lat + s(cell, bs = "re"))
mod_b2g <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               log(vArrMag) ~ #gr_mn + log(vGrMag) + 
                 AnomDGr + #gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 #AnomVGr + gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 s(species, bs = "re") + 
                 #s(year, bs = "re") + 
                 s(cell_lat, bs = "tp") + s(cell, bs = "re"))
AIC(mod_b1g, mod_b2g)
summary(mod_b2g)

# gu_dat_int.   positive - the earlier green up year is, the fast birds are
# gu_dat_slo**  negative - the less variation in green up date within a year, the faster the birds are? what about positive and negative slopes? abs()?

# ano_gu_dat_slo  negative - should I have the slopes abs??
#      slow and fast years on average (difference between each years slope with the mean slope for all years)
#      the more variation in green up arrival date within years, the smaller the bird speed

# gu_spe_int.   negative - the faster green up year is, the slower birds are?
# gu_spe_slo**  positive - the more variation in speed within an year, the faster the birds are

# ano_gu_spe_int.   negative - same as before, but standardized (anomaly)
# ano_gu_spe_slo**  positive - 

## same but with GU anomaly
mod_b1gA <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                log(vArrMag) ~ gr_mn + log(vGrMag) + AnomDGr + gu_datA_int + gu_datA_slo + #ano_guA_dat_int + ano_guA_dat_slo + 
                  gu_dat_int + gu_dat_slo + #ano_gu_dat_int + ano_gu_dat_slo +
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re"))
mod_b2gA <- gam(data = final %>% 
                  mutate(species = as.factor(species),
                         cell = as.factor(cell)), 
                log(vArrMag) ~ gr_mn + log(vGrMag) + AnomDGr + gu_datA_int + gu_datA_slo #+ ano_guA_dat_int + ano_guA_dat_slo + 
                  #AnomVGr + gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                  s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re"))
AIC(mod_b1gA, mod_b2gA, mod_b1g, mod_b2g)
summary(mod_b1gA)


#gu_datA_int. - negative: early years are faster
#gu_datA_slo*** - positive: more variation within an year in GU anomaly arrival makes mig faster
# ??ano_guA_dat_int. - negative:
# ??ano_guA_dat_slo*** - positive:
# not anymore - gu_dat_int* - positive:
#gu_dat_slo*** - negative:the more variation in time of arrival, 
# ??ano_gu_dat_slo*** - negative:



## bird mean early arrival date and bird speed ---------------------------
mod_a1 <- lm(data = final, log(vArrMag) ~ b_datea_mea + b_datea_med + b_datea_var + b_datea_cat) 

mod_a2 <- lmer(data = final, log(vArrMag) ~ b_datea_mea + b_datea_med + b_datea_var + b_datea_cat +
                 (1|species) + (1|year) + (1|cell_lat) + (1|cell)) 

head(aic_a <- AIC(mod_a1, mod_a2
) %>% arrange(AIC)) 

sjPlot::tab_model(mod_a2, show.re.var= TRUE, digits = 3)

mod_ag <- gam(data = final %>% 
                mutate(species = as.factor(species),
                       cell = as.factor(cell)), 
              log(vArrMag) ~ b_datea_mea + b_datea_med + b_datea_var + b_datea_cat +
                s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re")) 
summary(mod_ag)

# b_datea_var - negative, but very small - the more there is variation in when they arrive, the slower they are

## speed in migration vs breeding range ----------------------------
mod_ran1 <- lmer(data = final,
                 log(vArrMag) ~ cell_type +
                   (1|species) + (1|year) + (1|cell_lat) + (1|cell))
mod_ran2 <- lmer(data = final,
                 log(vArrMag) ~ cell_type * cell_lat +
                   (1|species) + (1|year) + (1|cell))
mod_ran3 <- lmer(data = final,
                 log(vArrMag) ~ mig_cell +
                   (1|species) + (1|year) + (1|cell_lat) + (1|cell))

mod_ran1g <- gam(data = final %>% 
                   mutate(cell_type = as.factor(cell_type),
                          species = as.factor(species),
                          cell = as.factor(cell)),
                 log(vArrMag) ~ cell_type +
                   s(species, bs = 're') + s(year, bs = 're') + s(cell_lat, bs = 're') + s(cell, bs = 're')) 

mod_ran2g <- gam(data = final %>% 
                   mutate(cell_type = as.factor(cell_type),
                          species = as.factor(species),
                          cell = as.factor(cell)),
                 log(vArrMag) ~ cell_type * cell_lat +
                   s(species, bs = 're') + s(year, bs = 're') + s(cell, bs = 're')) 

mod_ran3g <- gam(data = final %>% 
                   mutate(cell_type = as.factor(cell_type),
                          species = as.factor(species),
                          cell = as.factor(cell)),
                 log(vArrMag) ~ mig_cell +
                   s(species, bs = 're') + s(year, bs = 're') + s(cell, bs = 're')) 

head(aic_ran <- AIC(mod_ran1, mod_ran2, mod_ran3) %>% arrange(AIC)) 

sjPlot::tab_model(mod_ran3, show.re.var= TRUE, digits = 3)

head(aic_ran <- AIC(mod_ran1g, mod_ran2g, mod_ran3g) %>% arrange(AIC)) 
summary(mod_ran1g)

final %>% 
  filter(!is.na(cell_type)) %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = log(vArrMag), col = cell_type), alpha = 0.2) +
  geom_smooth(aes(x = cell_lat, y = log(vArrMag), col = cell_type)) +
  theme_bw()

a <- ggplot() +
  #geom_point(data = final %>% filter(breed_cell == T, mig_cell == F), aes(x = cell_lat, y = log(vArrMag)), col = "black") +
  #geom_point(data = final %>% filter(mig_cell == T, breed_cell == F), aes(x = cell_lat, y = log(vArrMag)), col = "gray") +
  geom_smooth(data = final %>% filter(breed_cell == T, mig_cell == F), aes(x = cell_lat, y = log(vArrMag)), col = "black") +
  geom_smooth(data = final %>% filter(mig_cell == T, breed_cell == F), aes(x = cell_lat, y = log(vArrMag)), col = "gray") +
  theme_bw()

b <- ggplot() +
  geom_smooth(data = final %>% filter(breed_cell == T), aes(x = cell_lat, y = log(vArrMag)), col = "black") +
  geom_smooth(data = final %>% filter(mig_cell == T), aes(x = cell_lat, y = log(vArrMag)), col = "gray") +
  theme_bw()

egg::ggarrange(a, b, ncol = 2, 
               top = "Migration speed in mig and breed ranges", widths = c(3,3))
nrow(final %>% filter(breed_cell == T, mig_cell == F)) # 12331
nrow(final %>% filter(breed_cell == F, mig_cell == T)) # 964


## anomaly on speed and green up (GAM) -----------------------------------
mod_b2g <- gam(data = final %>% 
                 mutate(species = as.factor(species),
                        cell = as.factor(cell)), 
               log(vArrMag) ~ gr_mn + log(vGrMag) + AnomDGr + AnomVGr +
                 gu_spe_int + gu_spe_slo + ano_gu_spe_int + ano_gu_spe_slo + 
                 gu_dat_int + gu_dat_slo + ano_gu_dat_int + ano_gu_dat_slo + 
                 s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "re") + s(cell, bs = "re")) 
summary(mod_b2g)

# gu_dat_int* - negative: in later years (or latitude? proxy for time), birds are slower
# gu_dat_slo** - negative: the less variation in green up dates within an year, the faster the birds are

# ano_gu_dat_int** - positive: in early years, birds are slower 
# ano_gu_dat_slo** - negative: the more variation in green up date whitin an year, the slower the birds are

# gu_spe_slo** - positive: in more variable years, birds are faster

# ano_gu_spe_slo** - positive: same as before



## lag across latitude --------------------------------
# just lag was also sig, but this model was better (AIC< but different number of obs)
mod_lal1 <- lmer(data = final, AnomLag ~ cell_lat + I(cell_lat^2) +
                   (1|Family:species) + (1|year) + (1|cell))
sjPlot::tab_model(mod_lal1,show.re.var= TRUE, digits = 3)

ggplot(final) +
  geom_point(aes(x = cell_lat, y = AnomLag)) +
  stat_smooth(aes(x = cell_lat, y = AnomLag), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_bw()

ggplot(final) +
  geom_point(aes(x = cell_lat, y = lag)) +
  stat_smooth(aes(x = cell_lat, y = lag), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_bw()
