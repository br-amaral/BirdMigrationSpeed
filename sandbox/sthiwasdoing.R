library(mgcv)
library(gam)
library(itsadug)
library(tidyverse)
library(gtools)

final <- read_rds(file = "final.rds")

corrplot::corrplot(final %>% dplyr::select(winlat , xi_mean , ea_lat , Body_mass_g , Distance_m)  %>% 
                     distinct() %>% cor(use = "complete.obs"), method="number")

x <- c('xi_mean','ea_lat','Diet','Body_mass_g','Distance_m','Time') 
xcombs <- do.call(c, lapply(seq_along(x), combn, x = x, simplify = FALSE))

combpar <- as.data.frame(matrix(NA, ncol = 9, nrow = length(xcombs)))
colnames(combpar) <- c("mod_num", "y", "fixed", "gam","model","AIC","AICg","npar", "GVC")
combpar$mod_num <- seq(1, length(xcombs),1)
combpar$y <- "log(vArrMag) ~"
combpar$gam <- "+ s(cell_lat, bs ='tp') +  s(species, bs ='re') + s(year, bs='re') + s(cell, bs='re')"
for(i in 1:nrow(combpar)){
  combpar$fixed[i] <- paste(unlist(xcombs[i]), collapse = ' + ')
  combpar$npar[i] <- lengths(xcombs[i])
}
combpar <- combpar %>% 
  as_tibble() %>% 
  mutate(model = paste0(y, fixed, gam))

for(i in 1:nrow(combpar)){
  mod <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)),
                   formula = combpar$model[i] %>% as.formula(), method = "ML"
  )
  assign(glue("model_{i}"), mod)
  combpar$AIC[i] <- mod$aic
  combpar$AICg[i] <- logLik.gam(mod) %>% as.numeric()
  combpar$GVC[i] <- as.numeric(mod$gcv.ubre)
  rm(mod)
  print(i)
}

aic1 <- min(combpar$AIC)
aic1g <- min(combpar$AICg)
combpar <- combpar %>% 
  mutate(deltaAIC = AIC - aic1,
         deltaAICg = AICg - aic1g)
combpar  %>% view()

gvc1 <- min(combpar$GVC)
combpar <- combpar %>% 
  mutate(deltaGVC = GVC - gvc1)
combpar  %>% view()

combpar %>% 
  arrange(AIC) %>% 
  slice(1)

pkgs <- c("here", "readr", "janitor", "mgcv", "gratia", "dplyr", "ggplot2",
          "ggrepel")
vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

summary(model_15)
draw(model_15)
sms <- smooths(model_15)
draw(model_63, select = sms[1:4])
plot(model_15)

# no winla
kx <- 5
super_gam5 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell),
                                                Diet = as.factor(Diet), Time = as.factor(Time)), 
                        log(vArrMag) ~ 
                          s(winlat, k = kx, bs = "ts") + 
                          s(xi_mean, k = kx, bs = "ts") + 
                          s(ea_lat, k = kx, bs = "ts") + 
                          Diet + 
                          s(Body_mass_g, k = kx, bs = "ts") + 
                          s(Distance_m, k = kx, bs = "ts") + 
                          Time +
                          s(cell_lat, bs = "tp") +  # or tp?
                          s(species, bs = "re") + 
                          s(cell, bs = "re") #+
                        #s(year, bs = "re") 
                        ,select = T#, method = "REML" # or gcv
)
## ts is tp with smooth penalty
plot(super_gam5,pages = 1)
summary(super_gam5)

plot(super_gam2,pages = 1)
summary(super_gam2)

AIC(super_gam5, super_gam52)


# rm(list=setdiff(ls(), "final"))
# bird speed and species traits ------------------------



mod_ag1 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + ea_lat + Distance_m + Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"),
                     method = "ML")

mod_ag1 %>% summary()


mod_ag1 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + xi_mean + ea_lat + Diet + Body_mass_g + Distance_m + Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no winlat
mod_ag2 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ xi_mean + ea_lat + Diet + Body_mass_g + Distance_m +  Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no xi_mean
mod_ag3 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + ea_lat + Diet + Body_mass_g + Distance_m + Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no ea_lat
mod_ag4 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + xi_mean + Diet + Body_mass_g + Distance_m + Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no diet
mod_ag5 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + xi_mean + ea_lat + Body_mass_g + Distance_m + Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no mass
mod_ag6 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + xi_mean + ea_lat + Diet + Distance_m +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no distance
mod_ag7 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + xi_mean + ea_lat + Diet + Body_mass_g +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no winlat and body mass
mod_ag8 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ xi_mean + ea_lat + Diet + Distance_m +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no xi_mean and body mass
mod_ag9 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ winlat + ea_lat + Diet + Distance_m + Time +
                       s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# no body mass and no distances (cor with sensi)
mod_ag10 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ xi_mean + ea_lat + Diet + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no time
mod_ag11 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ winlat + xi_mean + ea_lat + Diet + Body_mass_g + Distance_m +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# no body mass and no distances (cor with sensi) no time
mod_ag12 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ xi_mean + ea_lat + Diet + Distance_m + 
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# no body mass and no distance (cor with sensi)
mod_ag13 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ xi_mean + ea_lat + Diet + Time + winlat +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# nine with no time
mod_ag14 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ winlat + ea_lat + Diet + Distance_m + 
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# no xi_mean, distance and body mass
mod_ag15 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ winlat + ea_lat + Diet + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# no winlat and xi_mean
mod_ag16 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ ea_lat + Diet + Body_mass_g + Distance_m + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# no winlat and xi_mean and distance
mod_ag17 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ ea_lat + Diet + Body_mass_g + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# nine with no diet
mod_ag18 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ winlat + ea_lat + Distance_m + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))
# eighteen without time
mod_ag19 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ winlat + ea_lat + Distance_m + 
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# model 18 with no distance, only winlat
mod_ag20 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ winlat + ea_lat + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))

# opposite of 20
mod_ag21 <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      log(vArrMag) ~ ea_lat + Distance_m + Time +
                        s(cell_lat, bs = "tp") +  s(species, bs = "re") + s(year, bs = "re") + s(cell, bs = "re"))



extract_fixed <- function(model,ci_level = .95,digits = 3,...) {
  fe <- data.frame(summary(model, ...)$p.table)
  colnames(fe) =  c('value', 'se', 't', 'p')
  
  # no confint.gam
  if (ci_level > 0) {
    
    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    nu <- model$df.residual
    mult <- stats::qt(upper, nu)
    
    ci <- data.frame(
      lower = fe$value - mult * fe$se,
      upper = fe$value + mult * fe$se
    )
    
    colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
    
    fe <- data.frame(fe, ci)
  }
  
  fe <- fe %>%
    dplyr::mutate_all(round, digits = digits) %>%
    dplyr::mutate(term = gsub(rownames(fe),
                              pattern = '[\\(,\\)]',
                              replacement = '')) %>%
    dplyr::select(term, dplyr::everything()) %>%
    dplyr::as_tibble()
  
  fe
  
}

BICtab <- BIC(mod_ag1, mod_ag2, mod_ag3, mod_ag4, 
              mod_ag5, mod_ag6, mod_ag7, mod_ag8, mod_ag9,
              mod_ag10, mod_ag11, mod_ag12, mod_ag13, mod_ag14,
              mod_ag15, mod_ag16, mod_ag17, mod_ag18, mod_ag19
) %>% 
  arrange(BIC) 
bic1 <- BICtab[1,2] 
BICtab <- BICtab %>% 
  mutate(deltaBIC = BIC - bic1,
         par_numb = NA)
for(i in 1:nrow(BICtab)){
  BICtab$par_numb[i] <- dim(extract_fixed(get(rownames(BICtab)[i])))[1]
}
BICtab  

summary(mod_ag18)

max(final$vArrMag, na.rm = T)# %>% log()
min(final$vArrMag, na.rm = T)# %>% log()
median(final$vArrMag, na.rm = T)# %>% log()

ggplot(data = final) +
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


ggplot(data = final) +
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

ggplot(data = final) +
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

ggplot(data = final) +
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

plot.gam(mod_ag18)[1]

# bird speed latitude plot
spse <- table(final$species) %>% sort(decreasing = T) 
spse <- names(spse[1:10])

final %>% 
  filter(species %in% spse) %>% 
  ggplot() +
  geom_point(aes(x = jitter(cell_lat, 10), y = log(vArrMag)), alpha = 0.2) +
  theme_bw() +
  scale_y_continuous(trans='log10') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Cell latitude") +
  ylab("Bird speed of arrival (km/day)\n\n") +
  geom_smooth(aes(x = cell_lat, y = log(vArrMag)), col = 'black') +
  xlim(-10,7)



# OK bird speed and green up ------------------------
mod_b1g <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ AnomDGr + AnomVGr +
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

mod_b2g <- mgcv::gam(data = final %>%  mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ 
                       AnomDGr * xi_mean +
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

mod_b3g <- mgcv::gam(data = final %>%  mutate(species = as.factor(species),cell = as.factor(cell)), 
                     log(vArrMag) ~ 
                       AnomDGr * mig_cell +
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re"))

mod_b4g <- mgcv::gam(data = final %>%  mutate(species = as.factor(species),cell = as.factor(cell)), 
                     log(vArrMag) ~ 
                       AnomDGr * sensi +
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re"))

AICtab2 <- as.data.frame(cbind(c('mod_b1g', 'mod_b2g', 'mod_b3g', 'mod_b4g'),
                               c(mod_b1g$aic, mod_b2g$aic, mod_b3g$aic, mod_b4g$aic)))
colnames(AICtab2) <- c("mod", "AIC")
AICtab2 <- arrange(AICtab2, AIC) 
aic12 <- as.numeric(AICtab2[1,2])
AICtab2 <- AICtab2 %>% 
  mutate(deltaAIC = as.numeric(AIC) - aic12,
         par_numb = NA)
for(i in 1:nrow(AICtab2)){
  AICtab2$par_numb[i] <- dim(extract_fixed(get(AICtab2$mod[i])))[1]
}
AICtab2 

summary(mod_b1g)
plot(mod_b1g)

# anomalies in bird speed and green up ------------------------
mod_b1ga <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                      AnomVArr ~ AnomDGr + AnomVGr) #+
#s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

mod_b2ga <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomVArr ~ 
                        AnomDGr * xi_mean ) #+
#s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

mod_b3ga <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomVArr ~ 
                        AnomDGr * mig_cell) #+
#s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re"))

mod_b4ga <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomVArr ~ 
                        AnomDGr * sensi) #+
#s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re"))

BICtab3 <- BIC(mod_b1ga, mod_b2ga, mod_b3ga, mod_b4ga) %>% 
  arrange(BIC) 
bic13 <- BICtab3[1,2] 
BICtab3 <- BICtab3 %>% 
  mutate(deltaBIC = BIC - bic13,
         par_numb = NA)
for(i in 1:nrow(BICtab3)){
  BICtab3$par_numb[i] <- dim(extract_fixed(get(rownames(BICtab3)[i])))[1]
}
BICtab3

summary(mod_b1ga)
plot(mod_b1ga)

# anomalies in lag and green up ------------------------
modla <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     AnomVArr + 
                     ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"), select = T)

modla1 <- mgcv::gam(data = final %>% 
                      mutate(species = as.factor(species),
                             cell = as.factor(cell)), 
                    AnomLag ~ 
                      AnomVArr + 
                      ea_lat_ano + 
                      AnomVGr +
                      s(species, bs = "re"))# + s(cell_lat, bs = "tp"), select = T)

modlb <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     AnomVArr + 
                     #ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"))
modlc <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     #AnomVArr + 
                     ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"))
modld <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     AnomVArr + 
                     ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"))

modle <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     #AnomVArr + 
                     ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"))

modladis <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomLag ~ 
                        AnomVArr + 
                        ea_lat_ano + Distance_m +
                        #AnomVGr +
                        s(species, bs = "re"), select = T)

modla1dis <- mgcv::gam(data = final %>% 
                         mutate(species = as.factor(species),
                                cell = as.factor(cell)), 
                       AnomLag ~ 
                         AnomVArr + 
                         ea_lat_ano + Distance_m +
                         #AnomVGr +
                         AnomVGr +
                         s(species, bs = "re"))# + s(cell_lat, bs = "tp"), select = T)

modlbdis <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomLag ~ 
                        AnomVArr + Distance_m +
                        #AnomVGr +
                        #ea_lat_ano + 
                        #AnomVGr +
                        s(species, bs = "re"))
modlcdis <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomLag ~ 
                        #AnomVArr + 
                        ea_lat_ano + Distance_m +
                        #AnomVGr +
                        #AnomVGr +
                        s(species, bs = "re"))
modlddis <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomLag ~ 
                        AnomVArr + 
                        ea_lat_ano + Distance_m +
                        #AnomVGr +
                        #AnomVGr +
                        s(species, bs = "re"))

modledis <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      AnomLag ~ 
                        #AnomVArr + 
                        ea_lat_ano + Distance_m +
                        #AnomVGr +
                        s(species, bs = "re"))

BICtab4 <- BIC(modla, modla1, modlb, modlc, modld, modle,
               modladis, modla1dis, modlbdis, modlcdis, modlddis, modledis) %>% 
  arrange(BIC) 
bic14 <- BICtab4[1,2] 
BICtab4 <- BICtab4 %>% 
  mutate(deltaBIC = BIC - bic14,
         par_numb = NA)
for(i in 1:nrow(BICtab4)){
  BICtab4$par_numb[i] <- dim(extract_fixed(get(rownames(BICtab4)[i])))[1]
}
BICtab4

summary(modla1)
summary(modla1dis)

# annual means ----------------------------------
my_breaks = c(2, 3, 4, 5, 6)

medians <- final %>% dplyr::select(species, species2, vArrMag) %>% 
  group_by(species) %>% 
  mutate(meds = median(log(vArrMag), na.rm = T)) %>% 
  distinct() %>% 
  dplyr::select(-vArrMag)

pred.dat <- pred.dat %>% 
  left_join(., medians, by = c("species","species2")) %>% distinct()

## ADD THE MEAN FOR ALL SPECIES TOGETHER!!!!!



mod_an1 <- lmer(data = annual, BS ~ as.factor(year) -1 + (1|species))
sjPlot::tab_model(mod_an1, show.re.var= TRUE, digits = 3, p.style = "numeric")

mod_an2 <- lmer(data = annual, ABS ~ as.factor(year) -1 + (1|species))
sjPlot::tab_model(mod_an2, show.re.var= TRUE, digits = 3)

mod_an1a <- lm(data = annual %>% dplyr::select(GS,year) %>% distinct(), GS ~ as.factor(year) -1)
sjPlot::tab_model(mod_an1a, show.re.var= TRUE, digits = 3, p.style = "numeric")

mod_an2a <- lm(data = annual %>% dplyr::select(AGS,year) %>% distinct(), AGS ~ as.factor(year) -1 )
sjPlot::tab_model(mod_an2a, show.re.var= TRUE, digits = 3)

final %>% 
  dplyr::select(year, AnomDGr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_hline(yintercept=0, color = "darkgray") +
  geom_boxplot(aes(x = year, y = AnomDGr)) +
  geom_point(data = annual, aes(x = as.factor(year), y = AGD), col = "#7CAE00") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) +
  ggtitle("Annual green up date anomaly") +
  labs(y = "Date anomaly")

final %>% 
  dplyr::select(year, vGrMag, AnomVGr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_hline(yintercept=0, color = "darkgray") +
  geom_boxplot(aes(x = year, y = AnomVGr)) +
  geom_point(data = annual, aes(x = as.factor(year), y = AGS), col = "#7CAE00") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) +
  ggtitle("Annual green up speed anomaly") +
  labs(y = "Speed anomaly")

#estimates from model
mean_esti_spe <- as.data.frame(cbind(
  seq(2003,2017,1), 
  c(3.76904,3.98685,3.9475,3.99808,3.9928,3.90656,3.95527,4.05626,
    3.87221,4.03716,3.97347,3.95947,4.07852,3.91919,3.9055)))
colnames(mean_esti_spe) <- c("year", "bspe")
final %>% 
  dplyr::select(year, vArrMag, AnomVArr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_boxplot(aes(x = year, y = log(vArrMag))) +
  #geom_point(data = annual %>% group_by(year) %>% summarise(b_spe_mea = mean(BS, na.rm = T))
  #           , aes(x = as.factor(year), y = b_spe_mea), col = "#00BFC4") +
  geom_point(data = mean_esti_spe,
             aes(x = as.factor(year), y = bspe), col = "#00BFC4")+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) + 
  ggtitle("Annual bird speed ") +
  labs(y = "log (speed)") 



ggplot(pred.dat, aes(as.factor(year), 
                     reorder(species2,meds), 
                     fill= pred)) + 
  geom_tile() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
  ) +
  scale_fill_viridis(name = "Species mean\nannual speed\n(log)",
                     option = "H", trans = "log",
                     breaks = my_breaks, labels = my_breaks)


pred.datA <- pred.datA %>% 
  left_join(., medians, by = c("species","species2")) %>% distinct()

ggplot(pred.datA, aes(as.factor(year), 
                      reorder(species2,meds), 
                      fill= pred)) + 
  geom_tile() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1),
        legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
  ) +
  scale_fill_viridis(name = "Species mean\nannual speed\n(log)",
                     option = "H"
                     #breaks = my_breaks, labels = my_breaks
  )
bspe_yr_lm <- lmer(data = final %>% dplyr::select(vArrMag, cell, cell_lat, species, year) %>% 
                     filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                   log(vArrMag) ~ as.factor(species) * as.factor(year) + (1|cell))

# OK ANNUAL estimates (all sps together as re) -------------------
# bird speed
pred.dat <- expand.grid(species=sort(unique(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% 
                                              dplyr::select(species) %>% pull())),
                        year=seq(2003,2017,1))
pred.dat <- pred.dat %>% 
  mutate(pred = predict(bspe_yr_lm, newdata=pred.dat, re.form =~0)) %>% 
  left_join(., final %>% dplyr::select(species, species2) %>% distinct(), by = "species")

# bird speed anomaly
bspe_yr_lmA <- lmer(data = final %>% dplyr::select(AnomVArr, cell, cell_lat, species, year) %>% 
                      filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>%	distinct(),
                    AnomVArr ~ as.factor(species) * as.factor(year) + (1|cell))

pred.datA1 <- expand.grid(species=sort(unique(final %>% filter (!species == "Setophaga_pinus") %>% filter (!species == "Pipilo_erythrophthalmus") %>% 
                                                dplyr::select(species) %>% pull())),
                          year=seq(2003,2017,1))
pred.datA1 <- pred.datA1 %>%  mutate(pred = predict(bspe_yr_lmA, newdata=pred.datA1, re.form =~0)) %>% 
  left_join(., final %>% dplyr::select(species, species2) %>% distinct(), by = "species")

# green up speed anomaly
guspeA_lm <- lmer(data = final %>% dplyr::select(AnomVGr, cell, cell_lat, year) %>% distinct(),
                  AnomVGr ~ as.factor(year) - 1 + (1|cell))
yearsAlm <- cbind(as.numeric(substring(names(getME(guspeA_lm, name = "fixef")), 16)),
                  as.numeric(getME(guspeA_lm, name = "fixef")))
colnames(yearsAlm) <- c("year", "gu_speA_mea")
yearsAlm <- as.data.frame(yearsAlm)
rownames(yearsAlm) <- NULL

# green up date anomaly
gudatA_lm <- lmer(data = final %>% dplyr::select(AnomDGr, cell, cell_lat, year) %>% distinct(),
                  AnomDGr ~ as.factor(year) - 1 + (1|cell))
yeardAlm <- cbind(as.numeric(substring(names(getME(gudatA_lm, name = "fixef")), 16)),
                  as.numeric(getME(gudatA_lm, name = "fixef")))
colnames(yeardAlm) <- c("year", "gu_datA_mea")
yeardAlm <- as.data.frame(yeardAlm)
rownames(yeardAlm) <- NULL

annual <- left_join(pred.dat %>% rename(BS = pred), 
                    pred.datA %>% dplyr::select(species, pred, year) %>% rename(ABS = pred),
                    by = c("species","year")) %>% 
  #left_join(., yearslm, by = "year") %>% 
  #left_join(., yeardlm, by = "year") %>% 
  left_join(., yearsAlm, by = "year") %>% 
  left_join(., yeardAlm, by = "year") %>% 
  rename(#med_spe = meds,
    #         GS = gu_spe_mean,
    #         GD = gu_dat_mea,
    AGS = gu_speA_mea,
    AGD = gu_datA_mea)

mod_yr4 <- lmer(data = annual, ABS ~ AGS + AGD + (1|species))
mod_yr5 <- lm(data = annual, ABS ~ AGS + AGD)


summary(mod_yr5)
AIC(mod_yr5, mod_yr4)
sjPlot::tab_model(mod_yr5,show.re.var= TRUE, digits = 3, p.style = "scientific")
plot(mod_yr5)


mod_an12a <- mgcv::gam(data = final%>% 
                         mutate(species = as.factor(species),
                                cell = as.factor(cell)),
                       AnomVArr ~ as.factor(year) - 1 + 
                         s(species, bs = 're')) 

mod_an12c <- mgcv::gam(data = final%>% 
                         mutate(species = as.factor(species),
                                cell = as.factor(cell)),
                       AnomVArr ~ as.factor(year) - 1 ) 

sjPlot::tab_model(mod_an12a, show.re.var= TRUE, digits = 3, p.style = "numeric")


mod_an12b <- lm(data = annual %>% 
                  mutate(species = as.factor(species)),
                ABS ~ as.factor(year) - 1) 

sjPlot::tab_model(mod_an12b, show.re.var= TRUE, digits = 3, p.style = "numeric")


AIC(mod_an12a, mod_an12b, mod_an12c) %>% arrange(AIC)








ggplot(annual) +
  geom_point(aes(y = ABS, x = AGS, col =species)) +
  geom_smooth(aes(y = ABS, x = AGS)) +
  theme(legend.position = "NULL")



summary(mod_yr1)


ggplot(data = annual) +
  geom_point(aes(x = GS, y = BS)) +
  geom_point(aes(x = GD, y = BS), col = "blue")  +
  geom_smooth(aes(x = GS, y = BS), method = "lm") +
  geom_smooth(aes(x = GD, y = BS), col = "black", method = "lm")

final %>% 
  dplyr::select(year, AnomDGr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_hline(yintercept=0, color = "darkgray") +
  geom_boxplot(aes(x = year, y = AnomDGr)) +
  geom_point(data = annual, aes(x = as.factor(year), y = AGD), col = "#7CAE00") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) +
  ggtitle("Annual green up date anomaly") +
  labs(y = "Date anomaly")

final %>% 
  dplyr::select(year, vGrMag, AnomVGr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_hline(yintercept=0, color = "darkgray") +
  geom_boxplot(aes(x = year, y = AnomVGr)) +
  geom_point(data = annual, aes(x = as.factor(year), y = AGS), col = "#7CAE00") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) +
  ggtitle("Annual green up speed anomaly") +
  labs(y = "Speed anomaly")

mean_birds <- as.data.frame(byeardAlm)
final %>% 
  dplyr::select(year, vArrMag, AnomVArr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_hline(yintercept=0, color = "darkgray") +
  geom_boxplot(aes(x = year, y = AnomVArr)) +
  geom_point(data = mean_birds, aes(x = as.factor(year), y = b_speA_mea), col = "#00BFC4") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) + 
  ggtitle("Annual bird speed anomaly") +
  labs(y = "Speed anomaly") 

final %>% 
  dplyr::select(year, ea_lat_ano, ea_lat_yr) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  geom_hline(yintercept=0, color = "darkgray") +
  geom_boxplot(aes(x = year, y = ea_lat_ano)) +
  #geom_point(aes(x = as.factor(year), y = ea_lat_yr), col = "#00BFC4") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank()) + 
  ggtitle("Annual bird first arrival date anomaly") +
  labs(y = "Arrival date") 


# green-up speed ------------------
mod_g1 <- mgcv::gam(data = final %>% mutate(cell = as.factor(cell)), 
                    log(vGrMag) ~ AnomDGr + 
                      s(cell_lat, bs = "tp") + s(year, bs = "re") + s(cell, bs = "re"))

summary(mod_g1)

# migration direction
direc <- final %>% 
  dplyr::select(vArrAng) %>% 
  mutate(sum = vArrAng + 90,
         ang = ifelse(sum > 360, sum - 360, sum))
breaks_dir <- c(0, #45, 
                90, #135, 
                180, #225, 
                270, #315, 
                360)

breaks_dir2 <- c(0, 45, 
                 90, 135, 
                 180, 225, 
                 270, 315)
ggplot(direc) +
  geom_histogram(aes(x = ang, y =(..count..)/sum(..count..)), bins = 30) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(y = "Frequency", x = "Migration direction (degrees)") +
  scale_x_continuous(breaks = breaks_dir, labels = breaks_dir) 


ggplot(direc) +
  geom_histogram(aes(x = ang, y =(..count..)/sum(..count..)), bins = 30) +
  theme_bw() +
  scale_x_continuous(breaks = breaks_dir2, labels = breaks_dir2) + 
  # Make it circular!
  coord_polar() 



ggplot(direc, aes(x = ang, y = variable)) + 
  geom_line(size = 5) +
  scale_y_discrete(expand = c(0, 1), breaks = NULL) +
  xlim(month.abb) + 
  coord_polar() +
  theme_bw() + xlab(NULL) + ylab(NULL)


















d1 <- c("Granivore","Insectivore", "Omnivore")
d2 <- c(coefs[which(rownames(coefs) == "(Intercept)"),1],
        coefs[which(rownames(coefs) == "(Intercept)"),1] + coefs[which(rownames(coefs) == "DietInsectivore"),1] ,
        coefs[which(rownames(coefs) == "(Intercept)"),1] + coefs[which(rownames(coefs) == "DietOmnivore"),1])

d3 <- c(coefs[which(rownames(coefs) == "(Intercept)"),1] - coefs[which(rownames(coefs) == "(Intercept)"),2] ,
        coefs[which(rownames(coefs) == "(Intercept)"),1] - coefs[which(rownames(coefs) == "(Intercept)"),2] +
          coefs[which(rownames(coefs) == "DietInsectivore"),1] - coefs[which(rownames(coefs) == "DietInsectivore"),2] ,
        coefs[which(rownames(coefs) == "(Intercept)"),1] - coefs[which(rownames(coefs) == "(Intercept)"),2] + 
          coefs[which(rownames(coefs) == "DietOmnivore"),1] - coefs[which(rownames(coefs) == "DietOmnivore"),2])
d4 <- c(coefs[which(rownames(coefs) == "(Intercept)"),1] + coefs[which(rownames(coefs) == "(Intercept)"),2] ,
        coefs[which(rownames(coefs) == "(Intercept)"),1] + coefs[which(rownames(coefs) == "(Intercept)"),2] +
          coefs[which(rownames(coefs) == "DietInsectivore"),1] + coefs[which(rownames(coefs) == "DietInsectivore"),2] ,
        coefs[which(rownames(coefs) == "(Intercept)"),1] + coefs[which(rownames(coefs) == "(Intercept)"),2] + 
          coefs[which(rownames(coefs) == "DietOmnivore"),1] + coefs[which(rownames(coefs) == "DietOmnivore"),2])

diet <- cbind(d1,d2,d3,d4) %>% 
  as.data.frame() %>% 
  mutate(d2 = as.numeric(d2),
         d3 = as.numeric(d3),
         d4 = as.numeric(d4))


coefs <- summary_model$p.table
ggarrange(#labels = c("A", "B", "C", "D"),
  ncol = 2, #nrow = 2,
  
  #ggplot(diet) +
  #  geom_blank() +
  #  theme_bw() +
  #  theme(plot.title = element_text(hjust = 0.5)) +
  #  geom_segment(aes(x = d1, y = d3, xend = d1, yend = d4), colour = "darkgray") +
  #  geom_point(aes(x = d1, y = d2), col = 'red', size = 2) +
  #  geom_point(aes(x = d1, y = d3), col = 'darkgray', shape = "-", size = 7) +
  #  geom_point(aes(x = d1, y = d4), col = 'darkgray', shape = "-", size = 7) + 
  #  labs(title="Diet group",
  #       y = "Log(Speed)",
  #       x = "Diet group") , 
  
  #ggplot() +
  #  geom_blank() +
  #  scale_x_continuous(limits=c(0,0.5)) +
  #  scale_y_continuous(limits=c(0,6)) +
  #  theme_bw() +
  #  theme(plot.title = element_text(hjust = 0.5)) +
  #  geom_abline(aes(intercept = coefs[which(rownames(coefs) == "(Intercept)"),1], 
  #                  slope =coefs[which(rownames(coefs) == "winlat"),1]), col = 'red') +
  #  geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] - 
  #                                 coefs[which(rownames(coefs) == "(Intercept)"),2]), 
  #                  slope = (coefs[which(rownames(coefs) == "winlat"),1] - 
  #                             coefs[which(rownames(coefs) == "winlat"),2])), col = 'gray') +
  #  geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] + 
  #                                 coefs[which(rownames(coefs) == "(Intercept)"),2]), 
  #                  slope = (coefs[which(rownames(coefs) == "winlat"),1] + 
  #                             coefs[which(rownames(coefs) == "winlat"),2])), col = 'gray') +
  #  labs(title="Species wintering latitude",
  #       y = "Log(Speed)", x = "Wintering latitude") ,
  
  
  ggplot() +
    geom_blank() +
    scale_x_continuous(limits=c(0,0.5)) +
    scale_y_continuous(limits=c(-1,8)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(aes(intercept = coefs[which(rownames(coefs) == "(Intercept)"),1], 
                    slope =coefs[which(rownames(coefs) == "xi_mean"),1]), col = 'red') +
    geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] - 
                                   coefs[which(rownames(coefs) == "(Intercept)"),2]), 
                    slope = (coefs[which(rownames(coefs) == "xi_mean"),1] - 
                               coefs[which(rownames(coefs) == "xi_mean"),2])), col = 'gray') +
    geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] + 
                                   coefs[which(rownames(coefs) == "(Intercept)"),2]), 
                    slope = (coefs[which(rownames(coefs) == "xi_mean"),1] + 
                               coefs[which(rownames(coefs) == "xi_mean"),2])), col = 'gray') +
    labs(#title="Species sensitivity",
      y = "Log(Speed)", x = "Sensitivity") ,
  
  ggplot() +
    geom_blank() +
    scale_x_continuous(limits=c(80,120)) +
    scale_y_continuous(limits=c(-1,8)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1]), 
                    slope = (coefs[which(rownames(coefs) == "ea_lat"),1])), col = 'red') +
    geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] - 
                                   coefs[which(rownames(coefs) == "(Intercept)"),2]), 
                    slope = (coefs[which(rownames(coefs) == "ea_lat"),1] - 
                               coefs[which(rownames(coefs) == "ea_lat"),2])), col = 'gray') +
    geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] + 
                                   coefs[which(rownames(coefs) == "(Intercept)"),2]), 
                    slope = (coefs[which(rownames(coefs) == "ea_lat"),1] + 
                               coefs[which(rownames(coefs) == "ea_lat"),2])), col = 'gray') +
    labs(#title="Early arrival date",
      y = "Log(Speed)", x = "Arrival date") #,
  
  #ggplot() +
  #  geom_blank() +
  #  scale_x_continuous(limits=c(0,8000)) +
  #  scale_y_continuous(limits=c(0,6)) +
  #  theme_bw() +
  #  theme(plot.title = element_text(hjust = 0.5)) +
  #  geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1]),
  #                               slope = (coefs[which(rownames(coefs) == "Distance_m"),1])), col = 'red')+
  #  geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] - 
  #                                 coefs[which(rownames(coefs) == "(Intercept)"),2]), 
  #                  slope = (coefs[which(rownames(coefs) == "Distance_m"),1] - 
  #                             coefs[which(rownames(coefs) == "Distance_m"),2])), col = 'gray') +
  #  geom_abline(aes(intercept = (coefs[which(rownames(coefs) == "(Intercept)"),1] + 
  #                                 coefs[which(rownames(coefs) == "(Intercept)"),2]), 
  #                  slope = (coefs[which(rownames(coefs) == "Distance_m"),1] + 
  #                             coefs[which(rownames(coefs) == "Distance_m"),2])), col = 'gray') +
  #  labs(title="Migration distance",
  #       y = "Log(Speed)", x = "Migration distance") 
)


ggplot(data = final) +
  geom_boxplot(aes(x = reorder(species2,
                               #xi_mean, 
                               log(vArrMag),
                               FUN = median, na.rm = TRUE), 
                   y = log(vArrMag), fill = xi_mean),
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
  scale_fill_viridis(name = "Species\nsensitivity\n") +
  labs(y = "Log(Speed)\n") 

ggplot(final, aes(y = log(vArrMag))) +
  geom_histogram(aes(x =(..count..)/sum(..count..)), bins = 15) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank()) 

### Green-up - are birds tracking green up? 


mod_b1g <- mgcv::gam(data = final %>% mutate(species = as.factor(species), cell = as.factor(cell)), 
                     log(vArrMag) ~ AnomDGr + AnomVGr +
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

mod_b1g2 <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)), 
                      log(vArrMag) ~ 
                        AnomDGr  + xi_mean +
                        s(species, bs = "re") + s(year, bs = "re") + 
                        s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

BIC(mod_b1g, mod_b1g2)

mod_b1gm <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell))%>% 
                        filter(mig_cell == T), 
                      log(vArrMag) ~ 
                        AnomDGr + AnomVGr +
                        s(species, bs = "re") + s(year, bs = "re") + 
                        s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
mod_b1gb <- mgcv::gam(data = final %>% 
                        mutate(species = as.factor(species),
                               cell = as.factor(cell)) %>% 
                        filter(breed_cell == T), 
                      log(vArrMag) ~ 
                        AnomDGr + AnomVGr +
                        s(species, bs = "re") + s(year, bs = "re") + 
                        s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
summary(mod_b1gm)
summary(mod_b1gb)

mod_b3g <- mgcv::gam(data = final %>% 
                       mutate(species = as.factor(species),
                              cell = as.factor(cell)), 
                     log(vArrMag) ~ 
                       AnomDGr*mig_cell + AnomVGr *mig_cell+
                       s(species, bs = "re") + s(year, bs = "re") + 
                       s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
summary(mod_b1g)
mod_b2g <- mgcv::gam(data = final %>% 
                       mutate(species = as.factor(species),
                              cell = as.factor(cell)), 
                     log(vArrMag) ~ #gr_mn + log(vGrMag) + 
                       #AnomDGr + AnomVGr +
                       gu_spe_mean + gu_dat_mea + #gu_speA_mea + gu_datA_mea +
                       s(species, bs = "re") + s(year, bs = "re") + s(cell_lat, bs = "tp") + s(cell, bs = "re")) 
summary(mod_b2g)
BIC(mod_b1g, mod_b2g, mod_b3g)  %>% arrange(BIC) %>% 
  mutate(DeltaBIC = c(diff(BIC), NA))

## undone
mod_b1gm <- mgcv::gam(data = final,
                      log(b_spe_mean_yr) ~ 
                        gu_spe_mean + gu_dat_mea +
                        s(year, bs = "re")) 
summary(mod_b1gm)

## migration andb breeding cell speed
mod_ran1g <- mgcv::gam(data = final %>% 
                         mutate(cell_type = as.factor(mig_cell),
                                species = as.factor(species),
                                cell = as.factor(cell)),
                       log(vArrMag) ~ mig_cell +
                         s(species, bs = 're') + 
                         s(year, bs = 're') + 
                         s(cell_lat, bs = 'tp') + 
                         s(cell, bs = 're')
) 

mod_ran2g <- mgcv::gam(data = final %>% 
                         mutate(cell_type = as.factor(mig_cell),
                                species = as.factor(species),
                                cell = as.factor(cell)),
                       log(vArrMag) ~ mig_cell +
                         #s(species, bs = 're') + 
                         s(year, bs = 're') + 
                         s(cell_lat, bs = 'tp') + 
                         s(cell, bs = 're')
) 

mod_ran3g <- mgcv::gam(data = final %>% 
                         mutate(cell_type = as.factor(mig_cell),
                                species = as.factor(species),
                                cell = as.factor(cell)),
                       log(vArrMag) ~ mig_cell +
                         s(species, bs = 're') + 
                         #s(year, bs = 're') + 
                         s(cell_lat, bs = 'tp') + 
                         s(cell, bs = 're')
) 

mod_ran4g <- mgcv::gam(data = final %>% 
                         mutate(cell_type = as.factor(mig_cell),
                                species = as.factor(species),
                                cell = as.factor(cell)),
                       log(vArrMag) ~ mig_cell +
                         #s(species, bs = 're') + 
                         s(year, bs = 're') + 
                         s(cell_lat, bs = 'tp') #+ 
                       #s(cell, bs = 're')
) 

mod_ran5g <- mgcv::gam(data = final %>% 
                         mutate(cell_type = as.factor(mig_cell),
                                species = as.factor(species),
                                cell = as.factor(cell)),
                       log(vArrMag) ~ mig_cell +
                         s(species, bs = 're') + 
                         s(year, bs = 're') + 
                         s(cell_lat, bs = 'tp') #+ 
                       #s(cell, bs = 're')
) 

BIC(mod_ran1g,mod_ran2g,mod_ran3g,mod_ran4g,mod_ran5g) %>% arrange(BIC)
summary(mod_ran4g)
summary(mod_ran1g)

mod_ranA1g <- mgcv::gam(data = final %>% 
                          mutate(cell_type = as.factor(mig_cell),
                                 species = as.factor(species),
                                 cell = as.factor(cell)),
                        AnomVArr ~ #mig_cell +
                          s(species, bs = 're') + 
                          s(year, bs = 're') + 
                          s(cell_lat, bs = 'tp') + 
                          s(cell, bs = 're')
) 

summary(mod_ranA1g)

gu_gam1 <- mgcv::gam(data = final %>% dplyr::select(year, cell, cell_lat,
                                                    vGrMag, AnomVGr,
                                                    gu_spe_mean, gu_dat_mea),
                     log(vGrMag) ~ s(cell_lat, bs = "tp") +
                       s(year, bs = "re") + s(cell, bs = "re"))
summary(gu_gam1)

gu_gam2 <- mgcv::gam(data = final %>% dplyr::select(year, cell, cell_lat,
                                                    vGrMag, AnomVGr,
                                                    gu_spe_mean, gu_dat_mea),
                     AnomVGr ~ s(cell_lat, bs = "tp") +
                       s(year, bs = "re") + s(cell, bs = "re"))
summary(gu_gam1)





moda <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    ea_lat_ano + 
                    AnomVGr + AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

modb <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    #ea_lat_ano + 
                    AnomVGr + AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

modc <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    ea_lat_ano + 
                    #AnomVGr + AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

modd <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    ea_lat_ano + 
                    AnomVGr + #AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

mode <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    ea_lat_ano + 
                    #AnomVGr +
                    AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

modf <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    #ea_lat_ano + 
                    #AnomVGr +
                    AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

modg <- mgcv::gam(data = final %>% 
                    mutate(species = as.factor(species),
                           cell = as.factor(cell)), 
                  log(vArrMag) ~ 
                    #ea_lat_ano + 
                    AnomVGr +
                    #AnomDGr +
                    s(species, bs = "re") + s(year, bs = "re") + 
                    s(cell_lat, bs = "tp") + s(cell, bs = "re")) 

BIC(moda, modb, modc, modd, mode, modf, modg) %>% 
  arrange(BIC) %>% 
  mutate(DeltaBIC = c(diff(BIC), NA))

summary(modd)
plot.gam(modd)


modla <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     AnomVArr + 
                     ea_lat_ano + 
                     AnomVGr +
                     s(species, bs = "re"), select = T)

modlb <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     AnomVArr + 
                     #ea_lat_ano + 
                     AnomVGr +
                     s(species, bs = "re"))
modlc <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     #AnomVArr + 
                     ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"))
modld <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     AnomVArr + 
                     ea_lat_ano + 
                     #AnomVGr +
                     s(species, bs = "re"))

modle <- mgcv::gam(data = final %>% 
                     mutate(species = as.factor(species),
                            cell = as.factor(cell)), 
                   AnomLag ~ 
                     #AnomVArr + 
                     ea_lat_ano + 
                     AnomVGr +
                     s(species, bs = "re"))



BIC(modla, modlb, modlc, modld, modle) %>% 
  arrange(BIC) %>% 
  mutate(DeltaBIC = c(diff(BIC), NA))

summary(modla)




####
mig_pace <- read.csv("data/casey_pace.csv") 
hist(log(final$vArrMag))

b <- final %>% dplyr::select(species, b_spe_mean) %>% distinct() %>% left_join(., mig_pace, by = "species")
b
plot(b$b_spe_mean, b$MigPace)