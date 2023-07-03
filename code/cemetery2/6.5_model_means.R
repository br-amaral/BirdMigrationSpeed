library(mgcv)
library(gam)
library(itsadug)
library(tidyverse)

final2 <- final %>% dplyr::select(b_spe_mean, winlat, xi_mean, ea_lat, 
                                  Diet, Body_mass_g, Distance_m, year) %>% 
  distinct()
  
mod_agm1 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       xi_mean + 
                       ea_lat + 
                       Diet + 
                       Body_mass_g + 
                       Distance_m +
                       s(year, bs = "re"))

mod_agm2 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       xi_mean + 
                       ea_lat + 
                       Diet + 
                       Body_mass_g + 
                       Distance_m +
                       s(year, bs = "re"))

mod_agm3 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       ea_lat + 
                       Diet + 
                       Body_mass_g + 
                       Distance_m +
                       s(year, bs = "re"))

mod_agm4 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       xi_mean + 
                       Diet + 
                       Body_mass_g + 
                       Distance_m +
                       s(year, bs = "re"))

mod_agm5 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       xi_mean + 
                       ea_lat + 
                       Body_mass_g + 
                       Distance_m +
                       s(year, bs = "re"))

mod_agm6 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       xi_mean + 
                       ea_lat + 
                       Diet + 
                       Distance_m +
                       s(year, bs = "re"))

mod_agm7 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       xi_mean + 
                       ea_lat + 
                       Diet + 
                       Body_mass_g + 
                       s(year, bs = "re"))

mod_agm8 <- mgcv::gam(data = final2, 
                     log(b_spe_mean) ~ 
                       winlat + 
                       xi_mean + 
                       ea_lat + 
                       Body_mass_g + 
                       s(year, bs = "re"))

BIC(mod_agm1, mod_agm2, mod_agm3, mod_agm4, 
    mod_agm5, mod_agm6, mod_agm7) %>% 
  arrange(BIC) %>% 
  mutate(DeltaBIC = c(diff(BIC), NA))


(summary_model2 <- summary(mod_agm6))
(coefs2 <- summary_model2$p.table)

plot(mod_agm6)

d1 <- c("Granivore","Insectivore", "Omnivore")
d2 <- c(coefs2[which(rownames(coefs2) == "(Intercept)"),1],
        coefs2[which(rownames(coefs2) == "(Intercept)"),1] + coefs2[which(rownames(coefs2) == "DietInsectivore"),1] ,
        coefs2[which(rownames(coefs2) == "(Intercept)"),1] + coefs2[which(rownames(coefs2) == "DietOmnivore"),1])

d3 <- c(coefs2[which(rownames(coefs2) == "(Intercept)"),1] - coefs2[which(rownames(coefs2) == "(Intercept)"),2] ,
        coefs2[which(rownames(coefs2) == "(Intercept)"),1] - coefs2[which(rownames(coefs2) == "(Intercept)"),2] +
          coefs2[which(rownames(coefs2) == "DietInsectivore"),1] - coefs2[which(rownames(coefs2) == "DietInsectivore"),2] ,
        coefs2[which(rownames(coefs2) == "(Intercept)"),1] - coefs2[which(rownames(coefs2) == "(Intercept)"),2] + 
          coefs2[which(rownames(coefs2) == "DietOmnivore"),1] - coefs2[which(rownames(coefs2) == "DietOmnivore"),2])
d4 <- c(coefs2[which(rownames(coefs2) == "(Intercept)"),1] + coefs2[which(rownames(coefs2) == "(Intercept)"),2] ,
        coefs2[which(rownames(coefs2) == "(Intercept)"),1] + coefs2[which(rownames(coefs2) == "(Intercept)"),2] +
          coefs2[which(rownames(coefs2) == "DietInsectivore"),1] + coefs2[which(rownames(coefs2) == "DietInsectivore"),2] ,
        coefs2[which(rownames(coefs2) == "(Intercept)"),1] + coefs2[which(rownames(coefs2) == "(Intercept)"),2] + 
          coefs2[which(rownames(coefs2) == "DietOmnivore"),1] + coefs2[which(rownames(coefs2) == "DietOmnivore"),2])

diet2 <- cbind(d1,d2,d3,d4) %>% 
  as.data.frame() %>% 
  mutate(d2 = as.numeric(d2),
         d3 = as.numeric(d3),
         d4 = as.numeric(d4))


coefs2 <- summary_model$p.table

ggarrange(#labels = c("A", "B", "C", "D"),
  ncol = 2, #nrow = 2,
  
  ggplot(diet2) +
    geom_blank() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_segment(aes(x = d1, y = d3, xend = d1, yend = d4), colour = "darkgray") +
    geom_point(aes(x = d1, y = d2), col = 'red', size = 2) +
    geom_point(aes(x = d1, y = d3), col = 'darkgray', shape = "-", size = 7) +
    geom_point(aes(x = d1, y = d4), col = 'darkgray', shape = "-", size = 7) + 
    labs(#title="Diet group",
         #y = "Log(Mean Speed)",
         x = "Diet group") , 
  
  ggplot() +
    geom_blank() +
    scale_x_continuous(limits=c(0,0.5)) +
    scale_y_continuous(limits=c(0,6)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1]),
                    slope = (coefs2[which(rownames(coefs2) == "winlat"),1])), col = 'red')+
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] - 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "winlat"),1] - 
                               coefs2[which(rownames(coefs2) == "winlat"),2])), col = 'gray') +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] + 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "winlat"),1] + 
                               coefs2[which(rownames(coefs2) == "winlat"),2])), col = 'gray') +
    labs(#title="Migration distance",
      #y = "Log(Mean Speed)", 
      x = "Wintering latitude") ,
  
  
  ggplot() +
    geom_blank() +
    scale_x_continuous(limits=c(0,0.5)) +
    scale_y_continuous(limits=c(-1,8)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(aes(intercept = coefs2[which(rownames(coefs2) == "(Intercept)"),1], 
                    slope =coefs2[which(rownames(coefs2) == "xi_mean"),1]), col = 'red') +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] - 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "xi_mean"),1] - 
                               coefs2[which(rownames(coefs2) == "xi_mean"),2])), col = 'gray') +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] + 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "xi_mean"),1] + 
                               coefs2[which(rownames(coefs2) == "xi_mean"),2])), col = 'gray') +
    labs(#title="Species sensitivity",
      y = "Log(Mean Speed)", 
      x = "Sensitivity") ,
  
  ggplot() +
    geom_blank() +
    scale_x_continuous(limits=c(80,120)) +
    scale_y_continuous(limits=c(-1,8)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1]), 
                    slope = (coefs2[which(rownames(coefs2) == "ea_lat"),1])), col = 'red') +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] - 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "ea_lat"),1] - 
                               coefs2[which(rownames(coefs2) == "ea_lat"),2])), col = 'gray') +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] + 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "ea_lat"),1] + 
                               coefs2[which(rownames(coefs2) == "ea_lat"),2])), col = 'gray') +
    labs(#title="Early arrival date",
      #y = "Log(Mean Speed)", 
      x = "Arrival date") ,
  
  ggplot() +
    geom_blank() +
    scale_x_continuous(limits=c(0,8000)) +
    scale_y_continuous(limits=c(0,6)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1]),
                                 slope = (coefs2[which(rownames(coefs2) == "Distance_m"),1])), col = 'red')+
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] - 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "Distance_m"),1] - 
                               coefs2[which(rownames(coefs2) == "Distance_m"),2])), col = 'gray') +
    geom_abline(aes(intercept = (coefs2[which(rownames(coefs2) == "(Intercept)"),1] + 
                                   coefs2[which(rownames(coefs2) == "(Intercept)"),2]), 
                    slope = (coefs2[which(rownames(coefs2) == "Distance_m"),1] + 
                               coefs2[which(rownames(coefs2) == "Distance_m"),2])), col = 'gray') +
    labs(#title="Migration distance",
         #y = "Log(Mean Speed)", 
         x = "Migration distance") 
)








