bg.mat <- bg.mat %>% 
mutate(cell = as.factor(cell),
       lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps"))))
       
bg.matG <- bg.matG %>% 
  mutate(cell = as.factor(cell),
         lat_g = ifelse(cell_lat<34, 'low', ifelse(cell_lat > 34 & cell_lat < 42, "mid", ifelse(cell_lat > 42, "high", "opps"))))


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

bg.mat %>% 
  group_by(species) %>% 
  mutate(max_lat = max(cell_lat, na.rm = T),
         mean_vel = mean(vArrMag_log, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = max_lat, y = mean_vel)) +
  theme_bw() +
  geom_smooth(aes(x = max_lat, y = mean_vel), method = "lm", se = F)


bg.mat %>% 
  group_by(species) %>% 
  mutate(med_lat = median(cell_lat, na.rm = T),
         mean_anomv = mean(AnomVArr, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = AnomVArr, colour = species, show.legend = F)) +
  theme_bw() +
  coord_cartesian(xlim = c(38, 50)) +
  geom_smooth(aes(x = cell_lat, y = AnomVArr, colour = species), method = "lm", se = F,
              show.legend = F)

bg.mat %>% 
  ggplot() +
  #geom_point(data = bg.matG, aes(x = cell_lat, y = vGrMag)) +
  #coord_cartesian(ylim = c(400, 600)) + # this does not remove points from analysis
  theme_bw() +
  geom_point(data = bg.mat %>% filter(who_n == "mig"),
              aes(x = cell_lat, y = vArrMag_log, colour = species, group = species), 
              method = "lm",
              se = F) 


bg.mat %>% 
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(year), y = AnomVArr, fill = who)) +
  coord_cartesian(ylim = c(-2.5, 4)) + theme_bw() +
  geom_hline(yintercept = 0, color = "red") + facet_rep_wrap(~who)
  
bg.matG %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(year), y = AnomVGr)) +
  coord_cartesian(ylim = c(-2.5, 4)) + theme_bw() +
  geom_hline(yintercept = 0, color = "red") 

bg.mat %>% 
  filter(mig_cell == T) %>% 
  ggplot(aes(x = vGrMag_log, y = vArrMag_log)) +
  geom_point() +
  geom_smooth( se = F) + theme_bw() 

bg.mat %>% 
  filter(breed_cell == T) %>% 
  ggplot(aes(x = vGrMag_log, y = vArrMag_log)) +
  geom_point() +
  geom_smooth( se = F) + theme_bw() 

bg.mat %>% 
  ggplot(aes(x = AnomVGr, y = AnomVArr)) +
  geom_point() +
  geom_smooth( se = F) + theme_bw() 


bg.mat %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(year), y = vArrMag_log)) +
  coord_cartesian(ylim = c(0, 10)) + theme_bw() 

bg.matG %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(year), y = vGrMag_log)) +
  coord_cartesian(ylim = c(0,10)) + theme_bw() 



ggplot() +
  geom_smooth(data = bg.mat %>% filter(mig_cell == T, breed_cell == F),
             aes(x = cell_lat, y = vArrMag), color = "blue",
             method = "lm", se = F) +
  geom_smooth(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
             aes(x = cell_lat, y = vArrMag), color = "red",
             method = "lm", se = F) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,800)) +
  facet_rep_wrap(~year)

ggplot() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
             aes(x = lag, y = vArrMag), color = "red") +
  geom_point(data = bg.mat %>% filter(mig_cell == T, breed_cell == F),
              aes(x = lag, y = vArrMag), color = "blue") +
  theme_bw() +
  #coord_cartesian(ylim = c(0,800)) +
  facet_rep_wrap(~year)




ggplot() +
  geom_point(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
             aes(x = cell_lat, y = vArrMag), color = "red", alpha = 0.1) +
  geom_point(data = bg.mat %>% filter(mig_cell == T, breed_cell == F),
              aes(x = cell_lat, y = vArrMag), color = "blue", alpha = 0.1) +
  geom_smooth(data = bg.mat %>% filter(mig_cell == T, breed_cell == F),
              aes(x = cell_lat, y = vArrMag), color = "blue",
              method = "lm", se = F) +
  geom_smooth(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
              aes(x = cell_lat, y = vArrMag), color = "red",
              method = "lm", se = F) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,800)) +
  facet_rep_wrap(~year)

ggplot() +
  geom_boxplot(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
             aes(x = cell_lat, y = vArrMag_log), fill = "red", alpha = 0.5) +
  coord_cartesian(ylim = c(3,10)) +
  theme_bw()
ggplot() +
  geom_boxplot(data = bg.mat %>% filter(breed_cell == F, mig_cell == T),
               aes(x = cell_lat, y = vArrMag_log), fill = "blue", alpha = 0.5) +
  coord_cartesian(ylim = c(3,10)) +
  theme_bw()

a <- bg.mat
cor(a$vArrMag, a$cell_lat, "complete.obs")

b <- bg.mat %>% 
  filter(mig_cell == T, breed_cell == F)
cor(b$vArrMag, b$cell_lat, "complete.obs")

c <- bg.mat %>% 
 filter(mig_cell == F, breed_cell == T)
cor(c$vArrMag, c$cell_lat, "complete.obs")


ggplot() +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(data = bg.mat %>% filter(mig_cell == T, breed_cell == F),
              aes(x = lag, y = vArrMag), color = "blue",
              #method = "lm", 
              se = F) +
  geom_smooth(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
              aes(x = lag, y = vArrMag), color = "red",
              #method = "lm", 
              se = F) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,800)) +
  facet_rep_wrap(~year
                 , scales = "free"
                 )

ggplot() +
  geom_smooth(data = bg.mat %>% filter(mig_cell == T, breed_cell == F),
              aes(x = cell_lat, y = AnomVArr), color = "blue",
              method = "lm", se = F) +
  geom_smooth(data = bg.mat %>% filter(breed_cell == T, mig_cell == F),
              aes(x = cell_lat, y = AnomVArr), color = "red",
              method = "lm", se = F) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.05,0.05))


a <- bg.mat %>% 
  filter(mig_cell == T, breed_cell == F) %>% 
  group_by(species) %>% 
  mutate(dist_mig = max(cell_lat) - min(cell_lat)) %>% 
  ungroup() %>% 
  dplyr::select(species, cell, year, dist_mig)

b <- left_join(bg.mat, a, by = c('species', 'cell', 'year')) 
ggplot(b) +
  geom_point(aes(x = dist_mig, y = vArrMag)) +
  geom_smooth(aes(x = dist_mig, y = vArrMag), color = "blue",
              method = "lm", se = F) +
  theme_bw() 

c <- bg.mat %>% 
      filter(mig_cell == F, breed_cell == T) %>% 
      group_by(species) %>% 
      mutate(bre_dat = min(arr_GAM_mean)) %>% 
      ungroup() %>% 
      dplyr::select(species, cell, year, bre_dat)

d <- left_join(bg.mat, c, by = c('species', 'cell', 'year'))
ggplot(d) +
  geom_point(aes(x = bre_dat, y = vArrMag_log)) +
  geom_smooth(aes(x = bre_dat, y = vArrMag_log), color = "blue",
              method = "lm", se = F) +
  theme_bw() 

  summary(lm(vArrMag~bre_dat, data = d))
  
  
c <- bg.mat %>% 
    filter(mig_cell == T, breed_cell == F) %>% 
    group_by(species) %>% 
    mutate(mig_dat = min(arr_GAM_mean)) %>% 
    ungroup() %>% 
    dplyr::select(species, cell, year, mig_dat)
  
d <- left_join(bg.mat, c, by = c('species', 'cell', 'year'))
  ggplot(d) +
    geom_point(aes(x = mig_dat, y = vArrMag_log, color = species), show.legend = F) +
    geom_smooth(aes(x = mig_dat, y = vArrMag_log), color = "blue",
                method = "lm", se = F) +
    theme_bw() 
  
  summary(lm(vArrMag~mig_dat, data = d))
  
  
  ggplot(bg.mat, aes(x = cell_lat, y = vArrMag_log, 
                    col = species,
                    alpha = 0.1)) + 
    geom_point(show.legend = FALSE) +
    geom_smooth(aes(x = cell_lat, y = vArrMag_log), color = "black",
                     show.legend = FALSE, se = FALSE, method = "lm") + theme_bw() +
    facet_rep_wrap(~year)
  
  
ggplot(bg.mat, aes(x = arr_GAM_mean, y = vArrMag_log)) +
  geom_point(aes(color = species),show.legend = F) +
  geom_smooth(show.legend = F)
  
  
bg.mat %>%
  #filter(who_n == "mig") %>% 
  ggplot() +
  geom_boxplot(aes(x = lat_g, y = vArrMag_log)) +
  theme_bw() 

bg.mat %>%
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_point(aes(x = AnomDArr, y = AnomVArr, col = who)) +
  geom_smooth(aes(x = AnomDArr, y = AnomVArr, col = who), method = "lm", se = F) +
  coord_cartesian(xlim = c(-0.2, 0.15)) +
  theme_bw() 


bg.mat %>%
  filter(who_n == "mig") %>% 
  ggplot() +
  geom_point(aes(x = AnomVGr, y = AnomVArr, col = who)) +
  geom_smooth(aes(x = AnomDArr, y = AnomVArr, col = who), method = "lm", se = F) +
  #coord_cartesian(xlim = c(-0.2, 0.15)) +
  theme_bw() 


ggplot(bg.mat, aes(x = cell_lat, y = vArrMag_log,
                  alpha = 0.3)) + 
  geom_point() +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log), show.legend = FALSE, se = FALSE, method = "lm")+
  theme_bw() +
  facet_rep_wrap(~year)

ggplot(bg.mat, aes(x = cell_lat, y = AnomVArr,
                   alpha = 0.3)) + 
  geom_point() +
  geom_smooth(aes(x = cell_lat, y = AnomVArr), show.legend = FALSE, se = FALSE, method = "lm")+
  theme_bw() +
  facet_rep_wrap(~year)


ggplot(bg.mat, aes(x = cell_lat, y = AnomVGr,
                   alpha = 0.3)) + 
  geom_point() +
  geom_smooth(aes(x = cell_lat, y = AnomVGr), show.legend = FALSE, se = FALSE, method = "lm")+
  theme_bw() +
  facet_rep_wrap(~year)

ggplot(bg.mat, aes(x = AnomLag, y = AnomVArr,
                   alpha = 0.3)) + 
  geom_point() +
  geom_smooth(aes(x = AnomLag, y = AnomVArr), show.legend = FALSE, se = FALSE, method = "lm")+
  theme_bw() +
  facet_rep_wrap(~year)


ggplot(bg.mat%>%  filter(who_n == "mig") , aes(x = cell_lat, y = vArrMag_log,
                   alpha = 0.3, col = who)) + 
  geom_point() +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log), show.legend = FALSE, se = FALSE, method = "lm")+
  coord_cartesian(xlim = c(38, 47)) +
  theme_bw() +
  facet_rep_wrap(~year)

bg.mat %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = vArrMag_log)) +
  geom_smooth(aes(x = cell_lat, y = vArrMag_log)) +
  
  facet_rep_wrap(~year_g, ncol = 3) +
  geom_hline(yintercept = 0, color = "black")  +
  theme_bw()


bg.mat %>% 
  ggplot() +
  geom_point(aes(x = cell_lat, y = lag)) +
  geom_smooth(aes(x = cell_lat, y = lag)) +
  
  facet_rep_wrap(~year_g, ncol = 3) +
  geom_hline(yintercept = 0, color = "red")  +
  theme_bw()

## breeding range
a <- bg.mat %>% 
  filter(mig_cell == F, breed_cell == T) %>% 
  group_by(species) %>% 
  mutate(bre_dat = median(arr_GAM_mean)) %>% 
  ungroup() %>% 
  dplyr::select(species, cell, year, bre_dat)

b <- left_join(bg.mat, a, by = c('species', 'cell', 'year')) 

ggplot(b) +
  geom_point(aes(x = bre_dat, y = vArrMag_log, col = species), show.legend = F) +
  facet_rep_wrap(~year)

cor(b$bre_dat, b$vArrMag_log, use = "complete.obs")

bg.mat %>% 
  group_by(species) %>% 
  mutate(bre_dat = median(arr_GAM_mean, na.rm = T),
         mean_vel = mean(vArrMag_log, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x = bre_dat, y = mean_vel)) +
  theme_bw() +
  geom_smooth(aes(x = bre_dat, y = mean_vel), method = "lm", se = F)




a <- bg.mat %>% 
  #filter(mig_cell == T, breed_cell == F) %>% 
  group_by(species) %>% 
  mutate(Farr_dat = min(arr_GAM_mean)) %>% 
  ungroup() %>% 
  dplyr::select(species, cell, year, Farr_dat)

b <- left_join(bg.mat, a, by = c('species', 'cell', 'year')) 
b %>% 
  filter(mig_cell == T) %>% 
ggplot() +
  geom_point(aes(x = Farr_dat, y = vArrMag_log)) +
  geom_smooth(aes(x = Farr_dat, y = vArrMag_log), color = "blue",
              method = "lm", se = F) +
  theme_bw() 
