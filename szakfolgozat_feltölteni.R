
##Választási adatok modellezése

##Csomagok betöltése
library(raster)
library(gstat)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(sf)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringi)
library(stringr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(spdep)
library(spatialreg)
library(sp)
library(sphet)
library(broom)

##Adatok beolvasása

setwd("C:/Users/bariv/OneDrive/Desktop/szakdoló")
#Téri adatok
my_sf <- read_sf("telepules_w_bp.json") 

my_sf <- my_sf %>%
  group_by(NAME) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(telepules=tolower(NAME))

adat <- read_spss("AGG_valasztasi_adatok_osszevont_nepszamlalas_v5 (1).sav")

#Tisztítás

adat$telepules[str_detect(adat$telepules, "budapest")] <- paste(adat$telepules[str_detect(adat$telepules, "budapest")], ". kerület", sep="")

adat %>%
  select(teir_2022_szja_adofizeto_100lakosra, teir_2022_egylakos_szja_jov) %>%
  tab_corr()

#Szűrés
adat <-   adat %>%
  select(telepules, ksh_2011_katolikus, ksh_2011_reformatus, ksh_2011_evangelikus, ksh_2011_ortodox,ksh_2011_maskeresztenyfelekezet, ksh_2011_ferfi, ksh_2011_no) %>%
  mutate(across(everything(), ~tidyr::replace_na(., 0))) %>%
  mutate(kereszteny2011_rate=(ksh_2011_katolikus + ksh_2011_reformatus + 
                                ksh_2011_evangelikus + ksh_2011_ortodox + ksh_2011_maskeresztenyfelekezet)/(ksh_2011_ferfi+ksh_2011_no)) %>%
  select(telepules, kereszteny2011_rate) %>%
  left_join(adat, .)

adat <-   adat %>%
  select(telepules, ksh_2022_katolikus, ksh_2022_reformatus, ksh_2022_evangelikus, ksh_2022_ortodox,ksh_2022_maskeresztenyfelekezet, ksh_2022_ferfi, ksh_2022_no) %>%
  mutate(across(everything(), ~tidyr::replace_na(., 0))) %>%
  mutate(kereszteny2022_rate=(ksh_2022_katolikus + ksh_2022_reformatus + 
                                ksh_2022_evangelikus + ksh_2022_ortodox + ksh_2022_maskeresztenyfelekezet)/(ksh_2022_ferfi+ksh_2022_no),
         lakos2022=ksh_2022_ferfi+ksh_2022_no) %>%
  select(telepules, kereszteny2022_rate, lakos2022) %>%
  left_join(adat, .)

adat <- adat %>%
  select(telepules, ksh_2022_ferfi, ksh_2022_no,
         ksh_2022_60.69eves, ksh_2022_70.79eves, ksh_2022_80.89eves,
         ksh_2022_90eves_es_idosebb, ksh_2022_egyetem_foiskola, ksh_2022_altalanosiskola8.evfolyam, ksh_2022_8_evfolyamnal_alacsonyabb,
         ksh_2022_10evesnel_fiatalabb, ksh_2022_10.19eves) %>%
  mutate(across(everything(), ~tidyr::replace_na(., 0))) %>%
  mutate(kor60_100=(ksh_2022_60.69eves+ksh_2022_70.79eves+ksh_2022_80.89eves+
                      ksh_2022_90eves_es_idosebb)/(ksh_2022_ferfi+ksh_2022_no),
         kor70_100=(ksh_2022_70.79eves+ksh_2022_80.89eves+
                      ksh_2022_90eves_es_idosebb)/(ksh_2022_ferfi+ksh_2022_no),
         nyolcalt=(ksh_2022_altalanosiskola8.evfolyam+ksh_2022_8_evfolyamnal_alacsonyabb)/(ksh_2022_ferfi+ksh_2022_no),
         diplomas=ksh_2022_egyetem_foiskola/(ksh_2022_ferfi+ksh_2022_no-ksh_2022_10evesnel_fiatalabb-ksh_2022_10.19eves))  %>%
  select(telepules, kor60_100, kor70_100, nyolcalt, diplomas) %>%
  left_join(adat, .)

##Baloldal
adat <- adat %>%
  select(telepules, ep2024_Momentum, ep2024_MKKP, ep2024_LMP_Zöldek, ep2024_DK.MSZP.Párbeszéd..ZÖLDEK,
         ep2019_mszp_parbeszed, ep2019_mkkp, ep2019_momentum, ep2019_dk,
         ep2019_munkaspart, ep2019_lmp) %>%
  mutate(across(everything(), ~tidyr::replace_na(., 0))) %>%
  mutate(ep2024_bal=ep2024_Momentum+ep2024_MKKP+ep2024_LMP_Zöldek+ep2024_DK.MSZP.Párbeszéd..ZÖLDEK,
         ep2019_bal=ep2019_mszp_parbeszed+ep2019_mkkp+ep2019_momentum+ep2019_dk+
           ep2019_munkaspart+ep2019_lmp) %>%
  select(telepules, ep2024_bal, ep2019_bal) %>%
  left_join(adat, .)

#lakos kategóriák
adat$lakos2022_kat <- rec(adat$lakos2022, rec="1:200=1 [Kevesebb mint 200]; 201:500=2 [200-500 között];
    501:1000=3 [500-1e között]; 1001:2000=4 [1-2e között];2001:5000=5 [2-5ezer között]; 5001:20000=6 [5-20e között];
    else=7 [20e felett]")

#csoportosítás
adat_spatial <- adat %>%
  select(telepules, ep2024_TISZA, ep2024_FIDESZ.KDNP, ep2024_bal, ep2024_a, ep2024_n, ep2019_nepesseg, ep2019_ervenyes, ep2019_fidesz, ep2019_bal, kereszteny2011_rate, kereszteny2022_rate,
         kor60_100, kor70_100, nyolcalt, diplomas, teir_2022_szja_adofizeto_100lakosra,
         teir_2022_egylakos_szja_jov, lakos2022, lakos2022_kat, magyar_peter_orszagjaras) %>%
  na.omit() %>%
  mutate(reszvetel24=ep2024_n/ep2024_a, fidesz24_rate=ep2024_FIDESZ.KDNP/ep2024_n, fidesz19_rate=ep2019_fidesz/ep2019_ervenyes,
         tisza24_rate=ep2024_TISZA/ep2024_n, bal24_rate=ep2024_bal/ep2024_n, reszvetel2019=ep2019_ervenyes/ep2019_nepesseg,
         bal19_rate=ep2019_bal/ep2019_ervenyes) %>%
  mutate(fidesz2419_dif=fidesz24_rate-fidesz19_rate, 
         kereszteny_dif=kereszteny2022_rate-kereszteny2011_rate,
         bal_dif=bal24_rate-bal19_rate) %>%
  left_join(my_sf, .) %>%
  na.omit() %>%
  rename("adofizeto_arany"=teir_2022_szja_adofizeto_100lakosra, 
         "szja_atlag"=teir_2022_egylakos_szja_jov) %>%
  mutate(szja_atlag_log=log(szja_atlag)/10-1, adofizeto_arany=adofizeto_arany/100, 
         lakos2022_log=log(lakos2022)/10, reszvetel_diff=reszvetel24-reszvetel2019)


#Alapstatisztikák lekérése
adat_spatial  %>%
  select(-c(geometry, NAME, telepules)) %>%
  as_tibble() %>%
  descr(.)

tapply(adat_spatial$fidesz2419_dif, adat_spatial$lakos2022_kat, mean)
tapply(adat_spatial$fidesz2419_dif, adat_spatial$lakos2022_kat, sd)
nb <- poly2nb(adat_spatial)
listw <- nb2listw(nb, style="W")

##Moran tesztek

moran.test(adat_spatial$fidesz24_rate, listw)
moran.test(adat_spatial$fidesz19_rate, listw)
moran.test(adat_spatial$tisza24_rate, listw)
moran.test(adat_spatial$fidesz2419_dif, listw)
moran.test(adat_spatial$bal_dif, listw)
moran.test(adat_spatial$bal24_rate, listw)


adat_spatial %>%
  select(reszvetel24, reszvetel2019, lakos2022_log) %>%
  as_tibble() %>%
  select(-c(geometry)) %>%
  tab_corr()

##Simított változók
centroids <- st_centroid(adat_spatial)
coords <- st_coordinates(centroids)

#grid
bbox <- st_bbox(adat_spatial)
x.range <- seq(bbox[1], bbox[3], by = 0.05)
y.range <- seq(bbox[2], bbox[4], by = 0.05)
grd <- expand.grid(x = x.range, y = y.range)
coordinates(grd) <- ~x+y
gridded(grd) <- TRUE


#Fidesz24
values <- centroids$fidesz24_rate
sp_data <- SpatialPointsDataFrame(coords, data.frame(values=values))

idw <- idw(values ~ 1, sp_data, newdata = grd, idp = 2)
idw_raster <- raster::raster(idw)
shp_data_sp <- as(adat_spatial, "Spatial")
idw_masked <- raster::mask(idw_raster, shp_data_sp)
idw_values <- extract(idw_masked, shp_data_sp, fun = mean, na.rm = TRUE)

adat_spatial$fidesz24_smoothed <- idw_values


#Tisza24
values <- centroids$tisza24_rate
sp_data <- SpatialPointsDataFrame(coords, data.frame(values=values))

idw <- idw(values ~ 1, sp_data, newdata = grd, idp = 2)
idw_raster <- raster::raster(idw)
shp_data_sp <- as(adat_spatial, "Spatial")
idw_masked <- raster::mask(idw_raster, shp_data_sp)
idw_values <- extract(idw_masked, shp_data_sp, fun = mean, na.rm = TRUE)

adat_spatial$tisza24_smoothed <- idw_values


#Bal24
values <- centroids$bal24_rate
sp_data <- SpatialPointsDataFrame(coords, data.frame(values=values))

idw <- idw(values ~ 1, sp_data, newdata = grd, idp = 2)
idw_raster <- raster::raster(idw)
shp_data_sp <- as(adat_spatial, "Spatial")
idw_masked <- raster::mask(idw_raster, shp_data_sp)
idw_values <- extract(idw_masked, shp_data_sp, fun = mean, na.rm = TRUE)

adat_spatial$bal24_smoothed <- idw_values

#Fidesz_diff
values <- centroids$fidesz2419_dif
sp_data <- SpatialPointsDataFrame(coords, data.frame(values=values))

idw <- idw(values ~ 1, sp_data, newdata = grd, idp = 2)
idw_raster <- raster::raster(idw)
shp_data_sp <- as(adat_spatial, "Spatial")
idw_masked <- raster::mask(idw_raster, shp_data_sp)
idw_values <- extract(idw_masked, shp_data_sp, fun = mean, na.rm = TRUE)

adat_spatial$fidesz2419_dif_smoothed <- idw_values


#Bal_diff
values <- centroids$bal_dif
sp_data <- SpatialPointsDataFrame(coords, data.frame(values=values))

idw <- idw(values ~ 1, sp_data, newdata = grd, idp = 2)
idw_raster <- raster::raster(idw)
shp_data_sp <- as(adat_spatial, "Spatial")
idw_masked <- raster::mask(idw_raster, shp_data_sp)
idw_values <- extract(idw_masked, shp_data_sp, fun = mean, na.rm = TRUE)

adat_spatial$bal2419_dif_smoothed <- idw_values

##Térképek

ggplot(data = adat_spatial) +
  geom_sf(aes(fill = fidesz24_rate)) +
  scale_fill_viridis_b(name = "Fidesz arány") +
  ggtitle("2024 Fidesz EP") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = fidesz24_smoothed), color = NA) +
  scale_fill_viridis_b(name = "Fidesz simitott\nbinned") +
  ggtitle("2024 Fidesz EP - simított") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = tisza24_rate)) +
  scale_fill_viridis_b(name = "Tisza arány", option="H") +
  ggtitle("2024 TISZA EP") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = tisza24_smoothed), color = NA) +
  scale_fill_viridis_b(name = "Tisza simitott\nbinned") +
  ggtitle("2024 Tisza EP - simított") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = bal24_rate)) +
  scale_fill_viridis_b(name = "Baloldali pártok arány", option="H") +
  ggtitle("2024 Baloldali pártok EP") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = bal24_smoothed), color = NA) +
  scale_fill_viridis_b(name = "Baloldal simitott\nbinned") +
  ggtitle("2024 Bal EP - simított") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = fidesz2419_dif)) +
  scale_fill_viridis_b(name = "Fidesz differencia", option="H") +
  ggtitle("2024-2019 Fidesz") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = fidesz2419_dif_smoothed), color = NA) +
  scale_fill_viridis_b(name = "Fidesz differencia\nsimitott + binned") +
  ggtitle("2024-2019 Fidesz EP - simított") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = bal_dif)) +
  scale_fill_viridis_b(name = "Baloldal differencia", option="H") +
  ggtitle("2024-2019 baloldali pártok") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


ggplot(data = adat_spatial) +
  geom_sf(aes(fill = bal2419_dif_smoothed), color = NA) +
  scale_fill_viridis_b(name = "Baloldal differencia\nsimitott + binned") +
  ggtitle("2024-2019 Baloldal EP - simított") +
  theme_minimal() +
  #  geom_sf_label(aes(label=label), size=3) + 
  theme(legend.position = "right") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme(aspect.ratio = 1 / 1.5)


##Modellek
#országjárás beépítése
adat_spatial$magyar_peter_orszagjaras <- as.numeric(adat_spatial$magyar_peter_orszagjaras)
#korrelációk
adat_spatial %>%
  as_tibble() %>%
  select(fidesz24_rate, fidesz2419_dif, bal24_rate ,bal_dif, tisza24_rate, kereszteny_dif, 
         kor60_100, nyolcalt, diplomas, adofizeto_arany, szja_atlag_log, magyar_peter_orszagjaras) %>%
  tab_corr()

##SZJA átlagot kivesszük a modellből mert nagyon korreál más változókkal

#OLS MODEL

ols_model1 <- lm(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                   adofizeto_arany  +  lakos2022_log, 
                 data = adat_spatial)

tab_model(ols_model1, show.std=T)

##A folytonos log lakosságszámot használjuk

##SLM model
slm_model <- lagsarlm(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                        adofizeto_arany  +  lakos2022_log, 
                      data = adat_spatial, 
                      listw = listw)

summary(slm_model, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(slm_model, listw=listw)
moran.test(slm_model$residuals, listw)

#Ellenőrzések
qqnorm(slm_model$residuals)
qqline(slm_model$residuals, col = "red")


hist(slm_model$residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals", probability = TRUE)
xfit <- seq(min(slm_model$residuals), max(slm_model$residuals), length = 40)
yfit <- dnorm(xfit, mean = mean(slm_model$residuals), sd = sd(slm_model$residuals))
lines(xfit, yfit, col = "blue", lwd = 2)


shapiro.test(slm_model$residuals)
ks.test(slm_model$residuals, "pnorm", mean = mean(slm_model$residuals), sd = sd(slm_model$residuals))

#Heteroskedasticity

plot(slm_model$fitted.values, slm_model$residuals, 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


bptest.Sarlm(slm_model)
bptest.Sarlm(slm_model, studentize=FALSE)

plot(adat_spatial$lakos2022, slm_model$residuals, 
     xlab = "Lakosságszám", ylab = "Residuals", 
     main = "Residuals vs Lakosságszám")
abline(h = 0, col = "red")


#SPD Durbin Model

sdm_model <- lagsarlm(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                        adofizeto_arany +  lakos2022_log, 
                      data = adat_spatial, 
                      listw = listw, type="mixed", Durbin=T)

summary(sdm_model, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(sdm_model, listw=listw)

#Error model

sem_model <- errorsarlm(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                          adofizeto_arany +  lakos2022_log, 
                        data = adat_spatial, 
                        listw = listw)


summary(sem_model, Nagelkerke = T,
        Hausman=T)

#SPHET MODELL
sphet_model <- spreg(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                       adofizeto_arany +  lakos2022_log, 
                     data = adat_spatial, 
                     listw = listw, het=T, model="lag", Durbin=F)



summary(sphet_model, Nagelkerke = T,
        Hausman=T)
sphet::impacts(sphet_model, listw=listw)

#Modellek összekapcsolása

ols_fidesz24 <-  ols_model1
slm_fidesz24 <-  slm_model
sdm_fidesz24 <-  sdm_model
sem_fidesz24 <- sem_model
sphet_fidesz24 <- sphet_model

tidy_model <- ols_fidesz24 %>%
  tidy() %>%
  mutate(model="OLS") 

tidy_model <- slm_fidesz24 %>%
  tidy() %>%
  mutate(model="SLM")  %>%
  bind_rows(tidy_model, .)


tidy_model <- sdm_fidesz24 %>%
  tidy() %>%
  mutate(model="SDM")  %>%
  bind_rows(tidy_model, .)

tidy_model <- sem_fidesz24 %>%
  tidy() %>%
  mutate(model="SEM")  %>%
  bind_rows(tidy_model, .)


sphet_fidesz24.matrix <- as.matrix(summary(sphet_fidesz24)[[12]])

tidy_sphet <- tibble(term=rownames(sphet_fidesz24.matrix), estimate=sphet_fidesz24.matrix[,1],
                     std.error=sphet_fidesz24.matrix[,2],statistic=sphet_fidesz24.matrix[,3],
                     p.value=sphet_fidesz24.matrix[,4], model="SPHET")

tidy_model <- tidy_sphet %>%
  bind_rows(tidy_model, .)

rm(sphet_fidesz24.matrix, tidy_sphet)


tidy_model <- tidy_model[tidy_model$term!="rho",]
tidy_model <- tidy_model[tidy_model$term!="lambda",]
tidy_model <- tidy_model[tidy_model$term!="(Intercept)",]
tidy_model <- tidy_model[substr(tidy_model$term,1,4)!="lag.",]

#Coefficient plot
ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "darkred","navyblue","pink")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Models - Fidesz24",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###Egyéb slm modellek

#Behozzuk a Tisza országjárást

sdm_fidesz24_v2 <- lagsarlm(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                              adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras, 
                            data = adat_spatial, 
                            listw = listw)

sdm_fidesz24_reszvetel <- lagsarlm(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                                     adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras + reszvetel24, 
                                   data = adat_spatial, 
                                   listw = listw)



summary(sdm_fidesz24_v2, Nagelkerke = T,
        Hausman=T)

summary(sdm_fidesz24_reszvetel, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(sdm_fidesz24_v2, listw=listw)

##Tisza24

sdm_tisza <- lagsarlm(tisza24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                        adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras, 
                      data = adat_spatial, 
                      listw = listw)

summary(sdm_tisza, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(sdm_tisza, listw=listw)

##Ezt ábrázoljuk tidyval

tidy_model <- sdm_fidesz24_v2 %>%
  tidy() %>%
  mutate(model="FIDESZ24")


tidy_model <- sdm_tisza %>%
  tidy() %>%
  mutate(model="TISZA24")  %>%
  bind_rows(tidy_model, .)

tidy_model <- tidy_model[tidy_model$term!="rho",]
tidy_model <- tidy_model[tidy_model$term!="lambda",]
tidy_model <- tidy_model[tidy_model$term!="(Intercept)",]


ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "navyblue")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


tidy_model <- tidy_model[substr(tidy_model$term,1,4)!="lag.",]

ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "navyblue")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##Fidesz19

sdm_fidesz19 <- lagsarlm(fidesz19_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                           adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras, 
                         data = adat_spatial, 
                         listw = listw)

sdm_fidesz19_reszvetel <- lagsarlm(fidesz19_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                                     adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras + reszvetel2019, 
                                   data = adat_spatial, 
                                   listw = listw)

summary(sdm_fidesz19, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(sdm_fidesz19, listw=listw)

##Ezt ábrázoljuk tidyval

tidy_model <- sdm_fidesz24_v2 %>%
  tidy() %>%
  mutate(model="FIDESZ24")


tidy_model <- sdm_fidesz19 %>%
  tidy() %>%
  mutate(model="FIDESZ19")  %>%
  bind_rows(tidy_model, .)

tidy_model <- tidy_model[tidy_model$term!="rho",]
tidy_model <- tidy_model[tidy_model$term!="lambda",]
tidy_model <- tidy_model[tidy_model$term!="(Intercept)",]


ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "grey")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


tidy_model <- tidy_model[substr(tidy_model$term,1,4)!="lag.",]

ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "grey")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###Részvétellel kiigazított modell

##Ezt ábrázoljuk tidyval

tidy_model <- sdm_fidesz24_reszvetel %>%
  tidy() %>%
  mutate(model="FIDESZ24")


tidy_model <- sdm_fidesz19_reszvetel %>%
  tidy() %>%
  mutate(model="FIDESZ19")  %>%
  bind_rows(tidy_model, .)

tidy_model <- tidy_model[tidy_model$term!="rho",]
tidy_model <- tidy_model[tidy_model$term!="lambda",]
tidy_model <- tidy_model[tidy_model$term!="(Intercept)",]


tidy_model <- tidy_model[substr(tidy_model$term,1,4)!="lag.",]

ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "grey")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###24 adatsor

tidy_model <- sdm_fidesz24_v2 %>%
  tidy() %>%
  mutate(model="FIDESZ24")


tidy_model <- sdm_fidesz19 %>%
  tidy() %>%
  mutate(model="FIDESZ19")  %>%
  bind_rows(tidy_model, .)

tidy_model <- sdm_fidesz24_reszvetel %>%
  tidy() %>%
  mutate(model="FIDESZ24 - részvétel") %>%
  bind_rows(tidy_model, .)


tidy_model <- sdm_fidesz19_reszvetel %>%
  tidy() %>%
  mutate(model="FIDESZ19 - részvétel")  %>%
  bind_rows(tidy_model, .)

tidy_model <- tidy_model[tidy_model$term!="rho",]
tidy_model <- tidy_model[tidy_model$term!="lambda",]
tidy_model <- tidy_model[tidy_model$term!="(Intercept)",]


tidy_model <- tidy_model[substr(tidy_model$term,1,4)!="lag.",]

ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "grey", "lightblue", "darkgreen")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Részvétel
sdm_reszvetel_diff <- lagsarlm(reszvetel_diff ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                                 adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras, 
                               data = adat_spatial, 
                               listw = listw)


tidy_model <- sdm_fidesz24_v2 %>%
  tidy() %>%
  mutate(model="Részvétel differencia")


tidy_model <- tidy_model[tidy_model$term!="rho",]
tidy_model <- tidy_model[tidy_model$term!="lambda",]
tidy_model <- tidy_model[tidy_model$term!="(Intercept)",]


tidy_model <- tidy_model[substr(tidy_model$term,1,4)!="lag.",]

ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error, color=model),position = position_dodge(0.75)) +
  scale_color_manual(values = c("navyblue")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Spatial Lag Models",
       x = "Predictors",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##Scatter plotok

ggplot(adat_spatial, aes(x=kereszteny2022_rate, y=fidesz2419_dif)) + 
  geom_point()+
  geom_smooth(method=lm)+   theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'


ggplot(adat_spatial, aes(x=adofizeto_arany, y=fidesz2419_dif)) + 
  geom_point()+
  geom_smooth(method=lm)+   theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'


ggplot(adat_spatial, aes(x=lakos2022_log, y=fidesz2419_dif)) + 
  geom_point()+
  geom_smooth(method=lm)+   theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'


ggplot(adat_spatial, aes(x=kereszteny_dif, y=fidesz2419_dif)) + 
  geom_point()+
  geom_smooth(method=lm)+   theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'

##slm a fidesz változásra
sdm_fidesz_dif <- lagsarlm(fidesz2419_dif ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                             adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras, 
                           data = adat_spatial, 
                           listw = listw)

summary(sdm_fidesz_dif, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(sdm_fidesz_dif, listw=listw)

#Bal diff
descr(adat_spatial$bal_dif)

sdm_bal_dif <- lagsarlm(bal_dif ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                          adofizeto_arany +  lakos2022_log + magyar_peter_orszagjaras, 
                        data = adat_spatial, 
                        listw = listw)

summary(sdm_bal_dif, Nagelkerke = T,
        Hausman=T)

spatialreg::impacts(sdm_bal_dif, listw=listw)

sphet_model24 <- spreg(fidesz24_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                         adofizeto_arany +  lakos2022_log, 
                       data = adat_spatial, 
                       listw = listw, het=T, model="lag", Durbin=F)

sphet_model19 <- spreg(fidesz19_rate ~  kereszteny2022_rate+  kereszteny_dif + kor60_100 + nyolcalt + diplomas +
                         adofizeto_arany +  lakos2022_log, 
                       data = adat_spatial, 
                       listw = listw, het=T, model="lag", Durbin=F)


# Összegző mátrixok kinyerése
sphet24_tab <- summary(sphet_model24)$CoefTable
sphet19_tab <- summary(sphet_model19)$CoefTable

# Átalakítás tidy formába
tidy_model <- bind_rows(
  tibble(term = rownames(sphet24_tab), estimate = sphet24_tab[,1], std.error = sphet24_tab[,2],
         statistic = sphet24_tab[,3], p.value = sphet24_tab[,4], model = "FIDESZ24"),
  tibble(term = rownames(sphet19_tab), estimate = sphet19_tab[,1], std.error = sphet19_tab[,2],
         statistic = sphet19_tab[,3], p.value = sphet19_tab[,4], model = "FIDESZ19")
)

# Nem kellő változók kiszűrése
tidy_model <- tidy_model %>%
  filter(!term %in% c("rho", "lambda", "(Intercept)")) %>%
  filter(!startsWith(term, "lag."))

# Ábrázolás
ggplot(tidy_model, aes(x = term, y = estimate, color = model)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  position = position_dodge(0.75)) +
  scale_color_manual(values = c("orange", "grey")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "SPHET modellek – Fidesz támogatottság 2019 és 2024",
       x = "Magyarázó változók",
       y = "Becslések") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


