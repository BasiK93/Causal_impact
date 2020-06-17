#potrzebne pakiety
library(tidyr)
library(ggplot2)
library(dplyr)
library("rgdal")
library(rgeos)
library("mapproj")
library(broom)
library(maptools)
library(gpclib)

#wczytywanie zbioru danych
dane<-read.csv("dane_stat.csv", sep = ";", )

miejsce<-dane %>%
  dplyr::select(Rok, miasto, wies) %>%
  drop_na()

wiek<-dane %>%
  dplyr::select(Rok, wiek1, wiek2, wiek3, wiek4, wiek5, wiek6, wiek7) %>%
  drop_na()

wojewodztwo<- dane %>%
  dplyr::select(c(Rok, Dolnoœl¹skie:Zachodniopomorskie)) %>%
  drop_na()

#miasto-wies
miejsce$Rok<-as.Date(as.character(miejsce$Rok), format="%Y")
miejsce$Niemcy<-panstwa$Niemcy[33:50]
miejsce$Slowacja<-panstwa$S³owacja[33:50]
miejsce$Czechy<-panstwa$Czechy[33:50]
miejsce1<-subset(miejsce, select=-c(wies))
miejsce1_xts <-
  miejsce1 %>%
  dplyr::select(-Rok) %>%
  as.xts(order.by = miejsce$Rok)

miejsce1_causal <- CausalImpact(miejsce1_xts, 
                                pre.period = as.Date(c("2002-06-17", "2015-06-17")), 
                                post.period = as.Date(c("2016-06-17", "2019-06-17")))
miasto<-round(miejsce1_causal$summary[1,1]/miejsce1_causal$summary[1,2]*100-100)

miejsce2<-data.frame(Rok=miejsce$Rok, wies=miejsce$wies, Niemcy=panstwa$Niemcy[33:50], S³owacja=panstwa$S³owacja[33:50], Czechy=panstwa$Czechy[33:50])
miejsce2$Rok<-as.Date(as.character(miejsce$Rok), format="%Y")

miejsce2_xts <-
  miejsce2 %>%
  dplyr::select(-Rok) %>%
  as.xts(order.by = miejsce2$Rok)

miejsce2_causal <- CausalImpact(miejsce2_xts, 
                                pre.period = as.Date(c("2002-06-17", "2015-06-17")), 
                                post.period = as.Date(c("2016-06-17", "2019-06-17")))
wies<-round(miejsce2_causal$summary[1,1]/miejsce2_causal$summary[1,2]*100-100)

nowy<-data.frame(zmiana=c(0), gdzie=c("miasto", "wies"))
nowy$zmiana[1]<-miasto
nowy$zmiana[2]<-wies

ggplot(nowy, aes(x = gdzie, y = zmiana)) +
  geom_bar(stat = "identity", width = 0.5)+
  ylim(-2,2)

#wiek matki
nowy2<-data.frame(wiek=c('wiek1','wiek2','wiek3','wiek4','wiek5','wiek6','wiek7'), zmiana=c(0), p=c(0))
zbior1<-subset(dane, select=c(Rok, Niemcy, Czechy, S³owacja))
zbior1<-zbior1[33:50,]
zbior1$Rok<-as.Date(as.character(zbior1$Rok), format="%Y")
for (i in 2:8){
  a<-paste("wiek", i-1, sep="")
  wiekx<-wiek[i]
  wiek2<-cbind(wiekx, zbior1)
  wiek2_xts <-
    wiek2 %>%
    dplyr::select(-Rok) %>%
    as.xts(order.by = wiek2$Rok)
  wiek2_causal <- CausalImpact(wiek2_xts, 
                               pre.period = as.Date(c("2002-06-17", "2015-06-17")), 
                               post.period = as.Date(c("2016-06-17", "2019-06-17")))
  nowy2$zmiana[i-1]<-round(wiek2_causal$summary[1,1]/wiek2_causal$summary[1,2]*100-100)
  nowy2$p[i-1]<-wiek2_causal$summary[1,15]
}

ggplot(nowy2, aes(x = wiek, y = zmiana)) +
  geom_bar(stat = "identity", fill="darkorange")+
  ylab("%")+
  scale_x_discrete(labels= c("19 i mniej", "20-24 lat", "25-29 lat", "30-34", "35-39", "40-44", "45 i wiêcej"))+
  ggtitle("Zmiana liczby urodzeñ wzglêdem wieku matki")+
  theme_minimal()

####wojewodztwo

#wczytywanie mapy Polski
wojew <- readOGR("mapy/Województwa.shp")
wojew <- spTransform(wojew, CRS("+init=epsg:4326"))

wojew_nazwy <- wojew@data %>% dplyr::select(JPT_KOD_JE, JPT_NAZWA_)
kody<-sort(wojew_nazwy$JPT_KOD_JE)

wojew_df <- tidy(wojew, region = "JPT_KOD_JE")
wojew_df <- left_join(wojew_df, wojew_nazwy, by=c("id"="JPT_KOD_JE"))

#statystyki
wojewodztwo$Rok<-as.Date(as.character(wojewodztwo$Rok), format="%Y")
lista<-colnames(wojewodztwo[2:17])
nowy3<-data.frame(wojewodztwo=lista, zmiana=c(0))

for ( i in 1:16){
  a<-lista[i]
  woj<-wojewodztwo[i+1]
  woj2<-cbind(woj, zbior1)
  woj2_xts <-
    woj2 %>%
    dplyr::select(-Rok) %>%
    as.xts(order.by = woj2$Rok)
  woj2_causal <- CausalImpact(woj2_xts, 
                              pre.period = as.Date(c("2002-06-17", "2015-06-17")), 
                              post.period = as.Date(c("2016-06-17", "2019-06-17")))
  nowy3$zmiana[i]<-round(woj2_causal$summary[1,1]/woj2_causal$summary[1,2]*100-100)
}

nowy3$kod<-kody

#³aczenie zbiorów z danymi obszarowymi województw oraz wygenerowanymi statystykami
wojew_df<-left_join(wojew_df, nowy3, by=c("id"="kod"))

#wczytywanie danych dotycz¹cych œrodka województw
srodki <- read.csv("srodki.csv", sep=";")
srodki<-srodki %>%
  rename(long=D³ugoœæ.geograficzna,
         lat=Szerokoœæ.geograficzna)

#mapa
ggplot(wojew_df) +
  geom_polygon(aes(long, lat, group=group, fill=zmiana), color="gray") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  geom_point(data = srodki, aes(long, lat),show.legend = FALSE) +
  geom_text(data = srodki, aes(long, lat, label = Województwo), vjust = 1.7, size = 2.8) +
  coord_map() +
  theme_void()+
  ggtitle("Zmiana liczby urodzeñ wzglêdem województwa")
