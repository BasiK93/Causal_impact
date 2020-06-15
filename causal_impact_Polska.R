#pakiety
library(dplyr)
library(tidyr)
library(ggplot2)
library(CausalImpact)
library(xts)

#wczytywanie zbioru danych
dane<-read.csv("dane_stat.csv", sep = ";", )

#generowanie pomocniczego zbioru danych
panstwa<-subset(dane, select = c(Rok, Polska, Niemcy, Czechy, S這wacja))

#
panstwa$Rok<-as.Date(as.character(panstwa$Rok), format="%Y")

ggplot(panstwa, aes(x=Rok))+
  geom_line(aes(y=Polska, color="Polska"), size=1)+
  geom_line(aes(y=Niemcy, color="Niemcy"), size=1)+
  geom_line(aes(y=Czechy, color="Czechy"), size=1)+
  geom_line(aes(y=S這wacja, color="S這wacja"), size=1)+
  ylab("")+
  scale_color_manual(name = "Pa雟two", values = c("Polska"="red", "Niemcy"="orange", "Czechy"="blue", "S這wacja"="green"))+
  ggtitle("Liczba urodzonych dzieci w latach 1970-2019")+
  theme_minimal()

#zbi鏎 do analiz
panstwa_xts <-
  panstwa %>%
  dplyr::select(-Rok) %>%
  as.xts(order.by = panstwa$Rok)

#CausalImpact
panstwa_causal <- CausalImpact(panstwa_xts, 
                                 pre.period = as.Date(c("1970-06-09", "2015-06-09")), 
                                 post.period = as.Date(c("2016-06-09", "2019-06-09")))

#wyniki
summary(panstwa_causal)
plot(panstwa_causal, "original")
summary(panstwa_causal, "report")
plot(panstwa_causal)

