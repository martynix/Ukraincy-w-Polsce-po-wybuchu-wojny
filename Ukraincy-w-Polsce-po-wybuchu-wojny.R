setwd("C:/Users/marty/Desktop/Ukraincy-w-Polsce-po-wybuchu-wojny1")
list.files()
library(readxl)

#STYCZEN
ruch_graniczny_st_kw<-read_xlsx("Ruch_graniczny_śr._transportu_drogowego_-_styczeń-kwiecień_2022.xlsx",2)
class(ruch_graniczny_st_kw)
ruch_graniczny_st_kw<-(ruch_graniczny_st_kw[20:29,])
ruch_graniczny_st_kw<-data.frame(ruch_graniczny_st_kw,row.names=1)
#colnames(ruch_graniczny_st_kw1)<-ruch_graniczny_st_kw1[1,]
ruch_graniczny_st_kw<-ruch_graniczny_st_kw[-1,]

ruch_styczen_motocykle<-ruch_graniczny_st_kw[1:7,12:15]
colnames(ruch_styczen_motocykle)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_styczen_autobusy<-ruch_graniczny_st_kw[1:7,17:20]
colnames(ruch_styczen_autobusy)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_styczen_osobowe<-ruch_graniczny_st_kw[1:7,22:25]
colnames(ruch_styczen_osobowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_styczen_ciezarowe<-ruch_graniczny_st_kw[1:7,27:30]
colnames(ruch_styczen_ciezarowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

#ruch_styczen<-data.frame(ruch_styczen_motocykle,ruch_styczen_autobusy,ruch_styczen_osobowe,ruch_styczen_ciezarowe)

#LUTY

ruch_graniczny_lt<-read_xlsx("Ruch_graniczny_śr._transportu_drogowego_-_styczeń-kwiecień_2022.xlsx",3)
ruch_graniczny_lt<-ruch_graniczny_lt[20:29,]
ruch_graniczny_lt<-data.frame(ruch_graniczny_lt,row.names=1)
ruch_graniczny_lt<-ruch_graniczny_lt[-1,]

ruch_luty_motocykle<-ruch_graniczny_lt[c(1:8),12:15]
colnames(ruch_luty_motocykle)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_luty_autobusy<-ruch_graniczny_lt[c(1:8),17:20]
colnames(ruch_luty_autobusy)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_luty_osobowe<-ruch_graniczny_lt[c(1:8),22:25]
colnames(ruch_luty_osobowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_luty_ciezarowe<-ruch_graniczny_lt[c(1:8),27:30]
colnames(ruch_luty_ciezarowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

#MARZEC

ruch_graniczny_marzec<-read_xlsx("Ruch_graniczny_śr._transportu_drogowego_-_styczeń-kwiecień_2022.xlsx",4)
ruch_graniczny_marzec<-ruch_graniczny_marzec[20:29,]
ruch_graniczny_marzec<-data.frame(ruch_graniczny_marzec,row.names=1)
ruch_graniczny_marzec<-ruch_graniczny_marzec[-c(1,9),]

ruch_marzec_motocykle<-ruch_graniczny_marzec[,12:15]
colnames(ruch_luty_motocykle)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_marzec_autobusy<-ruch_graniczny_marzec[,17:20]
colnames(ruch_marzec_autobusy)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_marzec_osobowe<-ruch_graniczny_marzec[,22:25]

colnames(ruch_marzec_osobowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_marzec_ciezarowe<-ruch_graniczny_marzec[,27:30]
colnames(ruch_marzec_ciezarowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

#KWIECIEN
ruch_graniczny_kwiecien<-read_xlsx("Ruch_graniczny_śr._transportu_drogowego_-_styczeń-kwiecień_2022.xlsx",5)
ruch_graniczny_kwiecien<-ruch_graniczny_kwiecien[20:29,]
ruch_graniczny_kwiecien<-data.frame(ruch_graniczny_kwiecien,row.names=1)
ruch_graniczny_kwiecien<-ruch_graniczny_kwiecien[-c(1,9),]

ruch_kwiecien_motocykle<-ruch_graniczny_kwiecien[,12:15]
colnames(ruch_kwiecien_motocykle)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_kwiecien_autobusy<-ruch_graniczny_kwiecien[,17:20]
colnames(ruch_kwiecien_autobusy)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_kwiecien_osobowe<-ruch_graniczny_kwiecien[,22:25]
colnames(ruch_kwiecien_osobowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

ruch_kwiecien_ciezarowe<-ruch_graniczny_kwiecien[,27:30]
colnames(ruch_kwiecien_ciezarowe)<-c("obce do RP","obce z RP","polskie do RP","polskie z RP")

#SUMA PRZYJAZDOW OBCYCH DO POLSKI Z PODZIALEM NA MIESIACE

styczen_obce_do_RP<-cbind(ruch_styczen_motocykle[,1],ruch_styczen_autobusy[,1],ruch_styczen_osobowe[,1],ruch_styczen_ciezarowe[,1])
styczen_obce_do_RP<-data.matrix(as.numeric(styczen_obce_do_RP))
suma1<-colSums(styczen_obce_do_RP,na.rm=TRUE)

luty_obce_do_RP<-cbind(ruch_luty_motocykle[,1],ruch_luty_autobusy[,1],ruch_luty_osobowe[,1],ruch_luty_ciezarowe[,1])

luty_obce_do_RP<-data.matrix((as.numeric(luty_obce_do_RP)))
suma2<-colSums(luty_obce_do_RP,na.rm=T)

marzec_obce_do_RP<-cbind(ruch_marzec_motocykle[,1],ruch_marzec_autobusy[,1],ruch_marzec_osobowe[,1],ruch_marzec_ciezarowe[,1])
marzec_obce_do_RP<-data.matrix(as.numeric(marzec_obce_do_RP))
suma3<-colSums(marzec_obce_do_RP,na.rm=T)

kwiecien_obce_do_RP<-cbind(ruch_kwiecien_motocykle[,1],ruch_kwiecien_autobusy[,1],ruch_kwiecien_osobowe[,1],ruch_kwiecien_ciezarowe[,1])

kwiecien_obce_do_RP<-data.matrix(as.numeric(kwiecien_obce_do_RP))
suma4<-colSums(kwiecien_obce_do_RP,na.rm=T)

obce_do_RP<-data.frame(liczba=c(suma1,suma2,suma3,suma4))

row.names(obce_do_RP)<-c("styczen","luty","marzec","kwiecien")

barplot(obce_do_RP$liczba,names.arg=rownames(obce_do_RP),las=2,main="Liczba zagranicznych pojazdow wjezdzajacych do RP",
        #ylab="liczba",
        cex.names=0.8,
        col=heat.colors(9),
        font.main=3,
        font.lab=4)


#SUMA WYJAZDOW OBCYCH Z POLSKI Z PODZIALEM NA MIESIACE

styczen_obce_z_RP<-cbind(ruch_styczen_motocykle[,2],ruch_styczen_autobusy[,2],ruch_styczen_osobowe[,2],ruch_styczen_ciezarowe[,2])
styczen_obce_z_RP<-data.matrix(as.numeric(styczen_obce_z_RP))
suma11<-colSums(styczen_obce_z_RP,na.rm=TRUE)

luty_obce_z_RP<-cbind(ruch_luty_motocykle[,2],ruch_luty_autobusy[,2],ruch_luty_osobowe[,2],ruch_luty_ciezarowe[,2])
luty_obce_z_RP<-data.matrix((as.numeric(luty_obce_z_RP)))
suma22<-colSums(luty_obce_z_RP,na.rm=T)

marzec_obce_z_RP<-cbind(ruch_marzec_motocykle[,2],ruch_marzec_autobusy[,2],ruch_marzec_osobowe[,2],ruch_marzec_ciezarowe[,2])
marzec_obce_z_RP<-data.matrix(as.numeric(marzec_obce_z_RP))
suma33<-colSums(marzec_obce_z_RP,na.rm=T)

kwiecien_obce_z_RP<-cbind(ruch_kwiecien_motocykle[,2],ruch_kwiecien_autobusy[,2],ruch_kwiecien_osobowe[,2],ruch_kwiecien_ciezarowe[,2])
kwiecien_obce_z_RP<-data.matrix(as.numeric(kwiecien_obce_z_RP))
suma44<-colSums(kwiecien_obce_z_RP,na.rm=T)

obce_z_RP<-data.frame(liczba=c(suma11,suma22,suma33,suma44))
row.names(obce_z_RP)<-c("styczen","luty","marzec","kwiecien")



barplot(obce_z_RP$liczba,names.arg=rownames(obce_z_RP),las=2,main="Liczba zagranicznych pojazdow wyjezdzajacych z RP",
        #ylab="liczba",
        cex.names=0.8,
        col=heat.colors(9),
        font.main=3,
        font.lab=4)


#RODZAJE OBCYCH POJAZDOW WJEZDZAJACYCH
motocykle<-cbind(ruch_styczen_motocykle[,1],ruch_luty_motocykle[,1],ruch_marzec_motocykle[,1],ruch_kwiecien_motocykle[,1])
motocykle<-data.matrix(as.numeric(motocykle))
motosum<-colSums(motocykle,na.rm=T)

autobusy<-cbind(ruch_styczen_autobusy[,1],ruch_luty_autobusy[,1],ruch_marzec_autobusy[,1],ruch_kwiecien_autobusy[,1])
autobusy<-data.matrix(as.numeric(autobusy))
busysum<-colSums(autobusy,na.rm=T)

osobowe<-cbind(ruch_styczen_osobowe[,1],ruch_luty_osobowe[,1],ruch_marzec_osobowe[,1],ruch_kwiecien_osobowe[,1])
osobowe<-data.matrix(as.numeric(osobowe))
osobowesum<-colSums(osobowe,na.rm=T)

ciezarowe<-cbind(ruch_styczen_ciezarowe[,1],ruch_luty_ciezarowe[,1],ruch_marzec_ciezarowe[,1],ruch_kwiecien_ciezarowe[,1])
ciezarowe<-data.matrix(as.numeric(ciezarowe))
ciezarowesum<-colSums(ciezarowe,na.rm=T)

rodzaje_pojazdow_do_RP<-data.frame(liczba=c(motosum,busysum,osobowesum,ciezarowesum))
row.names(rodzaje_pojazdow_do_RP)<-c("motocykle","autobusy","osobowe","ciezarowe")

pojazdy<-data.matrix(rodzaje_pojazdow_do_RP)
suma_pojazdy<-rbind(pojazdy,colSums(pojazdy))
suma_pojazdy<-data.frame(suma_pojazdy)

library(RColorBrewer)
myPalette <- brewer.pal(4, "Set3")
pie(rodzaje_pojazdow_do_RP$liczba,labels = c("motocykle - 0,02%","autobusy - 3.33%","osobowe - 80.62%","ciezarowe - 16.02%"),main="podzial zagranicznych pojazdow wjezdzajacych do RP", border="white", col=myPalette,radius=1)

#RODZAJE OBCYCH POJAZDOW WYJEZDZAJACYCH
motocykleWY<-cbind(ruch_styczen_motocykle[,2],ruch_luty_motocykle[,2],ruch_marzec_motocykle[,2],ruch_kwiecien_motocykle[,2])
motocykleWY<-data.matrix(as.numeric(motocykleWY))
motosumWY<-colSums(motocykleWY,na.rm=T)

autobusyWY<-cbind(ruch_styczen_autobusy[,2],ruch_luty_autobusy[,2],ruch_marzec_autobusy[,2],ruch_kwiecien_autobusy[,2])
autobusyWY<-data.matrix(as.numeric(autobusyWY))
busysumWY<-colSums(autobusyWY,na.rm=T)

osoboweWY<-cbind(ruch_styczen_osobowe[,2],ruch_luty_osobowe[,2],ruch_marzec_osobowe[,2],ruch_kwiecien_osobowe[,2])
osoboweWY<-data.matrix(as.numeric(osoboweWY))
osobowesumWY<-colSums(osoboweWY,na.rm=T)

ciezaroweWY<-cbind(ruch_styczen_ciezarowe[,2],ruch_luty_ciezarowe[,2],ruch_marzec_ciezarowe[,2],ruch_kwiecien_ciezarowe[,2])
ciezaroweWY<-data.matrix(as.numeric(ciezaroweWY))
ciezarowesumWY<-colSums(ciezaroweWY,na.rm=T)

rodzaje_pojazdow_z_RP<-data.frame(liczba=c(motosumWY,busysumWY,osobowesumWY,ciezarowesumWY))
row.names(rodzaje_pojazdow_z_RP)<-c("motocykle","autobusy","osobowe","ciezarowe")

pie(rodzaje_pojazdow_z_RP$liczba,labels = c("motocykle - 0,02%","autobusy - 4.60%","osobowe - 72.06%","ciezarowe - 23.33%"),main="podzial zagranicznych pojazdow wyjezdzajacych z RP", border="white", col=myPalette,radius=1)
pojazdyz<-data.matrix(rodzaje_pojazdow_z_RP)
suma_pojazdyz<-rbind(pojazdyz,colSums(pojazdyz))
suma_pojazdyz<-data.frame(suma_pojazdyz)

#UDZIAL PROCENTOWY
perc_m<-suma_pojazdy[1,1]/suma_pojazdy[5,1]*100
perc_a<-suma_pojazdy[2,1]/suma_pojazdy[5,1]*100
perc_os<-suma_pojazdy[3,1]/suma_pojazdy[5,1]*100
perc_c<-suma_pojazdy[4,1]/suma_pojazdy[5,1]*100

pojazdy1<-data.frame("udzial procentowy"=c(round(perc_m,2),round(perc_a,2),round(perc_os,2),round(perc_c,2)))

pojazdy2<-data.frame(pojazdy,pojazdy1)

perc_mz<-suma_pojazdyz[1,1]/suma_pojazdyz[5,1]*100
perc_az<-suma_pojazdyz[2,1]/suma_pojazdyz[5,1]*100
perc_osz<-suma_pojazdyz[3,1]/suma_pojazdyz[5,1]*100
perc_cz<-suma_pojazdyz[4,1]/suma_pojazdyz[5,1]*100

pojazdy1z<-data.frame("udzial procentowy"=c(round(perc_mz,2),round(perc_az,2),round(perc_osz,2),round(perc_cz,2)))
pojazdy2z<-data.frame(pojazdyz,pojazdy1z)

list.files()

#WNIOSKI O NADANIE STATUSU UKRAINCA
wnioski<-read.table("WNIOSKI_UKR_20220831.csv",header=T,sep=",")
str(wnioski)
wnioski_marzec<-wnioski[1:18,4:9]
wnioski_kwiecien<-wnioski[19:45,4:9]
wnioski_maj<-wnioski[46:73,4:9]
wnioski_czerwiec<-wnioski[74:100,4:9]
wnioski_lipiec<-wnioski[101:126,4:9]
wnioski_sierpien<-wnioski[127:151,4:9]

suma_wnioskow<-rbind(colSums(wnioski_marzec),colSums(wnioski_kwiecien),colSums(wnioski_maj),colSums(wnioski_czerwiec),colSums(wnioski_lipiec),colSums(wnioski_sierpien) )
#rownames(suma_wnioskow)<-c("marzec","kwiecień","maj","czerwiec","lipiec","sierpien")
suma_wnioskow<-cbind(c("marzec","kwiecien","maj","czerwiec","lipiec","sierpien"),suma_wnioskow)
colnames(suma_wnioskow)<-c("x","kobiety<18","kobiety18-65","kobiety65+","mezczyzni<18","mezczyzni18-65","mezczyzni65+")
sumaw<-data.frame(suma_wnioskow)

sumaw$kobiety.18<-as.integer(sumaw$kobiety.18)
sumaw$kobiety18.65<-as.integer(sumaw$kobiety18.65)
sumaw$kobiety65.<-as.integer(sumaw$kobiety65.)
sumaw$mezczyzni.18<-as.integer(sumaw$mezczyzni.18)
sumaw$mezczyzni18.65<-as.integer(sumaw$mezczyzni18.65)
sumaw$mezczyzni65.<-as.integer(sumaw$mezczyzni65.)

str(sumaw)

library(tidyr)
library(dplyr)
library(knitr)

data_long <- gather(sumaw, grupa, wartosc, kobiety.18:mezczyzni65.) %>%
  arrange(factor(x, levels = c("marzec","kwiecien","maj","czerwiec","lipiec","sierpien"))) %>% 
  mutate(x=factor(x, levels=unique(x)))

kable(head(data_long, 36))

library(ggplot2)
library(hrbrthemes)
library(viridis)

ggplot(data_long, aes(fill=grupa, y=wartosc, x=x)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_viridis(discrete=T, name="") +
  theme_ipsum() +
  ylab("liczba wnioskow") + 
  xlab("miesiac")

