# R harkkatyö
# Tehtävä 1
asty<-read.csv2("asiakastyytyvaisyys.csv")
#a
yhteenveto <- function(x) {
  x.cut <- cut(x, c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5))
  freimi<- as.data.frame(table(x.cut))
  puuttuvat<-round(sum((is.na(x))))
  yhteensa<-round(length(x))-puuttuvat
  keskiarvo<-(mean(x, na.rm=T))
  varianssi<-(sd(x, na.rm=T))^2
  lista<-list(freimi, c(yhteensa, puuttuvat, keskiarvo, varianssi))
  return(lista)
}
#b
summamuuttuja <- function(nimet, data, max_puuttuvat) {
  data_kyssarit<-subset(data, select=nimet)
  kaikkiNA<-(is.na(data_kyssarit))
  puuttuvat<-apply(kaikkiNA, 1, sum)
  keskiarvot<-apply(data_kyssarit, 1, mean, na.rm=T)
  keskiarvot[puuttuvat>max_puuttuvat]<-NA
  keskiarvot<-apply(data_kyssarit, 1, mean, na.rm=T)
  return(keskiarvot)
}
#c
asty$tyyt_kulj<-summamuuttuja(c("K1A1", "K1A2", "K1A3"), data = asty, max_puuttuvat = 1)
hist(asty$tyyt_kulj, col="red",main = "Asiakkaiden tyytyväisyys kuljettajiin",xlab = "arvosana",ylab="frekvenssi")
ka<-mean(asty$tyyt_kulj, na.rm=T)
hist(asty$tyyt_kulj, col="green", breaks=12, xlab="arvosana", ylab="frekvenssi", main="Tyytyväisyys kuljettajiin")
abline(v=ka, col="blue", lwd=5)
# sum(is.na(asty$tyyt_kulj)) huolestuttaa
yhteenveto(asty$tyyt_kulj)
#d
tyyt_hsl<-summamuuttuja(c("K1A4", "K1A5", "K1A6", "K2A2", "K2A3", "K2A4", "K2A6"), data = asty, max_puuttuvat = 5)
hist(tyyt_hsl, col="turquoise", xlab="arvosana", ylab="frekvenssi", main="Tyytyväisyys HSL:n palveluihin")
yhteenveto(tyyt_hsl)
# histogrammi on painottunut selkeästi oikealle eli arvoihin välillä 4-5 eli asiakkaat ovat hyvin tyytyväsiä.

#Tehtävä 2
#a
asty_osa<-subset(asty, (T18==00200 | T18==02100 | T18==02230), na.rm=T)
asty_osa$T18 = factor(asty_osa$T18, labels=c("Lauttasaari", "Tapiola", "Matinkylä"))
#b
espoolaiset_X_korvaavat<-table(asty_osa$T18, asty_osa$K3A23)
round(prop.table(espoolaiset_X_korvaavat,1), 2)
espoo_korv1<-prop.table(espoolaiset_X_korvaavat,1)
#c
espoo_korv<-matrix(espoolaiset_X_korvaavat, ncol=5, byrow=F)
chisq.test(espoo_korv,corr=F)
# H0: "postiosoitteilla ei ole yhteyttä korvaavien linjojen arvosteluun" hylätään merkitsevyystasolla 
# alpha=0,01, koska testituloksesta saamamme p-arvo on 2,2E-16. eri asuinalueiden asukkaat vaikuttaisivat siis 
# olevan eri tavoin tyytyväisiä korvaavaan liikenteeseen.
#d
# huomautuksena: en ihan ymmärrä, miksi tässä käytetään t-testiä, koska ei varmaankaan voida olettaa vastausten olevan
#Lauttasaaressa normaalijakautuneita
lauttasaarelaiset<-asty[asty$T18==00200, ]
t.test(x=lauttasaarelaiset$K3A23, mu=3, conf.level = 0.95, alternative = "less", na.rm=T)
# Nähdään, että p-arvo on 0,0116, joten merkitsevyystasolla alpha=0,05 yksisuuntaisella t-testillä voidaan
# hylätä nollahypoteesi, jossa lauttasaaelaisten mieliteiden keskiarvo on suurempi tai yhtä suuri kuin 3.
# voidaan siis testin perusteella sanoa, että HSL ei ole onnistunut tavoitteessaan.

#Tehtävä 3.
#a
#T7 = syntymävuosi
vuosi<-asty$T7
vuosi<-as.numeric(vuosi)
paivamaara<-asty$PAIVAMAARA
paivamaara<-as.Date(paivamaara)
paivamaara<-format(paivamaara, "%Y")
paivamaara<-as.numeric(paivamaara)
asty$ika<-paivamaara-vuosi
#b
KeskiarvoJaVali<-function(vektori,luottamusvali) {
  ka<-mean(vektori, na.rm = T)
  s<-sd(vektori, na.rm=T)
  n<-length(vektori)
  pee<-(1-luottamusvali)
  ylar<-ka+qt(p=pee/2, df=(n-1), lower.tail = T)*(s/sqrt(n))
  alar<-ka-qt(p=pee/2, df=(n-1), lower.tail = T)*(s/sqrt(n))
  return(c(ylar,alar,ka))
}
#c
asty$ikaluokka<-cut(asty$ika, breaks=c(0,17,29,39,65,75,140))
levels(asty$ikaluokka)<-c("0-17","18-29","30-39","40-65","66-75","75-140")
table(asty$ikaluokka)
asty_metro<-(asty[asty$LIIKENNEMUOTO==3,])
KeskiarvoJaVali(asty_metro$K1A6, 0.95)
#ika_ja_mukavuus<-tapply(asty_metro$K1A6, asty_metro$ikaluokka, KeskiarvoJaVali, luottamusvali=0.95)
#ika_ja_mukavuus[[3]][2]
uusikokeilu<-aggregate(asty_metro$K1A6, list(asty_metro$ikaluokka), KeskiarvoJaVali, luottamusvali=0.95)

#d
eka<-c(uusikokeilu[1,2])
toka<-c(uusikokeilu[2,2])
kolmas<-c(uusikokeilu[3,2])
neljas<-c(uusikokeilu[4,2])
viides <-c(uusikokeilu[5,2])
kuudes<-c(uusikokeilu[6,2])
aks<-tapply(asty_metro$K1A6, asty_metro$ikaluokka, mean, na.rm=T)
plot(x=aks, xlim=c(0,6), ylim=c(1,5), xlab="ikaryhma", ylab="mielipide matk.muk", xaxt="n",pch=20, col="green")
axis(1, at=1:6, labels=levels(asty$ikaluokka))
segments(x0=1, y0=eka[1], x1=1, y1=eka[2])
segments(x0=2, y0=toka[1], x1=2, y1=toka[2])
segments(x0=3, y0=kolmas[1], x1=3, y1=kolmas[2])
segments(x0=4, y0=neljas[1], x1=4, y1=neljas[2])
segments(x0=5, y0=viides[1], x1=5, y1=viides[2])
segments(x0=6, y0=kuudes[1], x1=6, y1=kuudes[2])
# Nähdään, että luottamusvälit ovat pisimpiä pienimmässä ja suurimmassa ikäluokassa.
# Syy tähän on se, että näissä ikäluokissa on pienimmät otoskoot

#Tehtävä 4.
#a
yk <- read.csv2("YK.csv")
logBKT<-log(yk$BKT)
fit<-lm(yk$vaestonkasvu ~ logBKT)
summary(fit)
# kertoimet ovat hyvin "merkitseviä"
#todella tasaisesti jakautuneet residuaalit
# R^2 selitysaste on 0,26 eli selittää n. 1/4 vaihtelusta
#b
varit=terrain.colors(6)
varit_alue<-varit[yk$alue]
suora<-logBKT*(-0.3836)+4.528
plot(yk$vaestonkasvu~ logBKT, col=varit_alue, pch=20, ylab="vaestonkasvu")
lines(logBKT, suora, col="navy")
text(logBKT[yk$maa=="Georgia" | yk$maa=="Qatar"], yk$vaestonkasvu[yk$maa=="Georgia" | yk$maa=="Qatar"],
     labels=yk$maa[yk$maa=="Qatar" | yk$maa=="Georgia"], cex= 0.7, pos=3)
#c
fit2<-lm(yk$vaestonkasvu ~ logBKT + yk$lukutaito_naiset)
summary(fit2)
confint(fit2, level=0.9)
#selitysaste (R^2 on nyt noussu 44,11% eli jos otetaan bkt ja naisten lukutaito, niin selitysaste on jo melkein puolet parempi
# kuin jos katsotaan pelkkää bkt:ta. residuaalit jakautuu edelleen erittäin tasan. 
# Luulen että selitysaste paranee, koska myös naisten lukutaito on hyvä selittäjä lapsiluvulle.
# Erityisesti matala naisten lukutaito yhdistyy lähes poikkeuksetta kovaan väestönkasvuun.

#Tehtävä 5.
#a
kolikonheitto_nelja <- function(N) {
  theta <- runif(n = N, min = 0, max = 1)
  binomi<-rbinom(n=N, size=6, prob=theta)
  return(theta[binomi==4])
}
koe<-kolikonheitto_nelja(100000)
hist(koe,col="orange", breaks=100, xlab="theta", ylab="frekvenssi")
summary(koe)
koe_tunnuslukuja<-c(mean(koe), median(koe), sd(koe))
#nähdään että  paras arvio thetalle voisi olla: theta ~ 0,63. Sillä arvolla on suuri frekvenssi saaduille neljälle kruunalle.
#b
kolikonheitto <- function(N) {
  kolikot<-c(0.3,0.5,0.6,0.8)
  heitot<-sample(kolikot, N, prob=c(0.25,0.25,0.25,0.25), replace=TRUE)
  binomi<-rbinom(n=N, size=6, prob=heitot)
  halutut_tapaukset<-heitot[binomi==4]
  eka<-length(halutut_tapaukset[halutut_tapaukset==0.3])/length(halutut_tapaukset)
  toka<-length(halutut_tapaukset[halutut_tapaukset==0.5])/length(halutut_tapaukset)
  kolmas<-length(halutut_tapaukset[halutut_tapaukset==0.6])/length(halutut_tapaukset)
  neljas<-length(halutut_tapaukset[halutut_tapaukset==0.8])/length(halutut_tapaukset)
  return(c(eka, toka, kolmas, neljas))
}
plot(kolikonheitto(100000), xaxt = "n", type="h", col="violet", lwd=5, xlab="kolikko", ylab="kolikon osuus")
axis(1, at = 1:4)
#pekka heittelee siis todnäköisimmin kolikkoa 3