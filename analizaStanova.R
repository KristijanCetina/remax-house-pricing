#' ---
#' title: "Cijene nekretnina"
#' author: "Kristijan Cetina"
#' date: "`r format(Sys.time(), '%e.%m.%Y.')`"
#' ---
#' 

cat("\f"); graphics.off(); rm(list = ls())
setwd("~/Documents/remax-house-pricing")
options(digits=3)
options(scipen=5)
library("gvlma")
library("corrgram")

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

df = read.csv("houses_full.csv", sep = ",", header = TRUE, dec=",")
summary(df)
str(df)
unique( df$Vrsta.nekretnine.)


df <- df[df$Vrsta.nekretnine. =="Stan/Apartman",]
df$Vrt. <- NULL
df$Struja.zakupljena.snaga. <- NULL
df$Pripojene.prostorije. <- NULL
df$Vanjska.stolarija. <- NULL
df$Unutarnja.stolarija. <- NULL
df$Kupaonica..kuhinja. <- NULL
df$Instalacije.vode. <- NULL
df$Instalacije.struje. <- NULL
df$Pod..parket.laminat.pločice. <- NULL
df$Fasada. <- NULL
df$Krov. <- NULL
df$Površina.negrađevinskog.dijela. <- NULL
df$Površina.okućnice. <- NULL
df$Površina.građevinskog.dijela. <- NULL
df$Tlocrtna.površina.nekretnine. <- NULL
df$Vrsta.kuće.vikendice. <- NULL
df$Transakcija. <- NULL
df$Namjena.terena. <- NULL
df$Stil.gradnje. <- NULL
df$Mogućnost.zamjene. <- NULL
df$Dimnjak. <- NULL
df$Okućnica. <- NULL
df$Toaleti. <- NULL
df$Roštilj. <- NULL
df$Bazen. <- NULL
df$Nekretnina.se.prodaje.iznajmljuje. <- NULL
df$Nekretnina..stanje.. <- NULL
df$Natkriveni.parking. <- NULL
df$Razred.energetske.učinkovitosti. <- NULL
df$dodatne_oznake <- NULL
df$opis <- NULL
df$Pristup.invalidima. <- NULL
df$naziv <- NULL
df$Pogled.na. <- NULL
df$Pogled.na.more. <- NULL
df$Povlašteni.parking. <- NULL
df$Visina.plafona. <- NULL
df$Namjena.poslovnog.prostora. <- NULL
df$Lift. <- NULL
df$P.P..sa.izlogom. <- NULL
df$Pristup.kamionima.šleperima. <- NULL
df$Ključ.u.agenciji. <- NULL
df$Struja. <- NULL
df$Ukupno.katova. <- NULL
df$Parking. <- NULL
df$Parking..broj.. <- NULL
df$Garaža. <- NULL
df$Udaljenost.od.mora. <- NULL
df$Površina.balkona.lođe.terase. <- NULL
df$Vrsta.nekretnine. <- NULL

df$Površina. <- as.double(gsub(",",".", df$Površina.))
df$Godina.izgradnje.[df$Godina.izgradnje. == ""] <- NA
df$Cijena. <- readr::parse_number(as.character(df$Cijena.) ,locale = readr::locale(decimal_mark = ","))
df$Godina.izgradnje. <- readr::parse_number(as.character(df$Godina.izgradnje.) ,locale = readr::locale(decimal_mark = ","))
df <- df[!is.na(df$Cijena.),]

boxplot(df$Godina.izgradnje.)
boxplot(df$Ukupan.broj.soba.)
h0 <- hist(df$Ukupan.broj.soba., breaks = 6,
     labels = T,
     main = "Distribucija broja soba",
     xlab = "Broj soba",
     ylab = "Broj objekata"
)

df <- df[!df$Ukupan.broj.soba. > 6,]
df <- df[!df$Cijena. < 20000,]
df <- na.omit(df)
df <- df[!df$Godina.izgradnje. < 1900,]
boxplot(df$Ukupan.broj.soba.)
boxplot(df$Godina.izgradnje.)
h1 <- hist(df$Godina.izgradnje.,
          labels = T,
          main = "Distribucija broja soba",
          xlab = "Broj soba",
          ylab = "Broj objekata"
)

summary(df)

write.csv(df, file = "cleanData.csv")
pairs(df)
set.seed(666999)
idx <- sample(nrow(df), 0.7*nrow(df)) #70-30
trening <- df[idx,]
test <- df [-idx,]

corrgram(trening, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
cor.test(trening$Cijena., trening$Površina.)

model1 <- lm(Cijena.~Spavaće.sobe. +  + Površina. , data = trening)
summary(model1)
gvlma(model1)

model2 <- lm(Cijena.~ Lokacija. + Površina., data = trening)
summary(model2)
gvlma(model2)

model3 <- lm(Cijena.~Površina. , data = trening)
summary(model3)
gvlma(model3)

pred1 <- predict(model1, newdata = test)
#pred2 <- predict(model2, newdata = test)
pred3 <- predict(model3, newdata = test)

par(mfrow = c(2,2)) # 2x2 plot
plot(model1)
plot(model3)
par(mfrow=c(1,1))

t1 <- test$Cijena. - pred1
barplot(t1,
     main = "Predvidene cijene - model 1",
     xlab = "index nekretnine",
     ylab = "Razlika procijene")

h_t1 <- hist(t1,breaks = 10,
           labels = T,
           main = "Distribucije procijene cijena model_1",
           xlab = "Razlika cijene",
           ylab = "Broj objekata"
)
xfit<-seq(min(t1),max(t1),length=40)
yfit<-dnorm(xfit,mean=mean(t1),sd=sd(t1))
yfit <- yfit*diff(h_t1$mids[1:2])*length(t1)
lines(xfit, yfit, col="blue", lwd=2)

t3 <- test$Cijena. - pred3
barplot(t3,
        main = "Predvidene cijene - model 3",
        xlab = "index nekretnine",
        ylab = "Razlika procijene")

h_t3 <- hist(t3,breaks = 10,
             labels = T,
             main = "Distribucije procijene cijena- model 3
             ",
             xlab = "Razlika cijene",
             ylab = "Broj objekata"
)
xfit<-seq(min(t3),max(t3),length=40)
yfit<-dnorm(xfit,mean=mean(t3),sd=sd(t3))
yfit <- yfit*diff(h_t3$mids[1:2])*length(t3)
lines(xfit, yfit, col="blue", lwd=2)


#' \title{Zaključak i diskusija}
#' Korišteni model ne može dobro predvidjeti cijene nekretnina iz zadanog dataseta.
#' Trebalo bi rješiti poznati problem utjecaja lokacije na cijenu nekretnine.
#' Nadalje, vidi se da model ipak podjednako precijeni kao i podcijeni nekretnine iz testnog skupa podataka s blagim repom prema podcijenjenim
#' 
#'

str(df)

levels(df$Lokacija.)
df$lokacija.num <- df$Lokacija.
levels(df$lokacija.num) <- c(1:348)
df$lokacija.num <- as.numeric.factor(df$lokacija.num)
uniqueLocations <- unique(df$lokacija.num)

#ponovno podjeliti podatke na trening i test - pregaziti
set.seed(666999)
idx <- sample(nrow(df), 0.7*nrow(df)) #70-30
trening <- df[idx,]
test <- df [-idx,]

model4 <- lm(Cijena.~lokacija.num  + Površina. , data = trening)
summary(model4)
gvlma(model4) # i dalje nije neka sreca. Cini mi se da ima premalo primjeraka iz svake kategorije.
pred4 <- predict(model4, newdata = test)

t4 <- test$Cijena. - pred4
barplot(t1,
        main = "Predvidene cijene - model 4",
        xlab = "index nekretnine",
        ylab = "Razlika procijene")

h_t4 <- hist(t1,breaks = 10,
             labels = T,
             main = "Distribucije procijene cijena model_4",
             xlab = "Razlika cijene",
             ylab = "Broj objekata"
)
xfit<-seq(min(t4),max(t4),length=40)
yfit<-dnorm(xfit,mean=mean(t4),sd=sd(t4))
yfit <- yfit*diff(h_t1$mids[1:2])*length(t1)
lines(xfit, yfit, col="blue", lwd=2)
