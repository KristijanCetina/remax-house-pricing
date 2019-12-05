#' ---
#' title: "Cijene nekretnina"
#' author: "Kristijan Cetina"
#' date: "`r format(Sys.time(), '%e.%m.%Y.')`"
#' ---
#' 
cat("\f"); graphics.off(); rm(list = ls())
setwd("~/Documents/remax-house-pricing")
options(digits=7)
library("gvlma")
df = read.csv("houses_full.csv", sep = ",", header = TRUE, dec=",")
summary(df)
#unique( df$Vrsta.nekretnine.)
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

df$Površina_numeric <- as.double(gsub(",",".", df$Površina.))
df$Godina.izgradnje.[df$Godina.izgradnje. == ""] <- NA
df$Cijena. <- as.numeric(gsub("€.*","",df$Cijena.))
as.numeric(df$Cijena.)
df$Cijena.[df$Cijena. < 20.000] <- NA
df <- na.omit(df)
df$Površina. <- NULL
df$godinaIzgradnje <- as.numeric(gsub("god.", "", df$Godina.izgradnje.))
df$Godina.izgradnje. <- NULL
df <- na.omit(df)
#df$X <- NULL
#df$Lokacija. <- NULL
summary(df)


write.csv(df, file = "cleanData.csv")
pairs(df)

df_training <- df[1:280, ] #podajeli dataset u dva za trening i test
df_test <- df[281:nrow(df),]

model1 <- lm(Cijena.~Spavaće.sobe. +  + Površina_numeric , data = df_training)
summary(model1)
gvlma(model1)

model2 <- lm(Cijena.~., data = df_training)
summary(model2)
gvlma(model2)

model3 <- lm(Cijena.~Površina_numeric , data = df_training)
summary(model3)
gvlma(model3)
