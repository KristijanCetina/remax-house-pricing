#' ---
#' title: "Cijene nekretnina"
#' author: "Kristijan Cetina"
#' date: "`r format(Sys.time(), '%e.%m.%Y.')`"
#' ---
#' 
cat("\f"); graphics.off(); rm(list = ls())
setwd("~/Documents/remax-house-pricing")
df = read.csv("houses_full.csv", sep = ",", header = TRUE)
summary(df)
#pairs(df)
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

df$Površina_numeric <- as.numeric(df$Površina.)



#write.csv(df, file = "cleanData.csv")
