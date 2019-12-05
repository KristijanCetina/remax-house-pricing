#' ---
#' title: "Cijene nekretnina"
#' author: "Kristijan Cetina"
#' date: "`r format(Sys.time(), '%e.%m.%Y.')`"
#' ---
#' 
cat("\f"); graphics.off(); rm(list = ls())
setwd("~/Documents/remax-house-pricing")
df = read.csv("houses_full.csv", sep = ",", header = TRUE)#, dec = ",")
summary(df)
unique( df$Vrsta.nekretnine.)
df <- df[df$Vrsta.nekretnine. =="Stan/Apartman",]

