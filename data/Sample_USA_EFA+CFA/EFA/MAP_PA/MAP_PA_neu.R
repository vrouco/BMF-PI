#### EXPLORATORISCHE FAKTORENANALYSE ####
library(here)
library(tidyverse)
library(readxl)
#setwd("F:/IPIP Paper/Sample_USA/EFA/MAP_PA")
# hier bitte Ihren eigenen Pfad eingeben

setwd(here("/data/Sample_USA_EFA+CFA/EFA/MAP_PA"))

agree<-read_excel("ARI Daten Agreeableness EFA.xls")
consc<-read_excel("conneu.xlsx")
extra_n<-read.csv("ARI Daten Extraversion EFA.csv", header=TRUE, sep=";")
neuro_n<-read.csv("ARI Daten Neuroticism EFA.csv", header=TRUE, sep=";")
open<-read_excel("ARI Daten Openness EFA.xls")
open_n<-read.csv("ARI Daten Openness EFA.csv", header=TRUE, sep=";")

agree_items<-agree[c(2:103)]

names(consc)
consc_items<-consc[c(2:114)]

names(extra_n)
extra_items<-extra_n[c(2:82)]

names(neuro_n)
neuro_items<-neuro_n[c(2:119)]

names(open_n)
open_items<-open_n[c(2:112)]

str(agree_items)

#### MAP Test ####
library(psych)
library(GPArotation)
vss(agree_items)
vss(consc_items)
vss(extra_items)
vss(neuro_items)
vss(open_items)
# VSS complexity 1: per item only one charge on a single factor is allowed, the charge coefficients
# on other factors are set to zero
# VSS complexity 2: per item charges are allowed on two factors
# MAP also recommends a number of factors
# lower part of output: Values for different factor solutions: smallest MAP and largest VSS value crucial

#### Paralleltestanalyse: Eigenwert, Scree-Test und Parallelanalyse ####
library(parallel)
options(mc.cores=3)
fa.parallel(agree_items, fa="PC")
fa.parallel(consc_items, n.iter=1000, fa="PC")
fa.parallel(extra_items, n.iter=1000, fa="PC")
fa.parallel(neuro_items, n.iter=1000, fa="PC")
fa.parallel(open_items, n.iter=1000, fa="PC")

pfa_agree <- fa(agree_items, nfactors=8, rotate="geominT", fm="pa")
pfa_agree
