#### EXPLORATORISCHE FAKTORENANALYSE ####
library(here)
library(readxl)
setwd(here("data/Sample_USA_EFA+CFA/EFA/MAP_PA"))
#setwd("F:/IPIP Paper/Sample_USA/EFA/MAP_PA")
# hier bitte Ihren eigenen Pfad eingeben

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
# VSS complexity 1: pro Item ist nur eine Ladung auf einem einzigen Faktor zugelassen, die Ladungskoeffizienten
# auf anderen Faktoren sind auf Null gesetzt
# VSS complexity 2: pro Item sind Ladungen auf zwei Faktoren zugelassen
# MAP empfiehlt ebenfalls eine Anzahl an Faktoren
# unterer Teil der Ausgabe: Werte für verschiedene Faktorl?sungen: kleinster MAP und größter VSS-Wert entscheidend

#### Paralleltestanalyse: Eigenwert, Scree-Test und Parallelanalyse ####
library(future)
plan(multiprocess)


fa <- future(fa.parallel.poly(agree_items, n.iter=100, fa="PC"))
resolved(fa)
value(fa)


fa.parallel(consc_items, n.iter=1000, fa="PC")
fa.parallel(extra_items, n.iter=1000, fa="PC")
fa.parallel(neuro_items, n.iter=1000, fa="PC")
fa.parallel(open_items, n.iter=1000, fa="PC")

pfa_agree <- fa(agree_items, nfactors=8, rotate="geominT", fm="pa")
pfa_agree
