library(here)
library(tidyverse)
library(readxl)

# Sorry - Datasets not available

# setwd(here("data/Sample_USA_EFA+CFA/EFA/MAP_PA"))
# 
# agree<-read_excel("ARI Daten Agreeableness EFA.xls")
# consc<-read_excel("conneu.xlsx")
# extra_n<-read.csv("ARI Daten Extraversion EFA.csv", header=TRUE, sep=";")
# neuro_n<-read.csv("ARI Daten Neuroticism EFA.csv", header=TRUE, sep=";")
# open_n<-read.csv("ARI Daten Openness EFA.csv", header=TRUE, sep=";")

##############see initial items
a<-agree[c(1:103)]
c<-consc[c(1:114)]
c<-c[-which(is.na(match(c$respid, a$respid))),] #somehow there is two individuals in c which are not there in any other domain
e<-extra_n[c(1:82)]
n<-neuro_n[c(1:119)]
o<-open_n[c(1:112)]

items<-merge(a, c, by="respid")
items<-merge(items, e, by="respid")
items<-merge(items, n, by="respid")
items<-merge(items, o, by="respid")

dim(items)[2]-1   #number of initial items

###############MAP

library(psych)
library(GPArotation)
vss(a, 20)#MAP 9
vss(c,20)#12
vss(e,20)#13
vss(n,20)#9
vss(o,20)#10

###########PA

fa.parallel(a)
fa.parallel(c)
fa.parallel(e)
fa.parallel(n)
fa.parallel(o)
