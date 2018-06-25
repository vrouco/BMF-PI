#### EXPLORATORISCHE FAKTORENANALYSE ####

setwd("F:/IPIP Paper/Sample_USA/EFA/MAP_PA")
# hier bitte Ihren eigenen Pfad eingeben

library(xlsx)
agree<-read.xlsx("ARI Daten Agreeableness EFA.xls", as.data.frame=TRUE, sheetIndex=1)
consc<-read.xlsx("conneu.xlsx", as.data.frame=TRUE, sheetIndex=1)
extra_n<-read.csv("ARI Daten Extraversion EFA.csv", header=TRUE, sep=";")
neuro_n<-read.csv("ARI Daten Neuroticism EFA.csv", header=TRUE, sep=";")
open<-read.xlsx("ARI Daten Openness EFA.xls", as.data.frame=TRUE, sheetIndex=1, stringAsFactors=F)
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
# unterer Teil der Ausgabe: Werte für verschiedene Faktorl�sungen: kleinster MAP und größter VSS-Wert entscheidend

#### Paralleltestanalyse: Eigenwert, Scree-Test und Parallelanalyse ####
fa.parallel(agree_items, n.iter=1000, fa="PC")
fa.parallel(extra_items, n.iter=1000, fa="PC")
fa.parallel(consc_items, n.iter=1000, fa="PC")
fa.parallel(neuro_items, n.iter=1000, fa="PC")
fa.parallel(open_items, n.iter=1000, fa="PC")

#### EFA ####
pfa_7 <- fa(items, nfactors=7, rotate="promax", fm="pa") # hier sollen 7 Faktoren extrahiert werden
pfa_8 <- fa(items, nfactors=8, rotate="promax", fm="pa") # hier sollen 8 Faktoren extrahiert werden

print(pfa_7, digits=2, cutoff=.3, sort=TRUE)
print(pfa_8, digits=2, cutoff=.3, sort=TRUE)
# es werden nur Faktorladungen >.30 angezeigt
# so sortiert, dass die Items, die auf demselben Faktor laden, direkt untereinander stehen
# SS-loading: quadrierte und aufsummierte Ladungen aller Faktoren
# Proportion: Varianzaufklärung jedes einzelnen Faktors
# cumulative Var_ rechts unten: zusammen erklären die Faktoren x% der Varianz
# bei promax rotation inter-Faktor-Korrelationen
# Fit Statistiken
# "objetive function: Wert der ML-Funktion; 
# The number of observation is .. with chi square .. with prob
# Fit based upon off diagonal values =.98": gibt an, wie gut die beobachteten Korrelationen zwischen den
# Variablen durch die Faktoren erklärt werden können; (1-QS(Residuen))/QS(beobachtet); liegt
# zwischen 0 und 1, je näher an 1, desto besser erklären Faktoren die Zusammenhänge
# "Measures of factor score adequancy": Güte der Faktorwerte
# "Correlation of scores with factors": Korrelation zwischen Faktorwerten und Item-Rohwerten; sollten >.71 sein
# "Multiple R square": Quadrat der ersten Zeile; Anteil der Varianz in Faktorwerten, der durch Item-Rohwerte
# erklärt werden 
# "minimum correlation": minimale Korrelation zwischen alternativen Faktorwerten; sollten positiv korreliert sein

print(pfa_7, cutoff=.30, digits=2, sort=TRUE)
print(pfa_8$loadings, cutoff=.30, digits=2, sort=TRUE) # macht den output und die Faktorladungen etwas übersichtlicher, da die Ladungen nach ihrer Höhe pro Faktor sortiert werden
print(pfa_7$loadings, cutoff=.30, digits=2, sort=TRUE)
#### Grafik der EFA ####
fa.diagram(pfa_7, digits=2, cut=.30, simple=FALSE) # zeigt nur Ladungen ab .30 an
fa.diagram(pfa_8, digits=2, cut=.40, simple=FALSE) # zeigt nur Ladungen ab .40 an
# schauen Sie sich die Grafiken an und versuchen Sie, diese inhaltlich nachzuvollziehen, 
# indem Sie sich auch die Items anschauen: Machen die Faktoren inhaltlich Sinn? Wie könnte man die Faktoren interpretieren?


