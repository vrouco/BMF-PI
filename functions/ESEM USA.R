require(here)
library(tidyverse)
library(xtable)

setwd(here("data/Sample_USA_EFA+CFA/CFA/Full model/ESEM"))

#let's use the model without three facets, althought the 3 factes out and 3 correlated errors works best

library(MplusAutomation)

this.model <- readModels("esem_all facets on all domains without o8, a5, a4, e2.out")
mytable <- this.model$parameters$stdyx.standardized

this.table<-mytable[grepl("A", mytable$param) & mytable$paramHeader=="AGREE.BY",]
this.table<-rbind(this.table, mytable[grepl("C", mytable$param) & mytable$paramHeader=="CONSC.BY",])
this.table<-rbind(this.table, mytable[grepl("E", mytable$param) & mytable$paramHeader=="EXTRA.BY",])
this.table<-rbind(this.table, mytable[grepl("O", mytable$param) & mytable$paramHeader=="NEURO.BY",])
this.table<-rbind(this.table, mytable[grepl("N", mytable$param) & mytable$paramHeader=="OPEN.BY",])

this.table<-this.table[,2:3]

rm(this.model, mytable)

this.table$param<-tolower(this.table$param)
this.table$est<-unlist(lapply(this.table$est, round, 2))
