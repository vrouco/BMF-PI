library(here)
library(MplusAutomation)

#CFA info
source(here("functions/table3 DE.R"))

#ESEM info
here("data/Sample_Deutsch_CFA/CFA/Full model")

this.model <- readModels(here("data/Sample_Deutsch_CFA/CFA/Full model/esem_all facets on all  without a4, a5, e2, o8.out"))

mytable <- this.model$parameters$stdyx.standardized #Im repeating myself here, code equal as ESEM USA.R
this.table<-mytable[grepl("A", mytable$param) & mytable$paramHeader=="AGREE.BY",]
this.table<-rbind(this.table, mytable[grepl("C", mytable$param) & mytable$paramHeader=="CONSC.BY",])
this.table<-rbind(this.table, mytable[grepl("E", mytable$param) & mytable$paramHeader=="EXTRA.BY",])
this.table<-rbind(this.table, mytable[grepl("O", mytable$param) & mytable$paramHeader=="NEURO.BY",])
this.table<-rbind(this.table, mytable[grepl("N", mytable$param) & mytable$paramHeader=="OPEN.BY",])
this.table<-this.table[,2:3]
rm(this.model, mytable)
this.table$param<-tolower(this.table$param)


de.table <- merge(z, this.table, by.x="facets", by.y="param")
colnames(de.table)[7]<-"ESEM"


###reliability

data <- read.csv(here("data/Sample_Deutsch_CFA/German data merged Victor.csv"))

key <- read.csv(here("longkey.csv"), sep = ";")

library(MBESS)
library(psych)

which(colnames(data) %in%key$items[key$facet=="e1"])

for(i in 1:length(de.table$facets)){
  
de.table$alpha[i] <- round(as.numeric(alpha(data[,which(colnames(data) %in%key$items[key$facet==de.table$facets[i]])],
                 check.keys = T)$total$std.alpha), 2)
de.table$omega[i]<-round(as.numeric(omega(data[,which(colnames(data) %in%key$items[key$facet==de.table$facets[i]])],
                                    1)$omega.tot), 2)

}  


de.table <- de.table[,c("facets", "alpha", "omega","chisq(df)", "pvalue", "cfi", "rmsea", "ESEM")]

