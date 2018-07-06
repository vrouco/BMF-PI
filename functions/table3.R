library(here)
library(tidyverse)

make.table3 <- function(){
setwd(here("/functions"))
  source("table3 5items.R")
  source("table3 unlimited items.R")
  source("table3 DE.R")
  source("keys facet abbrev.R")

#need key, table3DE, table3unlimited & table3-5

x$abrev <- key$abrev[match(x$facets, key$facets)]
x <- x[order(x$abrev),]
x$facets <- x$abrev
x <- x[,-7]
colnames(x)[3]<-"chisq(df)"
x$pvalue <- ifelse(x$pvalue < 0.001, "<.001", x$pvalue)

y$abrev <- key$abrev[match(y$facets, key$facets)]
y <- y[order(y$abrev),]
y$facets <- y$abrev
y <- y[,-7]
colnames(y)[3]<-"chisq(df)"
y$pvalue <- ifelse(y$pvalue < 0.001, "<.001", y$pvalue)
##need y from table 3


table3 <- merge(x,y,by=c("facets"), suffixes = c("",""))
table3 <- merge(table3, z, by="facets", suffixes=c("",""))
return(table3)
}

table3 <- make.table3()

setwd(here("/tables"))
#write_excel_csv(x = table3, path = "table3.csv")


