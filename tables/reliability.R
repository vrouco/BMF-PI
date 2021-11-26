library(here)
library(readxl)


dt<-read_excel(here("data/ETS_IPIP/Dataset/IPIP.xlsx"))
items <- dt[,72:281]

# source("keys facet abbrev.R")
# make.key()


key <- c(paste("O", rep(1:9), sep=""),
  paste("C", rep(1:9), sep=""),
  paste("E", rep(1:9), sep=""),
  paste("A", rep(1:8), sep=""),
  paste("N", rep(1:7), sep=""))

for(i in 1:length(key)){
  assign(key[i],items[,grep(key[i], colnames(items))])
}


#dimensions
O <- cbind(O1,O2,O3,O4,O5,O6,O7,O8,O9)
C <- cbind(C1,C2,C3,C4,C5,C6,C7,C8,C9)
E <- cbind(E1,E2,E3,E4,E5,E6,E7,E8,E9)
A <- cbind(A1,A2,A3,A4,A5,A6,A7,A8)
N <- cbind(N1,N2,N3,N4,N5,N6,N7)

library(MBESS)

# alpha(O, check.keys = T)
# alpha(C, check.keys = T)
# alpha(A, check.keys = T)
# alpha(N, check.keys = T)
# alpha(E, check.keys = T)

library(psych)

facets.rel <- data.frame(facets= key,
           alpha=NA,
           omega=NA)

for(i in 1:length(key)){
  facets.rel[i,2] <- as.numeric(psych::alpha(get(key[i]), check.keys = T)$total$std.alpha)
  facets.rel[i,3] <- as.numeric(omega(get(key[i]), 1)$omega.tot)
}

facets.rel[,2:3] <- as.data.frame(lapply(facets.rel[,2:3], round,2))

#write_csv(facets.rel, here("tables/facets reliability.csv"))

omega(O, 1,n.iter=10,plot=FALSE)
omega(C, 1,n.iter=10,plot=FALSE)
omega(E, 1,n.iter=10,plot=FALSE)
omega(A, 1,n.iter=10,plot=FALSE)
omega(N, 1,n.iter=10,plot=FALSE)
