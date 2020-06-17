

data <- read.table(here("data/Measurement Invariance/Domänen mit ESEM/factorscores_combined.txt"))


library(psych)
b5.efa <- fa(data[,2:43], nfact = 5, rotate = "geominQ", fm = "ml")


b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 2), nrow = 42, ncol = 5))
rownames(b5.loadmat) <- colnames_facets[2:43]



terms <- vector()
for (i in 1:5) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse = "+"))
}



b5.esem <- paste(terms, collapse = "\n")
b5.esem
colnames(data)<-colnames_facets

b5.cfa2 <- lavaan::cfa(b5.esem, data = data, verbose = F)

summary(b5.cfa2, fit.measures=T)

lavaan::fitmeasures(b5.cfa2, c("cfi","tli","rmsea","srmr"))


######without some facets

data2 <- data[,-c(5,6,20,42)]

b5.efa <- fa(data2[,2:39], nfact = 5, rotate = "geominQ", fm = "ml")

b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 2), nrow = 38, ncol = 5))
rownames(b5.loadmat) <- colnames(data2)[2:39]


terms <- vector()
for (i in 1:5) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse = "+"))
}



b5.esem <- paste(terms, collapse = "\n")
b5.esem
colnames(data)<-colnames_facets

b5.cfa2 <- lavaan::cfa(b5.esem, data = data2, verbose = F)

summary(b5.cfa2, fit.measures=T)

lavaan::fitmeasures(b5.cfa2, c("cfi","tli","rmsea","srmr"))


######measurement invariance

fitmeasures(cfa(
  model = b5.esem,
  data = data2,
  group = "country"), c("cfi","rmsea","srmr"))

fitmeasures(cfa(
  model = b5.esem,
  data = data2,
  group = "country",
  group.equal="loadings"), c("cfi","rmsea","srmr"))

fitmeasures(cfa(
  model = b5.esem,
  data = data2,
  group = "country",
  group.equal=c("loadings", "intercepts")), c("cfi","rmsea","srmr"))




#####try something different

data2 <- data[,-c(5,6,20,42)]

b5.efa <- fa(data2[,2:39], nfact = 5, rotate = "geominQ", fm = "ml")

b5.loadmat <- zapsmall(matrix(round(b5.efa$loadings, 2), nrow = 38, ncol = 5))
rownames(b5.loadmat) <- colnames(data2)[2:39]


terms <- vector()
for (i in 1:5) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(b5.loadmat[,i]), "*", names(b5.loadmat[,1]), collapse="+"))
}



b5.esem <- paste(terms, collapse = "\n")
b5.esem
colnames(data)<-colnames_facets

b5.cfa2 <- lavaan::cfa(b5.esem, data = data2, verbose = F)

lavaan::fitmeasures(b5.cfa2, c("cfi","tli","rmsea","srmr"))


#########try with psych
library(psych)

esem(data2[,c(-1,-40)],colnames(data2)[c(-1,-40)])
