library(here)
library(readxl)

dt<-read_excel(here("data/ETS_IPIP/dataset/ETS_regression.xlsx"))

options(scipen=999)

dt$hsgpa_num[which(dt$hsgpa_num<0)] <- NA
dt$gpa_univ[which(dt$gpa_univ<0)] <- NA

dt$absences2[which(dt$absences2<0)] <- NA
dt$absences4[which(dt$absences4<0)] <- NA


remove <- c("sumsO8", "sumsA5", "sumsA4", "sumsE2")
dt<-dt[,-which(colnames(dt) %in% remove)]


dt$final_abs<-ifelse(is.na(dt$absences2), dt$absences4, dt$absences2)  #put together 2 and 4 year absences
dt$final_abs<-log1p(dt$final_abs)

mydata<-dt

colnames(mydata)[43:47]<-c("A","C","E","N","O")
domains<-colnames(mydata)[43:47]

mydata[,5:61] <- as.data.frame(lapply(dt[,5:61],scale)) #to get standardized betas


########in case we need other criterions
external <- read_excel(here("data/ETS_IPIP/dataset/ETS_alle Variablen.xlsx"))
mydata$who <- scale(as.numeric(unlist(external[which(mydata$id %in% external$respid), "whoqoldom3"])))

########

myrownames<-character()
for(i in 1:5){
this<-c(grep(paste0("sums", domains[i]),colnames(mydata), value=T), domains[i])
myrownames<-c(myrownames, this)
}

perso<-mydata[,myrownames]

make.crit.table<-function(criterion){
  crit.table<-data.frame(matrix(nrow=length(colnames(mydata)[5:47]),
                               ncol=4))

  rownames(crit.table)<-myrownames
  colnames(crit.table)<-c("r", "b", "pval", "r2")

  for(i in 1:nrow(crit.table)){
    crit.table[i,1]<-round(cor(perso[,i], mydata[,criterion], use="complete.obs"), 2)
    for(j in 1:5){
      fit <- lm(paste0(criterion, "~", paste0(grep(paste0("sums", domains[j]), rownames(crit.table), value = T), collapse = "+")), data=mydata)
      crit.table[grep(paste0("sums", domains[j]), rownames(crit.table), value=T),2]<-round(fit$coefficients[-1], 2)
      crit.table[grep(paste0("sums", domains[j]), rownames(crit.table), value=T),3]<-summary(fit)$coefficients[-1,4]
      crit.table[which(rownames(crit.table) %in% domains[j]),4]<-round(summary(fit)$r.squared, 2)
    }
  }
  crit.table$pval<-ifelse(crit.table$pval < 0.01, "*", "")
  crit.table$b<-paste0(crit.table$b, crit.table$pval)
  crit.table$b<-ifelse(crit.table$b == "NANA", "", crit.table$b)
  crit.table$r2<-ifelse(is.na(crit.table$r2), "", crit.table$r2)
  crit.table<-crit.table[,-3]
  colnames(crit.table)<-paste0(colnames(crit.table), "-", paste0(strsplit(criterion, "")[[1]][1:4], collapse=""))
  return(crit.table)
}




ls <- make.crit.table("lifesat")
gpa <- make.crit.table("hsgpa_num")
abs <- make.crit.table("final_abs")

write.csv(cbind(ls, gpa, abs), here("tables/nom network.csv"))

