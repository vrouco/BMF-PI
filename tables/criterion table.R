library(here)
library(readxl)
setwd(here("data/ETS_IPIP/Dataset"))

dt<-read_excel("ETS_regression.xlsx")

options(scipen=999)

dt$hsgpa_num[which(dt$hsgpa_num<0)] <- NA
dt$gpa_univ[which(dt$gpa_univ<0)] <- NA

dt$absences2[which(dt$absences2<0)] <- NA
dt$absences4[which(dt$absences4<0)] <- NA

dt$absences2 <- log1p(dt$absences2)
dt$absences4 <- log1p(dt$absences4)

ctable <- as.data.frame(matrix(nrow=52,ncol=15 ))
colnames(ctable) <- c("LS_cor",
                      "LS_beta",
                      "LS_R",
                      "GPA_cor",
                      "GPA_beta",
                      "GPA_R",
                      "GPAhs_cor",
                      "GPAhs_beta",
                      "GPAhs_R",
                      "ABShs_cor",
                      "ABShs_beta",
                      "ABShs_R",
                      "ABSuniv_cor",
                      "ABSuniv_beta",
                      "ABSuniv_R")
rownames(ctable)<-c("Openness", paste("O", seq(1,9), sep=""), "Domain score1",
                    "Conscientiousness", paste("C", seq(1,9), sep=""), "Domain score2",
                    "Extraversion", paste("E", seq(1,9), sep=""), "Domain score3",
                    "Agreeableness", paste("A", seq(1,8), sep=""), "Domain score4",
                    "Neuroticism", paste("N", seq(1,7), sep=""), "Domain score5")

## correlations
#ls
for(i in 1:9){
  ctable[1+i,1] <- cor(dt$lifesat, dt[,grep("O",colnames(dt))[i]])
  ctable[12+i,1] <- cor(dt$lifesat, dt[,grep("C",colnames(dt))[i]])
  ctable[23+i,1] <- cor(dt$lifesat, dt[,grep("E",colnames(dt))[i]])
}
for(i in 1:8){
  ctable[34+i,1] <- cor(dt$lifesat, dt[,grep("A",colnames(dt))[i]])
}
for(i in 1:7){
  ctable[44+i,1] <- cor(dt$lifesat, dt[,grep("N",colnames(dt))[i]])
}

domainnames <- c("sumsO","sumsC","sumsE", "sumsA", "sumsN")
for(i in 1:5){
  ctable[grep("Domain", rownames(ctable))[i],1] <- cor(dt$lifesat, dt[,tail(grep(domainnames[i],colnames(dt), value=T), 1)])
}


  
#gpa_univ
for(i in 1:9){
  ctable[1+i,4] <- cor(dt$gpa_univ, dt[,grep("O",colnames(dt))[i]], use="complete.obs")
  ctable[12+i,4] <- cor(dt$gpa_univ, dt[,grep("C",colnames(dt))[i]], use="complete.obs")
  ctable[23+i,4] <- cor(dt$gpa_univ, dt[,grep("E",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:8){
  ctable[34+i,4] <- cor(dt$gpa_univ, dt[,grep("A",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:7){
  ctable[44+i,4] <- cor(dt$gpa_univ, dt[,grep("N",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:5){
  ctable[grep("Domain", rownames(ctable))[i],4] <- cor(dt$gpa_univ, dt[,tail(grep(domainnames[i],colnames(dt), value=T), 1)], use="complete.obs")
}

#gpa_hs
for(i in 1:9){
  ctable[1+i,7] <- cor(dt$hsgpa_num, dt[,grep("O",colnames(dt))[i]], use="complete.obs")
  ctable[12+i,7] <- cor(dt$hsgpa_num, dt[,grep("C",colnames(dt))[i]], use="complete.obs")
  ctable[23+i,7] <- cor(dt$hsgpa_num, dt[,grep("E",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:8){
  ctable[34+i,7] <- cor(dt$hsgpa_num, dt[,grep("A",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:7){
  ctable[44+i,7] <- cor(dt$hsgpa_num, dt[,grep("N",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:5){
  ctable[grep("Domain", rownames(ctable))[i],7] <- cor(dt$hsgpa_num, dt[,tail(grep(domainnames[i],colnames(dt), value=T), 1)], use="complete.obs")
}

#absences2
for(i in 1:9){
  ctable[1+i,10] <- cor(dt$absences2, dt[,grep("O",colnames(dt))[i]], use="complete.obs")
  ctable[12+i,10] <- cor(dt$absences2, dt[,grep("C",colnames(dt))[i]], use="complete.obs")
  ctable[23+i,10] <- cor(dt$absences2, dt[,grep("E",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:8){
  ctable[34+i,10] <- cor(dt$absences2, dt[,grep("A",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:10){
  ctable[44+i,10] <- cor(dt$absences2, dt[,grep("N",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:5){
  ctable[grep("Domain", rownames(ctable))[i],10] <- cor(dt$absences2, dt[,tail(grep(domainnames[i],colnames(dt), value=T), 1)], use="complete.obs")
}

#absences4
for(i in 1:9){
  ctable[1+i,13] <- cor(dt$absences4, dt[,grep("O",colnames(dt))[i]], use="complete.obs")
  ctable[12+i,13] <- cor(dt$absences4, dt[,grep("C",colnames(dt))[i]], use="complete.obs")
  ctable[23+i,13] <- cor(dt$absences4, dt[,grep("E",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:8){
  ctable[34+i,13] <- cor(dt$absences4, dt[,grep("A",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:13){
  ctable[44+i,13] <- cor(dt$absences4, dt[,grep("N",colnames(dt))[i]], use="complete.obs")
}
for(i in 1:5){
  ctable[grep("Domain", rownames(ctable))[i],13] <- cor(dt$absences4, dt[,tail(grep(domainnames[i],colnames(dt), value=T), 1)], use="complete.obs")
}


##betas & r square
#ls
for(i in 1:9){
ctable[1+i,2]<-lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                               collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[11,3] <- summary(lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                       collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[12+i,2]<-lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[22,3] <- summary(lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                       collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[23+i,2]<-lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                 collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[33,3] <- summary(lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                       collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:8){
  ctable[34+i,2]<-lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                 collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[43,3] <- summary(lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                       collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:7){
  ctable[44+i,2]<-lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                 collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[52,3] <- summary(lm(as.formula(paste("scale(dt$lifesat)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                       collapse="+", sep=""), sep=""))))$adj.r.squared


#gpa_univ

# for(i in 1:9){
#   ctable[1+i,5]<-lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsO", seq(1,9), ")",
#                                                                 collapse="+", sep=""), sep="")))$coefficients[1+i]}
# ctable[11,6] <- summary(lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsO", seq(1,9), ")",
#                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared
# 
# for(i in 1:9){
#   ctable[12+i,5]<-lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsC", seq(1,9), ")",
#                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}
# ctable[22,6] <- summary(lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsC", seq(1,9), ")",
#                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared
# 
# for(i in 1:9){
#   ctable[23+i,5]<-lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsE", seq(1,9), ")",
#                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}
# 
# ctable[33,6] <- summary(lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsE", seq(1,9), ")",
#                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared
# 
# 
# for(i in 1:8){
#   ctable[34+i,5]<-lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsA", seq(1,8), ")",
#                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}
# 
# ctable[43,6] <- summary(lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsA", seq(1,8), ")",
#                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared
# 
# 
# for(i in 1:7){
#   ctable[44+i,5]<-lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsN", seq(1,7), ")",
#                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}
# 
# ctable[52,6] <- summary(lm(as.formula(paste("scale(dt$gpa_univ)~",paste("scale(dt$sumsN", seq(1,7), ")",
#                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared


#gpa_hs

for(i in 1:9){
  ctable[1+i,8]<-lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                 collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[11,9] <- summary(lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[12+i,8]<-lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[22,9] <- summary(lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[23+i,8]<-lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[33,9] <- summary(lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:8){
  ctable[34+i,8]<-lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[43,9] <- summary(lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:7){
  ctable[44+i,8]<-lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[52,9] <- summary(lm(as.formula(paste("scale(dt$hsgpa_num)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared



#absences2

for(i in 1:9){
  ctable[1+i,11]<-lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                 collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[11,12] <- summary(lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[12+i,11]<-lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[22,12] <- summary(lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[23+i,11]<-lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[33,12] <- summary(lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:8){
  ctable[34+i,11]<-lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[43,12] <- summary(lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:7){
  ctable[44+i,11]<-lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                  collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[52,12] <- summary(lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                        collapse="+", sep=""), sep=""))))$adj.r.squared

#absences4

for(i in 1:9){
  ctable[1+i,14]<-lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                   collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[11,15] <- summary(lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsO", seq(1,9), ")",
                                                                          collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[12+i,14]<-lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                    collapse="+", sep=""), sep="")))$coefficients[1+i]}
ctable[22,15] <- summary(lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsC", seq(1,9), ")",
                                                                          collapse="+", sep=""), sep=""))))$adj.r.squared

for(i in 1:9){
  ctable[23+i,14]<-lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                    collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[33,15] <- summary(lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsE", seq(1,9), ")",
                                                                          collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:8){
  ctable[34+i,14]<-lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                    collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[43,15] <- summary(lm(as.formula(paste("scale(dt$absences4)~",paste("scale(dt$sumsA", seq(1,8), ")",
                                                                          collapse="+", sep=""), sep=""))))$adj.r.squared


for(i in 1:7){
  ctable[44+i,14]<-lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                    collapse="+", sep=""), sep="")))$coefficients[1+i]}

ctable[52,15] <- summary(lm(as.formula(paste("scale(dt$absences2)~",paste("scale(dt$sumsN", seq(1,7), ")",
                                                                          collapse="+", sep=""), sep=""))))$adj.r.squared


ctable <- as.data.frame(lapply(ctable, round, 2))

rownames(ctable)<-c("Openness", paste("O", seq(1,9), sep=""), "Domain score1",
                    "Conscientiousness", paste("C", seq(1,9), sep=""), "Domain score2",
                    "Extraversion", paste("E", seq(1,9), sep=""), "Domain score3",
                    "Agreeableness", paste("A", seq(1,8), sep=""), "Domain score4",
                    "Neuroticism", paste("N", seq(1,7), sep=""), "Domain score5")

# 
# ctable[,4]<-paste(ctable[,7],"/", ctable[,4], sep="")
# ctable[,5]<-paste(ctable[,8],"/", ctable[,5], sep="")
# ctable[,6]<-paste(ctable[,9],"/", ctable[,6], sep="")
# 
# ctable[,10]<-paste(ctable[,10],"/", ctable[,13], sep="")
# ctable[,11]<-paste(ctable[,11],"/", ctable[,14], sep="")
# ctable[,12]<-paste(ctable[,12],"/", ctable[,15], sep="")

ctable <- ctable[,c(-4,-5,-6,-13,-14,-15)]



write.csv(ctable, paste(here("tables/"), "criterion table round2 no uni.csv", sep=""))
