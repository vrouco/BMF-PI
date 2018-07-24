library(here)
library(readxl)
setwd(here("data/ETS_IPIP/Datensätze"))
#setwd("/Users/cw/Dropbox/ETS_IPIP/Datens?tze")

dt<-read_excel("ETS_regression.xlsx")

# Missings definieren
dt$lifesat[dt$lifesat == -77]<- NA
dt$hsgpa_num[dt$hsgpa == -77]<- NA
dt$gpa_univ[dt$gpa_univ == -77]<- NA
dt$absences2[dt$absences2 == -77]<- NA
dt$absences4[dt$absences4 == -77]<- NA
dt$highedlvl2[dt$highedlvl2 == -77]<- NA
dt$highedlvl4[dt$highedlvl4 == -77]<- NA
dt$cog15fac1[dt$cog15fac1 == -77]<- NA


#### hsgpa ####
#model 1 hsgpa
m1hs<- lm(hsgpa_num~age+sex, data=dt)

#model 2 hsgpa
#model 2A
m2hsa<- lm(hsgpa_num~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2hsc<-lm(hsgpa_num~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2hse<-lm(hsgpa_num~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2hso<-lm(hsgpa_num~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2hsn<-lm(hsgpa_num~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova hsgpa (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1hs,m2hsa)  #signifikant-------NO. none is significant. THEY become significant if nonsense obs removed dt <- dt[-which(dt$hsgpa_num < 0), ]
anova(m1hs,m2hsc)  #signifikant
anova(m1hs,m2hse)  #signifikant
anova(m1hs,m2hso)  #signifikant
anova(m1hs,m2hsn)  #signifikant

#Neuer Datensatz ohne Missings bei cog15fac1
dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 mit datnew
m1hs<- lm(hsgpa_num~age+sex, data=dtnew)
#model 3 mit datnew
m3hs<- lm(hsgpa_num~age+sex+cog15fac1, data=dtnew)


#anova hsgpa (Vergleich sex+age vs. sex+age+intelligence )
anova(m1hs,m3hs) #signifikant


# R2 hsgpa Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex) NONE is important
rm1hs <- summary.lm(m1hs)
r2m1hs<-rm1hs$"r.squared" 
r2m1hs
# 
rm3hs <- summary.lm(m3hs)
r2m3hs<-rm3hs$"r.squared" 
r2m3hs
# 
rm2hsa <- summary.lm(m2hsa)
r2m2hsa<-rm2hsa$"r.squared"
r2m2hsa
# 
rm2hsc <- summary.lm(m2hsc)
r2m2hsc<-rm2hsc$"r.squared"
r2m2hsc
# 
rm2hse <- summary.lm(m2hse)
r2m2hse<-rm2hse$"r.squared"
r2m2hse
# 
rm2hso <- summary.lm(m2hso)
r2m2hso<-rm2hso$"r.squared"
r2m2hso
# 
rm2hsn <- summary.lm(m2hsn)
r2m2hsn<-rm2hsn$"r.squared"
r2m2hsn

#Delta R? A Facetten
r2m2hsa-r2m1hs
#Delta R? C Facetten
r2m2hsc-r2m1hs
#Delta R? E Facetten
r2m2hse-r2m1hs
#Delta R? O Facetten
r2m2hso-r2m1hs
#Delta R? N Facetten
r2m2hsn-r2m1hs
#Delta R? Intelligenz
r2m3hs-r2m1hs


#model 3 hsgpa
m3hs<- lm(hsgpa_num~age+sex+cog15fac1, data=dt)
#model 3A
m3hsa<- lm(hsgpa_num~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3hsc<-lm(hsgpa_num~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3hse<-lm(hsgpa_num~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3hso<-lm(hsgpa_num~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3hsn<-lm(hsgpa_num~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova hsgpa (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3hs,m3hsa) #signifikant.......NONE sig
anova(m3hs,m3hsc) #signifikant
anova(m3hs,m3hse) #signifikant 
anova(m3hs,m3hso) #signifikant
anova(m3hs,m3hsn) #signifikant

# 
#R? hsgpa Facetten (inkremt. Valid. zu age+sex+intelligence)
rm3hsa <- summary.lm(m3hsa)
r2m3hsa<-rm3hsa$"r.squared"
r2m3hsa
#
# # R? hsgpa Facetten (inkremt. Valid. zu age+sex+intelligence)
rm3hsa <- summary.lm(m3hsa)
r2m3hsa<-rm3hsa$"r.squared"
r2m3hsa
# # 
rm3hsc <- summary.lm(m3hsc)
r2m3hsc<-rm3hsc$"r.squared"
r2m3hsc
# # 
rm3hse <- summary.lm(m3hse)# # r2m3hse<-rm3hse$"r.squared"
r2m3hse
#  
rm3hso <- summary.lm(m3hso)
r2m3hso<-rm3hso$"r.squared"
r2m3hso
# # 
rm3hsn <- summary.lm(m3hsn)
r2m3hsn<-rm3hsn$"r.squared"
r2m3hsn
# # 
# # #Delta R? A Facetten
r2m3hsa-r2m3hs
# # #Delta R? C Facetten
r2m3hsc-r2m3hs
# # #Delta R? E Facetten
r2m3hse-r2m3hs
# # #Delta R? O Facetten
r2m3hso-r2m3hs
# # #Delta R? N Facetten
r2m3hsn-r2m3hs


#model 4 hsgpa
m4hs<-lm(hsgpa_num~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1hs<- lm(hsgpa_num~age+sex, data=dt)
#model 4A
m4hsa<- lm(hsgpa_num~age+sex+sumsA, data=dt)
#model 4C
m4hsc<-lm(hsgpa_num~age+sex+sumsC, data=dt)
#model 4E
m4hse<-lm(hsgpa_num~age+sex+sumsE, data=dt)
#model 4O
m4hso<-lm(hsgpa_num~age+sex+sumsO, data=dt)
#model 4N
m4hsn<-lm(hsgpa_num~age+sex+sumsN, data=dt)


#anova hsgpa (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1hs,m4hsa)  #signifikant...AGAIN
anova(m1hs,m4hsc)  #signifikant
anova(m1hs,m4hse)  #nicht signifikant
anova(m1hs,m4hso)  #signifikant
anova(m1hs,m4hsn)  #signifikant
anova(m1hs,m4hs)   #signifikant

# R? hsgpa Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4hsa <- summary.lm(m4hsa)
r2m4hsa<-rm4hsa$"r.squared"
r2m4hsa
# 
rm4hsc <- summary.lm(m4hsc)
r2m4hsc<-rm4hsc$"r.squared"
r2m4hsc
# 
rm4hse <- summary.lm(m4hse)
r2m4hse<-rm4hse$"r.squared"
r2m4hse
# 
rm4hso <- summary.lm(m4hso)
r2m4hso<-rm4hso$"r.squared"
r2m4hso
# 
rm4hsn <- summary.lm(m4hsn)
r2m4hsn<-rm4hsn$"r.squared"
r2m4hsn
# 
rm4hs <- summary.lm(m4hs)
r2m4hs<-rm4hs$"r.squared"
r2m4hs
# 
# #Delta R? A
r2m4hsa-r2m1hs
# #Delta R? C
 r2m4hsc-r2m1hs
# #Delta R? O
 r2m4hso-r2m1hs
# #Delta R? N
 r2m4hsn-r2m1hs
# #Delta R? alle Dom?nen
 r2m4hs-r2m1hs

#model 5 hsgpa
m5hs<-lm(hsgpa_num~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5hsa<- lm(hsgpa_num~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5hsc<-lm(hsgpa_num~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5hse<-lm(hsgpa_num~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5hso<-lm(hsgpa_num~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5hsn<-lm(hsgpa_num~age+sex+cog15fac1+sumsN, data=dt)


#anova hsgpa (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3hs,m5hsa) #signifikant
anova(m3hs,m5hsc) #THIS IS MARGINALLY SIG
anova(m3hs,m5hse) #nicht signifikant 
anova(m3hs,m5hso) #nicht signifikant
anova(m3hs,m5hsn) #signifikant
anova(m3hs,m5hs)  #signifikant

# R? hsgpa Dom?nen
rm5hsa <- summary.lm(m5hsa)
r2m5hsa<-rm5hsa$"r.squared"
r2m5hsa

rm5hsc <- summary.lm(m5hsc)
r2m5hsc<-rm5hsc$"r.squared"
r2m5hsc

rm5hse <- summary.lm(m5hse)
r2m5hse<-rm5hse$"r.squared"
r2m5hse

rm5hso <- summary.lm(m5hso)
r2m5hso<-rm5hso$"r.squared"
r2m5hso

rm5hsn <- summary.lm(m5hsn)
r2m5hsn<-rm5hsn$"r.squared"
r2m5hsn

rm5hs<-summary.lm(m5hs)
r2m5hs<-rm5hs$"r.squared"
r2m5hs

#Delta R? A 
r2m5hsa-r2m3hs
#Delta R? C
r2m5hsc-r2m3hs
#Delta R? N
r2m5hsn-r2m3hs
#Delta R? alle Dom?nen
r2m5hs-r2m3hs



#### gpa_univ ####
#model 1 gpa_univ
m1uni<- lm(gpa_univ~age+sex, data=dt)

#model 2 gpa_univ
#model 2A
m2unia<- lm(gpa_univ~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2unic<-lm(gpa_univ~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2unie<-lm(gpa_univ~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2unio<-lm(gpa_univ~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2unin<-lm(gpa_univ~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)

#anova gpa_univ (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1uni,m2unia)  #THIS IS
anova(m1uni,m2unic)  #TOO
anova(m1uni,m2unie)  #nicht signifikant
anova(m1uni,m2unio)  #ALSO
anova(m1uni,m2unin)  #ALSO

#Neuer Datensatz ohne Missings bei cog15fac1
#dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 gpa_univ mit datnew
m1uni<- lm(gpa_univ~age+sex, data=dtnew)
#model 3 gpa_univ mit datnew
m3uni<- lm(gpa_univ~age+sex+cog15fac1, data=dtnew)


#anova gpa_univ (Vergleich sex+age vs. sex+age+intelligence)
anova(m1uni,m3uni) #YES


# R? gpa_univ Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex)
rm1uni <- summary.lm(m1uni)
r2m1uni<-rm1uni$"r.squared" 
r2m1uni

rm3uni <- summary.lm(m3uni)
r2m3uni<-rm3uni$"r.squared" 
r2m3uni

rm2unia <- summary.lm(m2unia)
r2m2unia<-rm2unia$"r.squared"
r2m2unia

rm2unic <- summary.lm(m2unic)
r2m2unic<-rm2unic$"r.squared"
r2m2unic

rm2unie <- summary.lm(m2unie)
r2m2unie<-rm2unie$"r.squared"
r2m2unie

rm2unio <- summary.lm(m2unio)
r2m2unio<-rm2unio$"r.squared"
r2m2unio

rm2unin <- summary.lm(m2unin)
r2m2unin<-rm2unin$"r.squared"
r2m2unin

#Delta R? A Facetten
r2m2unia-r2m1uni
#Delta R? C Facetten
r2m2unic-r2m1uni
#Delta R? O Facetten
r2m2unio-r2m1uni
#Delta R? N Facetten
r2m2unin-r2m1uni
#Delta R? Intelligenz
r2m3uni-r2m1uni


#model 3 gpa_univ
m3uni<- lm(gpa_univ~age+sex+cog15fac1, data=dt)
#model 3A
m3unia<- lm(gpa_univ~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3unic<-lm(gpa_univ~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3unie<-lm(gpa_univ~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3unio<-lm(gpa_univ~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3unin<-lm(gpa_univ~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova gpa_univ (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3uni,m3unia) #signifikant
anova(m3uni,m3unic) #signifikant
anova(m3uni,m3unie) #nicht signifikant 
anova(m3uni,m3unio) #signifikant
anova(m3uni,m3unin) #signifikant

# R? gpa_univ Facetten (inkremt. Valid. zu intelligence)
rm3unia <- summary.lm(m3unia)
r2m3unia<-rm3unia$"r.squared"
r2m3unia

rm3unic <- summary.lm(m3unic)
r2m3unic<-rm3unic$"r.squared"
r2m3unic

rm3unie <- summary.lm(m3unie)
r2m3unie<-rm3unie$"r.squared"
r2m3unie

rm3unio <- summary.lm(m3unio)
r2m3unio<-rm3unio$"r.squared"
r2m3unio

rm3unin <- summary.lm(m3unin)
r2m3unin<-rm3unin$"r.squared"
r2m3unin

#Delta R? A Facetten
r2m3unia-r2m3uni
#Delta R? C Facetten
r2m3unic-r2m3uni
#Delta R? O Facetten
r2m3unio-r2m3uni
#Delta R? N Facetten
r2m3unin-r2m3uni


#model 4 gpa_univ
m4uni<-lm(gpa_univ~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1uni<- lm(gpa_univ~age+sex, data=dt)
#model 4A
m4unia<- lm(gpa_univ~age+sex+sumsA, data=dt)
#model 4C
m4unic<-lm(gpa_univ~age+sex+sumsC, data=dt)
#model 4E
m4unie<-lm(gpa_univ~age+sex+sumsE, data=dt)
#model 4O
m4unio<-lm(gpa_univ~age+sex+sumsO, data=dt)
#model 4N
m4unin<-lm(gpa_univ~age+sex+sumsN, data=dt)



#anova gpa_univ (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1uni,m4unia)  #nicht signifikant
anova(m1uni,m4unic)  #signifikant
anova(m1uni,m4unie)  #nicht signifikant
anova(m1uni,m4unio)  #nicht signifikant
anova(m1uni,m4unin)  #nicht signifikant
anova(m1uni,m4uni)   #signifikant

# R? gpa_univ Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4unia <- summary.lm(m4unia)
r2m4unia<-rm4unia$"r.squared"
r2m4unia

rm4unic <- summary.lm(m4unic)
r2m4unic<-rm4unic$"r.squared"
r2m4unic

rm4unie <- summary.lm(m4unie)
r2m4unie<-rm4unie$"r.squared"
r2m4unie

rm4unio <- summary.lm(m4unio)
r2m4unio<-rm4unio$"r.squared"
r2m4unio

rm4unin <- summary.lm(m4unin)
r2m4unin<-rm4unin$"r.squared"
r2m4unin

rm4uni <- summary.lm(m4uni)
r2m4uni<-rm4uni$"r.squared"
r2m4uni

#Delta R? C
r2m4unic-r2m1uni
#Delta R? alle Dom?nen
r2m4uni-r2m1uni


#model 5 gpa_univ
m5uni<-lm(gpa_univ~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5unia<- lm(gpa_univ~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5unic<-lm(gpa_univ~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5unie<-lm(gpa_univ~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5unio<-lm(gpa_univ~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5unin<-lm(gpa_univ~age+sex+cog15fac1+sumsN, data=dt)


#anova gpa_univ (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3uni,m5unia) #nicht signifikant
anova(m3uni,m5unic) #nicht signifikant
anova(m3uni,m5unie) #nicht signifikant 
anova(m3uni,m5unio) #nicht signifikant
anova(m3uni,m5unin) #nicht signifikant
anova(m3uni,m5uni)  #signifikant

# R? gpa_univ Dom?nen
rm5unia<-summary.lm(m5unia)
r2m5unia<-rm5unia$"r.squared"
r2m5unia

rm5unic<-summary.lm(m5unic)
r2m5unic<-rm5unic$"r.squared"
r2m5unic

rm5unie<-summary.lm(m5unie)
r2m5unie<-rm5unie$"r.squared"
r2m5unie

rm5unio<-summary.lm(m5unio)
r2m5unio<-rm5unio$"r.squared"
r2m5unio

rm5unin<-summary.lm(m5unin)
r2m5unin<-rm5unin$"r.squared"
r2m5unin


rm5uni<-summary.lm(m5uni)
r2m5uni<-rm5uni$"r.squared"
r2m5uni

#Delta R? alle Dom?nen
r2m5uni-r2m3uni



#### absences2 ####
#model 1 absences2
m1ab2<- lm(absences2~age+sex, data=dt)

#model 2 absences2
#model 2A
m2ab2a<- lm(absences2~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2ab2c<-lm(absences2~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2ab2e<-lm(absences2~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2ab2o<-lm(absences2~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2ab2n<-lm(absences2~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)

#anova absences2 (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1ab2,m2ab2a)  #nicht signifikant
anova(m1ab2,m2ab2c)  #signifikant
anova(m1ab2,m2ab2e)  #nicht signifikant
anova(m1ab2,m2ab2o)  #nicht signifikant
anova(m1ab2,m2ab2n)  #nicht signifikant

#Neuer Datensatz ohne Missings bei cog15fac1
#dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 absences2 mit datnew
m1ab2<- lm(absences2~age+sex, data=dtnew)
#model 3 absences2 mit datnew
m3ab2<- lm(absences2~age+sex+cog15fac1, data=dtnew)


#anova absences2(Vergleich sex+age vs. sex+age+intelligence)
anova(m1ab2,m3ab2) #nicht signifikant


# R? absences2 Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex)
rm1ab2 <- summary.lm(m1ab2)
r2m1ab2<-rm1ab2$"r.squared"
r2m1ab2

rm3ab2 <- summary.lm(m3ab2)
r2m3ab2<-rm3ab2$"r.squared" 
r2m3ab2

rm2ab2a <- summary.lm(m2ab2a)
r2m2ab2a<-rm2ab2a$"r.squared"
r2m2ab2a

rm2ab2c <- summary.lm(m2ab2c)
r2m2ab2c<-rm2ab2c$"r.squared"
r2m2ab2c   ##HIGHEST R2

rm2ab2e <- summary.lm(m2ab2e)
r2m2ab2e<-rm2ab2e$"r.squared"
r2m2ab2e   #THIS TOO

rm2ab2o <- summary.lm(m2ab2o)
r2m2ab2o<-rm2ab2o$"r.squared"
r2m2ab2o  #AND THIS

rm2ab2n <- summary.lm(m2ab2n)
r2m2ab2n<-rm2ab2n$"r.squared"
r2m2ab2n

#Delta R? C Facetten
r2m2ab2c-r2m1ab2
#Delta R? Intelligenz
r2m3ab2-r2m1ab2


#model 3 absences2
m3ab2<- lm(absences2~age+sex+cog15fac1, data=dt)
#model 3A
m3ab2a<- lm(absences2~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3ab2c<-lm(absences2~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3ab2e<-lm(absences2~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3ab2o<-lm(absences2~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3ab2n<-lm(absences2~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova absences2 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3ab2,m3ab2a) #nicht signifikant
anova(m3ab2,m3ab2c) #signifikant
anova(m3ab2,m3ab2e) #signifikant 
anova(m3ab2,m3ab2o) #nicht signifikant
anova(m3ab2,m3ab2n) #nicht signifikant

# R? absences2 Facetten (inkremt. Valid. zu intelligence)
rm3ab2a <- summary.lm(m3ab2a)
r2m3ab2a<-rm3ab2a$"r.squared"
r2m3ab2a

rm3ab2c <- summary.lm(m3ab2c)
r2m3ab2c<-rm3ab2c$"r.squared"
r2m3ab2c

rm3ab2e <- summary.lm(m3ab2e)
r2m3ab2e<-rm3ab2e$"r.squared"
r2m3ab2e

rm3ab2o <- summary.lm(m3ab2o)
r2m3ab2o<-rm3ab2o$"r.squared"
r2m3ab2o

rm3ab2n <- summary.lm(m3ab2n)
r2m3ab2n<-rm3ab2n$"r.squared"
r2m3ab2n

#Delta R? C Facetten
r2m3ab2c-r2m3ab2
#Delta R? E Facetten
r2m3ab2e-r2m3ab2


#model 4 absences2
m4ab2<-lm(absences2~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1ab2<- lm(absences2~age+sex, data=dt)
#model 4A
m4ab2a<- lm(absences2~age+sex+sumsA, data=dt)
#model 4C
m4ab2c<-lm(absences2~age+sex+sumsC, data=dt)
#model 4E
m4ab2e<-lm(absences2~age+sex+sumsE, data=dt)
#model 4O
m4ab2o<-lm(absences2~age+sex+sumsO, data=dt)
#model 4N
m4ab2n<-lm(absences2~age+sex+sumsN, data=dt)



#anova absences2 (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1ab2,m4ab2a)  #nicht signifikant
anova(m1ab2,m4ab2c)  #signifikant
anova(m1ab2,m4ab2e)  #nicht signifikant
anova(m1ab2,m4ab2o)  #nicht signifikant
anova(m1ab2,m4ab2n)  #nicht signifikant
anova(m1ab2,m4ab2)   #signifikant

# R? absences2 Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4ab2a <- summary.lm(m4ab2a)
r2m4ab2a<-rm4ab2a$"r.squared"
r2m4ab2a

rm4ab2c <- summary.lm(m4ab2c)
r2m4ab2c<-rm4ab2c$"r.squared"
r2m4ab2c

rm4ab2e <- summary.lm(m4ab2e)
r2m4ab2e<-rm4ab2e$"r.squared"
r2m4ab2e

rm4ab2o <- summary.lm(m4ab2o)
r2m4ab2o<-rm4ab2o$"r.squared"
r2m4ab2o

rm4ab2n <- summary.lm(m4ab2n)
r2m4ab2n<-rm4ab2n$"r.squared"
r2m4ab2n

rm4ab2 <- summary.lm(m4ab2)
r2m4ab2<-rm4ab2$"r.squared"
r2m4ab2

#Delta R? C 
r2m4ab2c-r2m1ab2
#Delta R? alle Dom?nen
r2m4ab2-r2m1ab2


#model 5 absences2
m5ab2<-lm(absences2~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5ab2a<- lm(absences2~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5ab2c<-lm(absences2~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5ab2e<-lm(absences2~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5ab2o<-lm(absences2~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5ab2n<-lm(absences2~age+sex+cog15fac1+sumsN, data=dt)


#anova absences2 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3ab2,m5ab2a) #nicht signifikant
anova(m3ab2,m5ab2c) #signifikant
anova(m3ab2,m5ab2e) #nicht signifikant 
anova(m3ab2,m5ab2o) #nicht signifikant
anova(m3ab2,m5ab2n) #nicht signifikant
anova(m3ab2,m5ab2)  #signifikant

# R? absences2 Dom?nen
rm5ab2a<-summary.lm(m5ab2a)
r2m5ab2a<-rm5ab2a$"r.squared"
r2m5ab2a

rm5ab2c<-summary.lm(m5ab2c)
r2m5ab2c<-rm5ab2c$"r.squared"
r2m5ab2c

rm5ab2e<-summary.lm(m5ab2e)
r2m5ab2e<-rm5ab2e$"r.squared"
r2m5ab2e

rm5ab2o<-summary.lm(m5ab2o)
r2m5ab2o<-rm5ab2o$"r.squared"
r2m5ab2o

rm5ab2n<-summary.lm(m5ab2n)
r2m5ab2n<-rm5ab2n$"r.squared"
r2m5ab2n

rm5ab2<-summary.lm(m5ab2)
r2m5ab2<-rm5ab2$"r.squared"
r2m5ab2

#Delta R? C
r2m5ab2c-r2m3ab2
#Delta R? alle Dom?nen
r2m5ab2-r2m3ab2



#### absences4 ####
#model 1 absences4
m1ab4<- lm(absences4~age+sex, data=dt)

#model 2 absences4
#model 2A
m2ab4a<- lm(absences4~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2ab4c<-lm(absences4~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2ab4e<-lm(absences4~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2ab4o<-lm(absences4~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2ab4n<-lm(absences4~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)

#anova absences4 (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1ab4,m2ab4a)  #signifikant
anova(m1ab4,m2ab4c)  #signifikant
anova(m1ab4,m2ab4e)  #signifikant
anova(m1ab4,m2ab4o)  #nicht signifikant
anova(m1ab4,m2ab4n)  #signifikant

#Neuer Datensatz ohne Missings bei cog15fac1
#dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 absences4 mit datnew
m1ab4<- lm(absences4~age+sex, data=dtnew)
#model 3 absences4 mit datnew
m3ab4<- lm(absences4~age+sex+cog15fac1, data=dtnew)


#anova absences4(Vergleich sex+age vs. sex+age+intelligence)
anova(m1ab4,m3ab4) #nicht signifikant

# R? absences4 Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex)
rm1ab4 <- summary.lm(m1ab4)
r2m1ab4<-rm1ab4$"r.squared" 
r2m1ab4

rm3ab4 <- summary.lm(m3ab4)
r2m3ab4<-rm3ab4$"r.squared"
r2m3ab4

rm2ab4a <- summary.lm(m2ab4a)
r2m2ab4a<-rm2ab4a$"r.squared"
r2m2ab4a

rm2ab4c <- summary.lm(m2ab4c)
r2m2ab4c<-rm2ab4c$"r.squared"
r2m2ab4c

rm2ab4e <- summary.lm(m2ab4e)
r2m2ab4e<-rm2ab4e$"r.squared"
r2m2ab4e

rm2ab4o <- summary.lm(m2ab4o)
r2m2ab4o<-rm2ab4o$"r.squared"
r2m2ab4o

rm2ab4n <- summary.lm(m2ab4n)
r2m2ab4n<-rm2ab4n$"r.squared"
r2m2ab4n

#Delta R? A Facetten
r2m2ab4a-r2m1ab4
#Delta R? C Facetten
r2m2ab4c-r2m1ab4
#Delta R? E Facetten
r2m2ab4e-r2m1ab4
#Delta R? N Facetten
r2m2ab4n-r2m1ab4


#model 3 absences4
m3ab4<- lm(absences4~age+sex+cog15fac1, data=dt)
#model 3A
m3ab4a<- lm(absences4~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3ab4c<-lm(absences4~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3ab4e<-lm(absences4~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3ab4o<-lm(absences4~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3ab4n<-lm(absences4~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova absences4 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3ab4,m3ab4a) #nicht signifikant
anova(m3ab4,m3ab4c) #signifikant
anova(m3ab4,m3ab4e) #signifikant 
anova(m3ab4,m3ab4o) #signifikant
anova(m3ab4,m3ab4n) #signifikant

# R? absences4 Facetten (inkremt. Valid. zu intelligence)
rm3ab4 <- summary.lm(m3ab4)
r2m3ab4<-rm3ab4$"r.squared" 
r2m3ab4

rm3ab4a <- summary.lm(m3ab4a)
r2m3ab4a<-rm3ab4a$"r.squared"
r2m3ab4a

rm3ab4c <- summary.lm(m3ab4c)
r2m3ab4c<-rm3ab4c$"r.squared"
r2m3ab4c

rm3ab4e <- summary.lm(m3ab4e)
r2m3ab4e<-rm3ab4e$"r.squared"
r2m3ab4e

rm3ab4o <- summary.lm(m3ab4o)
r2m3ab4o<-rm3ab4o$"r.squared"
r2m3ab4o

rm3ab4n <- summary.lm(m3ab4n)
r2m3ab4n<-rm3ab4n$"r.squared"
r2m3ab4n


#Delta R? C Facetten
r2m3ab4c-r2m3ab4
#Delta R? E Facetten
r2m3ab4e-r2m3ab4
#Delta R? O Facetten
r2m3ab4o-r2m3ab4
#Delta R? N Facetten
r2m3ab4n-r2m3ab4


#model 4 absences4
m4ab4<-lm(absences4~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1ab4<- lm(absences4~age+sex, data=dt)
#model 4A
m4ab4a<- lm(absences4~age+sex+sumsA, data=dt)
#model 4C
m4ab4c<-lm(absences4~age+sex+sumsC, data=dt)
#model 4E
m4ab4e<-lm(absences4~age+sex+sumsE, data=dt)
#model 4O
m4ab4o<-lm(absences4~age+sex+sumsO, data=dt)
#model 4N
m4ab4n<-lm(absences4~age+sex+sumsN, data=dt)


#anova absences4 (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1ab4,m4ab4a)  #signifikant
anova(m1ab4,m4ab4c)  #signifikant
anova(m1ab4,m4ab4e)  #nicht signifikant
anova(m1ab4,m4ab4o)  #nicht signifikant
anova(m1ab4,m4ab4n)  #signifikant
anova(m1ab4,m4ab4)   #signifikant

# R? absences4 Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4ab4a <- summary.lm(m4ab4a)
r2m4ab4a<-rm4ab4a$"r.squared"
r2m4ab4a

rm4ab4c <- summary.lm(m4ab4c)
r2m4ab4c<-rm4ab4c$"r.squared"
r2m4ab4c

rm4ab4e <- summary.lm(m4ab4e)
r2m4ab4e<-rm4ab4e$"r.squared"
r2m4ab4e

rm4ab4o <- summary.lm(m4ab4o)
r2m4ab4o<-rm4ab4o$"r.squared"
r2m4ab4o

rm4ab4n <- summary.lm(m4ab4n)
r2m4ab4n<-rm4ab4n$"r.squared"
r2m4ab4n

rm4ab4 <- summary.lm(m4ab4)
r2m4ab4<-rm4ab4$"r.squared"
r2m4ab4

#Delta R? A 
r2m4ab4a-r2m1ab4
#Delta R? C 
r2m4ab4c-r2m1ab4
#Delta R? N 
r2m4ab4n-r2m1ab4
#Delta R? alle Dom?nen
r2m4ab4-r2m1ab4

#model 5 absences4
m5ab4<-lm(absences4~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5ab4a<- lm(absences4~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5ab4c<-lm(absences4~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5ab4e<-lm(absences4~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5ab4o<-lm(absences4~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5ab4n<-lm(absences4~age+sex+cog15fac1+sumsN, data=dt)


#anova absences4 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3ab4,m5ab4a) #signifikant
anova(m3ab4,m5ab4c) #signifikant
anova(m3ab4,m5ab4e) #nicht signifikant 
anova(m3ab4,m5ab4o) #nicht signifikant
anova(m3ab4,m5ab4n) #signifikant
anova(m3ab4,m5ab4)  #signifikant

# R? absences4 Dom?nen
rm5ab4a<-summary.lm(m5ab4a)
r2m5ab4a<-rm5ab4a$"r.squared"
r2m5ab4a

rm5ab4c<-summary.lm(m5ab4c)
r2m5ab4c<-rm5ab4c$"r.squared"
r2m5ab4c

rm5ab4e<-summary.lm(m5ab4e)
r2m5ab4e<-rm5ab4e$"r.squared"
r2m5ab4e

rm5ab4o<-summary.lm(m5ab4o)
r2m5ab4o<-rm5ab4o$"r.squared"
r2m5ab4o

rm5ab4n<-summary.lm(m5ab4n)
r2m5ab4n<-rm5ab4n$"r.squared"
r2m5ab4n

rm5ab4<-summary.lm(m5ab4)
r2m5ab4<-rm5ab4$"r.squared"
r2m5ab4

#Delta R? A
r2m5ab4a-r2m3ab4
#Delta R? C
r2m5ab4c-r2m3ab4
#Delta R? N
r2m5ab4n-r2m3ab4
#Delta R? alle Dom?nen
r2m5ab4-r2m3ab4



#### lifesat ####
#model 1 lifesat
m1sat<- lm(lifesat~age+sex, data=dt)

#model 2 lifesat
#model 2A
m2sata<- lm(lifesat~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2satc<-lm(lifesat~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2sate<-lm(lifesat~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2sato<-lm(lifesat~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2satn<-lm(lifesat~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)

#anova lifesat (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1sat,m2sata)  #signifikant
anova(m1sat,m2satc)  #signifikant
anova(m1sat,m2sate)  #signifikant
anova(m1sat,m2sato)  #signifikant
anova(m1sat,m2satn)  #signifikant

#Neuer Datensatz ohne Missings bei cog15fac1
#dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 lifesat mit datnew
m1sat<- lm(lifesat~age+sex, data=dtnew)
#model 3 lifesat mit datnew
m3sat<- lm(lifesat~age+sex+cog15fac1, data=dtnew)


#anova lifesat (Vergleich sex+age vs. sex+age+intelligence)
anova(m1sat,m3sat) #signifikant

# R? lifesat Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex)
rm1sat <- summary.lm(m1sat)
r2m1sat<-rm1sat$"r.squared" 
r2m1sat

rm3sat <- summary.lm(m3sat)
r2m3sat<-rm3sat$"r.squared" 
r2m3sat

rm2sata <- summary.lm(m2sata)
r2m2sata<-rm2sata$"r.squared"
r2m2sata

rm2satc <- summary.lm(m2satc)
r2m2satc<-rm2satc$"r.squared"
r2m2satc

rm2sate <- summary.lm(m2sate)
r2m2sate<-rm2sate$"r.squared"
r2m2sate

rm2sato <- summary.lm(m2sato)
r2m2sato<-rm2sato$"r.squared"
r2m2sato

rm2satn <- summary.lm(m2satn)
r2m2satn<-rm2satn$"r.squared"
r2m2satn

#Delta R? A Facetten
r2m2sata-r2m1sat
#Delta R? C Facetten
r2m2satc-r2m1sat
#Delta R? E Facetten
r2m2sate-r2m1sat
#Delta R? O Facetten
r2m2sato-r2m1sat
#Delta R? N Facetten
r2m2satn-r2m1sat


#model 3 lifesat
m3sat<- lm(lifesat~age+sex+cog15fac1, data=dt)
#model 3A
m3sata<- lm(lifesat~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3satc<-lm(lifesat~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3sate<-lm(lifesat~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3sato<-lm(lifesat~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3satn<-lm(lifesat~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova lifesat (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3sat,m3sata) #signifikant
anova(m3sat,m3satc) #signifikant
anova(m3sat,m3sate) #signifikant 
anova(m3sat,m3sato) #signifikant
anova(m3sat,m3satn) #signifikant

# R? lifesat Facetten (inkremt. Valid. zu intelligence)
rm3sata <- summary.lm(m3sata)
r2m3sata<-rm3sata$"r.squared"
r2m3sata

rm3satc <- summary.lm(m3satc)
r2m3satc<-rm3satc$"r.squared"
r2m3satc

rm3sate <- summary.lm(m3sate)
r2m3sate<-rm3sate$"r.squared"
r2m3sate

rm3sato <- summary.lm(m3sato)
r2m3sato<-rm3sato$"r.squared"
r2m3sato

rm3satn <- summary.lm(m3satn)
r2m3satn<-rm3satn$"r.squared"
r2m3satn

#Delta R? A Facetten
r2m3sata-r2m3sat
#Delta R? C Facetten
r2m3satc-r2m3sat
#Delta R? E Facetten
r2m3sate-r2m3sat
#Delta R? O Facetten
r2m3sato-r2m3sat
#Delta R? N Facetten
r2m3satn-r2m3sat


#model 4 lifesat
m4sat<-lm(lifesat~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1sat<- lm(lifesat~age+sex, data=dt)
#model 4A
m4sata<- lm(lifesat~age+sex+sumsA, data=dt)
#model 4C
m4satc<-lm(lifesat~age+sex+sumsC, data=dt)
#model 4E
m4sate<-lm(lifesat~age+sex+sumsE, data=dt)
#model 4O
m4sato<-lm(lifesat~age+sex+sumsO, data=dt)
#model 4N
m4satn<-lm(lifesat~age+sex+sumsN, data=dt)


#anova lifesat (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1sat,m4sata)  #signifikant
anova(m1sat,m4satc)  #signifikant
anova(m1sat,m4sate)  #signifikant
anova(m1sat,m4sato)  #signifikant
anova(m1sat,m4satn)  #signifikant
anova(m1sat,m4sat)   #signifikant

# R? lifesat Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4sata <- summary.lm(m4sata)
r2m4sata<-rm4sata$"r.squared"
r2m4sata

rm4satc <- summary.lm(m4satc)
r2m4satc<-rm4satc$"r.squared"
r2m4satc

rm4sate <- summary.lm(m4sate)
r2m4sate<-rm4sate$"r.squared"
r2m4sate

rm4sato <- summary.lm(m4sato)
r2m4sato<-rm4sato$"r.squared"
r2m4sato

rm4satn <- summary.lm(m4satn)
r2m4satn<-rm4satn$"r.squared"
r2m4satn

rm4sat <- summary.lm(m4sat)
r2m4sat<-rm4sat$"r.squared"
r2m4sat

#Delta R? A
r2m4sata-r2m1sat
#Delta R? C 
r2m4satc-r2m1sat
#Delta R? E 
r2m4sate-r2m1sat
#Delta R? O 
r2m4sato-r2m1sat
#Delta R? N 
r2m4satn-r2m1sat
#Delta R? alle Dom?nen
r2m4sat-r2m1sat

#model 5 lifesat
m5sat<-lm(lifesat~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5sata<- lm(lifesat~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5satc<-lm(lifesat~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5sate<-lm(lifesat~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5sato<-lm(lifesat~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5satn<-lm(lifesat~age+sex+cog15fac1+sumsN, data=dt)


#anova lifesat (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3sat,m5sata) #signifikant
anova(m3sat,m5satc) #signifikant
anova(m3sat,m5sate) #signifikant 
anova(m3sat,m5sato) #signifikant
anova(m3sat,m5satn) #signifikant
anova(m3sat,m5sat)  #signifikant

# R? lifesat Dom?nen
rm5sata<-summary.lm(m5sata)
r2m5sata<-rm5sata$"r.squared"
r2m5sata

rm5satc<-summary.lm(m5satc)
r2m5satc<-rm5satc$"r.squared"
r2m5satc

rm5sate<-summary.lm(m5sate)
r2m5sate<-rm5sate$"r.squared"
r2m5sate

rm5sato<-summary.lm(m5sato)
r2m5sato<-rm5sato$"r.squared"
r2m5sato

rm5satn<-summary.lm(m5satn)
r2m5satn<-rm5satn$"r.squared"
r2m5satn

rm5sat<-summary.lm(m5sat)
r2m5sat<-rm5sat$"r.squared"
r2m5sat

#Delta R? A
r2m5sata-r2m3sat
#Delta R? C
r2m5satc-r2m3sat
#Delta R? E
r2m5sate-r2m3sat
#Delta R? O
r2m5sato-r2m3sat
#Delta R? N
r2m5satn-r2m3sat
#Delta R? alle Dom?nen
r2m5sat-r2m3sat



#### highedlvl2 ####
#model 1 highedlvl2
m1el2<- lm(highedlvl2~age+sex, data=dt)

#model 2 highedlvl2
#model 2A
m2el2a<- lm(highedlvl2~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2el2c<-lm(highedlvl2~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2el2e<-lm(highedlvl2~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2el2o<-lm(highedlvl2~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2el2n<-lm(highedlvl2~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)

#anova highedlvl2 (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1el2,m2el2a)  #signifikant
anova(m1el2,m2el2c)  #nicht signifikant
anova(m1el2,m2el2e)  #nicht signifikant
anova(m1el2,m2el2o)  #signifikant
anova(m1el2,m2el2n)  #nicht signifikant

#Neuer Datensatz ohne Missings bei cog15fac1
#dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 highedlvl2 mit datnew
m1el2<- lm(highedlvl2~age+sex, data=dtnew)
#model 3 highedlvl2 mit datnew
m3el2<- lm(highedlvl2~age+sex+cog15fac1, data=dtnew)


#anova highedlvl2(Vergleich sex+age vs. sex+age+intelligence)
anova(m1el2,m3el2) #nicht signifikant


# R? highedlvl2 Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex)
rm1el2 <- summary.lm(m1el2)
r2m1el2<-rm1el2$"r.squared"
r2m1el2

rm3el2 <- summary.lm(m3el2)
r2m3el2<-rm3el2$"r.squared" 
r2m3el2

rm2el2a <- summary.lm(m2el2a)
r2m2el2a<-rm2el2a$"r.squared"
r2m2el2a

rm2el2c <- summary.lm(m2el2c)
r2m2el2c<-rm2el2c$"r.squared"
r2m2el2c

rm2el2e <- summary.lm(m2el2e)
r2m2el2e<-rm2el2e$"r.squared"
r2m2el2e

rm2el2o <- summary.lm(m2el2o)
r2m2el2o<-rm2el2o$"r.squared"
r2m2el2o

rm2el2n <- summary.lm(m2el2n)
r2m2el2n<-rm2el2n$"r.squared"
r2m2el2n

#Delta R? A Facetten
r2m2el2a-r2m1el2
#Delta R? O Facetten
r2m2el2o-r2m1el2
#Delta R? Intelligenz
r2m3el2-r2m1el2


#model 3 highedlvl2
m3el2<- lm(highedlvl2~age+sex+cog15fac1, data=dt)
#model 3A
m3el2a<- lm(highedlvl2~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3el2c<-lm(highedlvl2~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3el2e<-lm(highedlvl2~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3el2o<-lm(highedlvl2~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3el2n<-lm(highedlvl2~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova highedlvl2 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3el2,m3el2a) #signifikant
anova(m3el2,m3el2c) #nicht signifikant
anova(m3el2,m3el2e) #nicht signifikant 
anova(m3el2,m3el2o) #signifikant
anova(m3el2,m3el2n) #nicht signifikant

# R? highedlvl2 Facetten (inkremt. Valid. zu intelligence)
rm3el2a <- summary.lm(m3el2a)
r2m3el2a<-rm3el2a$"r.squared"
r2m3el2a

rm3el2c<- summary.lm(m3el2c)
r2m3el2c<-rm3el2c$"r.squared"
r2m3el2c

rm3el2e <- summary.lm(m3el2e)
r2m3el2e<-rm3el2e$"r.squared"
r2m3el2e

rm3el2o <- summary.lm(m3el2o)
r2m3el2o<-rm3el2o$"r.squared"
r2m3el2o

rm3el2n <- summary.lm(m3el2n)
r2m3el2n<-rm3el2n$"r.squared"
r2m3el2n


#Delta R? A Facetten
r2m3el2a-r2m3el2
#Delta R? O Facetten
r2m3el2o-r2m3el2


#model 4 highedlvl2
m4el2<-lm(highedlvl2~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1el2<- lm(highedlvl2~age+sex, data=dt)
#model 4A
m4el2a<- lm(highedlvl2~age+sex+sumsA, data=dt)
#model 4C
m4el2c<-lm(highedlvl2~age+sex+sumsC, data=dt)
#model 4E
m4el2e<-lm(highedlvl2~age+sex+sumsE, data=dt)
#model 4O
m4el2o<-lm(highedlvl2~age+sex+sumsO, data=dt)
#model 4N
m4el2n<-lm(highedlvl2~age+sex+sumsN, data=dt)



#anova highedlvl2 (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1el2,m4el2a)  #nicht signifikant
anova(m1el2,m4el2c)  #nicht signifikant
anova(m1el2,m4el2e)  #nicht signifikant
anova(m1el2,m4el2o)  #signifikant
anova(m1el2,m4el2n)  #nicht signifikant
anova(m1el2,m4el2)   #nicht signifikant

# R? highedlvl2 Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4el2a <- summary.lm(m4el2a)
r2m4el2a<-rm4el2a$"r.squared"
r2m4el2a

rm4el2c <- summary.lm(m4el2c)
r2m4el2c<-rm4el2c$"r.squared"
r2m4el2c

rm4el2e <- summary.lm(m4el2e)
r2m4el2e<-rm4el2e$"r.squared"
r2m4el2e

rm4el2o <- summary.lm(m4el2o)
r2m4el2o<-rm4el2o$"r.squared"
r2m4el2o

rm4el2n <- summary.lm(m4el2n)
r2m4el2n<-rm4el2n$"r.squared"
r2m4el2n

rm4el2 <- summary.lm(m4el2)
r2m4el2<-rm4el2$"r.squared"
r2m4el2

#Delta R? O
r2m4el2o-r2m1el2


#model 5 highedlvl2
m5el2<-lm(highedlvl2~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5el2a<- lm(highedlvl2~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5el2c<-lm(highedlvl2~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5el2e<-lm(highedlvl2~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5el2o<-lm(highedlvl2~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5el2n<-lm(highedlvl2~age+sex+cog15fac1+sumsN, data=dt)


#anova highedlvl2 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3el2,m5el2a) #nicht signifikant
anova(m3el2,m5el2c) #nicht signifikant
anova(m3el2,m5el2e) #nicht signifikant 
anova(m3el2,m5el2o) #signifikant
anova(m3el2,m5el2n) #nicht signifikant
anova(m3el2,m5el2)  #nicht signifikant

# R? highedlvl2 Dom?nen
rm5el2a<-summary.lm(m5el2a)
r2m5el2a<-rm5el2a$"r.squared"
r2m5el2a

rm5el2c<-summary.lm(m5el2c)
r2m5el2c<-rm5el2c$"r.squared"
r2m5el2c

rm5el2e<-summary.lm(m5el2e)
r2m5el2e<-rm5el2e$"r.squared"
r2m5el2e

rm5el2o<-summary.lm(m5el2o)
r2m5el2o<-rm5el2o$"r.squared"
r2m5el2o

rm5el2n<-summary.lm(m5el2n)
r2m5el2n<-rm5el2n$"r.squared"
r2m5el2n

rm5el2<-summary.lm(m5el2)
r2m5el2<-rm5el2$"r.squared"
r2m5el2

#Delta R? C
r2m5el2o-r2m3el2


#### highedlvl4 ####
#model 1 highedlvl4
m1el4<- lm(highedlvl4~age+sex, data=dt)

#model 2 highedlvl4
#model 2A
m2el4a<- lm(highedlvl4~age+sex+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 2C
m2el4c<-lm(highedlvl4~age+sex+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 2E
m2el4e<-lm(highedlvl4~age+sex+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 2O
m2el4o<-lm(highedlvl4~age+sex+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 2N
m2el4n<-lm(highedlvl4~age+sex+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)

#anova highedlvl4 (Vergleich sex+age vs. sex+age+einzelne Facetten jeder Dom?ne)
anova(m1el4,m2el4a)  #signifikant
anova(m1el4,m2el4c)  #signifikant
anova(m1el4,m2el4e)  #nicht signifikant
anova(m1el4,m2el4o)  #signifikant
anova(m1el4,m2el4n)  #signifikant

#Neuer Datensatz ohne Missings bei cog15fac1
#dtnew<-dt[!is.na(dt$cog15fac1),]
#model 1 highedlvl4 mit datnew
m1el4<- lm(highedlvl4~age+sex, data=dtnew)
#model 3 highedlvl4 mit datnew
m3el4<- lm(highedlvl4~age+sex+cog15fac1, data=dtnew)


#anova highedlvl4(Vergleich sex+age vs. sex+age+intelligence)
anova(m1el4,m3el4) #signifikant


# R? highedlvl4 Facetten (inkremt. Valid. Facetten einer Dom?ne zu age+sex)
rm1el4 <- summary.lm(m1el4)
r2m1el4<-rm1el4$"r.squared" 
r2m1el4

rm3el4 <- summary.lm(m3el4)
r2m3el4<-rm3el4$"r.squared" 
r2m3el4

rm2el4a <- summary.lm(m2el4a)
r2m2el4a<-rm2el4a$"r.squared"
r2m2el4a

rm2el4c <- summary.lm(m2el4c)
r2m2el4c<-rm2el4c$"r.squared"
r2m2el4c

rm2el4e <- summary.lm(m2el4e)
r2m2el4e<-rm2el4e$"r.squared"
r2m2el4e

rm2el4o <- summary.lm(m2el4o)
r2m2el4o<-rm2el4o$"r.squared"
r2m2el4o

rm2el4n <- summary.lm(m2el4n)
r2m2el4n<-rm2el4n$"r.squared"
r2m2el4n

#Delta R? A Facetten
r2m2el4a-r2m1el4
#Delta R? c Facetten
r2m2el4c-r2m1el4
#Delta R? O Facetten
r2m2el4o-r2m1el4
#Delta R? N Facetten
r2m2el4n-r2m1el4
#Delta R? Intelligenz
r2m3el4-r2m1el4


#model 3 highedlvl4
m3el4<- lm(highedlvl4~age+sex+cog15fac1, data=dt)
#model 3A
m3el4a<- lm(highedlvl4~age+sex+cog15fac1+sumsA1+sumsA2+sumsA3+sumsA4+sumsA5+sumsA6+sumsA7+sumsA8, data=dt)
#model 3C
m3el4c<-lm(highedlvl4~age+sex+cog15fac1+sumsC1+sumsC2+sumsC3+sumsC4+sumsC5+sumsC6+sumsC7+sumsC8+sumsC9, data=dt)
#model 3E
m3el4e<-lm(highedlvl4~age+sex+cog15fac1+sumsE1+sumsE2+sumsE3+sumsE4+sumsE5+sumsE6+sumsE7+sumsE8+sumsE9, data=dt)
#model 3O
m3el4o<-lm(highedlvl4~age+sex+cog15fac1+sumsO1+sumsO2+sumsO3+sumsO4+sumsO5+sumsO6+sumsO7+sumsO8+sumsO9, data=dt)
#model 3N
m3el4n<-lm(highedlvl4~age+sex+cog15fac1+sumsN1+sumsN2+sumsN3+sumsN4+sumsN5+sumsN6+sumsN7, data=dt)


#anova highedlvl4 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Facetten jeder Dom?ne)
anova(m3el4,m3el4a) #signifikant
anova(m3el4,m3el4c) #signifikant
anova(m3el4,m3el4e) #nicht signifikant 
anova(m3el4,m3el4o) #signifikant
anova(m3el4,m3el4n) #signifikant

# R? highedlvl4 Facetten (inkremt. Valid. zu intelligence)
rm3el4a <- summary.lm(m3el4a)
r2m3el4a<-rm3el4a$"r.squared"
r2m3el4a

rm3el4c <- summary.lm(m3el4c)
r2m3el4c<-rm3el4c$"r.squared"
r2m3el4c

rm3el4e <- summary.lm(m3el4e)
r2m3el4e<-rm3el4e$"r.squared"
r2m3el4e

rm3el4o <- summary.lm(m3el4o)
r2m3el4o<-rm3el4o$"r.squared"
r2m3el4o

rm3el4n <- summary.lm(m3el4n)
r2m3el4n<-rm3el4n$"r.squared"
r2m3el4n

#Delta R? A Facetten
r2m3el4a-r2m3el4
#Delta R? C Facetten
r2m3el4c-r2m3el4
#Delta R? O Facetten
r2m3el4o-r2m3el4
#Delta R? N Facetten
r2m3el4n-r2m3el4


#model 4 highedlvl4
m4el4<-lm(highedlvl4~age+sex+sumsN+sumsA+sumsC+sumsE+sumsO, data=dt)
m1el4<- lm(highedlvl4~age+sex, data=dt)
#model 4A
m4el4a<- lm(highedlvl4~age+sex+sumsA, data=dt)
#model 4C
m4el4c<-lm(highedlvl4~age+sex+sumsC, data=dt)
#model 4E
m4el4e<-lm(highedlvl4~age+sex+sumsE, data=dt)
#model 4O
m4el4o<-lm(highedlvl4~age+sex+sumsO, data=dt)
#model 4N
m4el4n<-lm(highedlvl4~age+sex+sumsN, data=dt)



#anova highedlvl4 (Vergl. sex+age vs. sex+age+einzelne Dom?nen)
anova(m1el4,m4el4a)  #nicht signifikant
anova(m1el4,m4el4c)  #nicht signifikant
anova(m1el4,m4el4e)  #nicht signifikant
anova(m1el4,m4el4o)  #nicht signifikant
anova(m1el4,m4el4n)  #signifikant
anova(m1el4,m4el4)   #signifikant

# R? highedlvl4 Dom?nen (inkremt. Valid. einzelne Dom?nen zu age+sex)
rm4el4a <- summary.lm(m4el4a)
r2m4el4a<-rm4el4a$"r.squared"
r2m4el4a

rm4el4c <- summary.lm(m4el4c)
r2m4el4c<-rm4el4c$"r.squared"
r2m4el4c

rm4el4e <- summary.lm(m4el4e)
r2m4el4e<-rm4el4e$"r.squared"
r2m4el4e

rm4el4o <- summary.lm(m4el4o)
r2m4el4o<-rm4el4o$"r.squared"
r2m4el4o

rm4el4n <- summary.lm(m4el4n)
r2m4el4n<-rm4el4n$"r.squared"
r2m4el4n

rm4el4 <- summary.lm(m4el4)
r2m4el4<-rm4el4$"r.squared"
r2m4el4

#Delta R? N
r2m4el4n-r2m1el4
#Delta R? alle Dom?nen
r2m4el4-r2m1el4


#model 5 highedlvl4
m5el4<-lm(highedlvl4~age+sex+cog15fac1+sumsA+sumsC+sumsE+sumsO+sumsN, data=dt)
#model 5A
m5el4a<- lm(highedlvl4~age+sex+cog15fac1+sumsA, data=dt)
#model 5C
m5el4c<-lm(highedlvl4~age+sex+cog15fac1+sumsC, data=dt)
#model 5E
m5el4e<-lm(highedlvl4~age+sex+cog15fac1+sumsE, data=dt)
#model 5O
m5el4o<-lm(highedlvl4~age+sex+cog15fac1+sumsO, data=dt)
#model 5N
m5el4n<-lm(highedlvl4~age+sex+cog15fac1+sumsN, data=dt)


#anova highedlvl4 (Vergleich sex+age+intel. vs. sex+age+intel.+einzelne Dom?nen)
anova(m3el4,m5el4a) #nicht signifikant
anova(m3el4,m5el4c) #nicht signifikant
anova(m3el4,m5el4e) #nicht signifikant 
anova(m3el4,m5el4o) #nicht signifikant
anova(m3el4,m5el4n) #signifikant
anova(m3el4,m5el4)  #signifikant

# R? highedlvl2 Dom?nen
rm5el4<-summary.lm(m5el4)
r2m5el4<-rm5el4$"r.squared"
r2m5el4

rm5el4a<-summary.lm(m5el4a)
r2m5el4a<-rm5el4a$"r.squared"
r2m5el4a

rm5el4c<-summary.lm(m5el4c)
r2m5el4c<-rm5el4c$"r.squared"
r2m5el4c

rm5el4e<-summary.lm(m5el4e)
r2m5el4e<-rm5el4e$"r.squared"
r2m5el4e

rm5el4o<-summary.lm(m5el4o)
r2m5el4o<-rm5el4o$"r.squared"
r2m5el4o
rm5el4n<-summary.lm(m5el4n)
r2m5el4n<-rm5el4n$"r.squared"
r2m5el4n

#Delta R? C
r2m5el4n-r2m3el4
#Delta R? alle Dom?nen
r2m5el4-r2m3el4

