library(here)
library(readxl)
setwd(here("data/ETS_IPIP/Datensätze"))

dt<-read_excel("ETS_regression.xlsx")

################################SWLS

cor(dt$sumsN2, dt$lifesat)
cor(dt$sumsE4, dt$lifesat)

summary(lm(dt$lifesat~dt$sumsN2 + dt$sumsE4))
summary(lm(dt$lifesat~dt$sumsN2 + dt$sumsE4 + dt$age + dt$sex))
range(dt$age)

step(lm(data = dt, lifesat ~ sumsE1 + sumsE2 + sumsE3 + sumsE4 + sumsE5 + sumsE6 + sumsE7 + sumsE8 + sumsE9 +
          sumsN1 + sumsN2 + sumsN3 + sumsN4 + sumsN5 + sumsN6 + sumsN7 + age))

summary(lm(formula = lifesat ~ sumsE4 + sumsE9 + sumsN2 + sumsN5 + age, 
   data = dt))

pool <- dt[,c(4:46, 54)]#using all facets in stepwise regression
step(lm(data=pool, lifesat ~ .))

summary(lm(formula = lifesat ~ age + sumsA1 + sumsC4 + sumsC6 + sumsC7 + 
             sumsE2 + sumsE4 + sumsE9 + sumsN2 + sumsO1 + sumsO2 + sumsO8 + 
             sumsO9, data = pool))




########################################## GPA

range(dt$hsgpa_num)
dt <- dt[-which(dt$hsgpa_num < 0), ]

summary(lm(data=dt, hsgpa_num ~ sumsO + sumsC + sumsE + sumsA + sumsN)) #C and A!!?

pool <- dt[,c(3:46, 53)]
step(lm(data=pool, hsgpa_num ~ .))
summary(lm(formula = hsgpa_num ~ sex + age + sumsA1 + sumsA4 + sumsA5 + 
             sumsA6 + sumsA7 + sumsA8 + sumsC1 + sumsC5 + sumsE1 + sumsE2 + 
             sumsE4 + sumsE5 + sumsE8 + sumsO4 + sumsO5 + sumsO8 + sumsO9, 
             data = pool))

#Hypothesis: C5 goal orientation (NEO achievement striving), C3 self discipline, NEO dutifulness between 
#task planning C4 and dominance C1
cor(dt$hsgpa_num, dt$sumsC) #very smilar to O'Connor Paunonen 2007
cor(dt$hsgpa_num, dt$sumsC5)
cor(dt$hsgpa_num, dt$sumsC3)
cor(dt$hsgpa_num, dt$sumsC4)

summary(lm(data=dt, hsgpa_num ~ sumsC5 + sumsC3 + sumsC4 + sumsC1))

##summing up all facets which might be important



        