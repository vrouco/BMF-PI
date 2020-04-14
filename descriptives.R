
library(here)
library(readxl)
dt1<-read_excel(here("data/ETS_IPIP/dataset/ETS.xlsx"), sheet=1)

dt1$sex[dt1$sex==1]<-"Male"
dt1$sex[dt1$sex==2]<-"Female"

prop.table(table(dt1$sex))


https://fil.email/fLgLAGaA

