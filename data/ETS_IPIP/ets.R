
library(here)
library(readxl)
dt1<-read_excel(here("data/ETS_IPIP/dataset/ETS.xlsx"), sheet=1)

#library(xlsx)
#dat1<-read.xlsx("C:/Users/Annemarie/Dropbox/Arbeit/Diagnostik/Matthias/ETS.xlsx", sheetIndex=1)
#setwd("C:/Users/Annemarie/Dropbox/Arbeit/Diagnostik/Matthias/ETS_IPIP")
#dt1<-read.xlsx("ETS.xlsx", sheetIndex=1)

#reverse items
library(car)
dt1$A2_1<-recode(dt1$A2_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A2_2<-recode(dt1$A2_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A2_3<-recode(dt1$A2_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A2_4<-recode(dt1$A2_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A2_5<-recode(dt1$A2_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$A3_1<-recode(dt1$A3_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A3_2<-recode(dt1$A3_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A3_3<-recode(dt1$A3_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A3_4<-recode(dt1$A3_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A3_5<-recode(dt1$A3_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$A6_5<-recode(dt1$A6_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$A7_1<-recode(dt1$A7_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A7_3<-recode(dt1$A7_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A7_4<-recode(dt1$A7_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$A7_5<-recode(dt1$A7_5,"1='5'; 2='4'; 4='2'; 5='1'")


dt1$C2_1<-recode(dt1$C2_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C2_3<-recode(dt1$C2_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C2_4<-recode(dt1$C2_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C2_5<-recode(dt1$C2_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$C3_1<-recode(dt1$C3_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C3_2<-recode(dt1$C3_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C3_3<-recode(dt1$C3_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C3_4<-recode(dt1$C3_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C3_5<-recode(dt1$C3_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$C5_3<-recode(dt1$C5_3,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$C7_1<-recode(dt1$C7_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C7_2<-recode(dt1$C7_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C7_3<-recode(dt1$C7_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C7_4<-recode(dt1$C7_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$C7_5<-recode(dt1$C7_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$C8_2<-recode(dt1$C8_2,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$C9_5<-recode(dt1$C9_5,"1='5'; 2='4'; 4='2'; 5='1'")


dt1$E1_1<-recode(dt1$E1_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$E1_3<-recode(dt1$E1_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$E1_5<-recode(dt1$E1_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$E2_3<-recode(dt1$E2_3,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$E3_1<-recode(dt1$E3_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$E3_2<-recode(dt1$E3_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$E3_3<-recode(dt1$E3_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$E3_4<-recode(dt1$E3_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$E3_5<-recode(dt1$E3_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$E9_2<-recode(dt1$E9_2,"1='5'; 2='4'; 4='2'; 5='1'")


dt1$N1_1<-recode(dt1$N1_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N1_2<-recode(dt1$N1_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N1_3<-recode(dt1$N1_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N1_4<-recode(dt1$N1_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N1_5<-recode(dt1$N1_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$N2_1<-recode(dt1$N2_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N2_2<-recode(dt1$N2_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N2_3<-recode(dt1$N2_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N2_4<-recode(dt1$N2_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N2_5<-recode(dt1$N2_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$N3_1<-recode(dt1$N3_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N3_2<-recode(dt1$N3_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N3_3<-recode(dt1$N3_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N3_4<-recode(dt1$N3_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N3_5<-recode(dt1$N3_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$N5_1<-recode(dt1$N5_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N5_2<-recode(dt1$N5_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N5_3<-recode(dt1$N5_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N5_4<-recode(dt1$N5_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N5_5<-recode(dt1$N5_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$N6_1<-recode(dt1$N6_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N6_2<-recode(dt1$N6_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N6_3<-recode(dt1$N6_3,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N6_4<-recode(dt1$N6_4,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N6_5<-recode(dt1$N6_5,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$N7_1<-recode(dt1$N7_1,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N7_2<-recode(dt1$N7_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$N7_3<-recode(dt1$N7_3,"1='5'; 2='4'; 4='2'; 5='1'")


dt1$O5_4<-recode(dt1$O5_4,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$O7_4<-recode(dt1$O7_4,"1='5'; 2='4'; 4='2'; 5='1'")

dt1$O8_2<-recode(dt1$O8_2,"1='5'; 2='4'; 4='2'; 5='1'")
dt1$O8_3<-recode(dt1$O8_3,"1='5'; 2='4'; 4='2'; 5='1'")


#objects
A1<-dt1[c("A1_1", "A1_2", "A1_3", "A1_4", "A1_5")]
A2<-dt1[c("A2_1", "A2_2", "A2_3", "A2_4", "A2_5")]
A3<-dt1[c("A3_1", "A3_2", "A3_3", "A3_4", "A3_5")]
A4<-dt1[c("A4_1", "A4_2", "A4_3", "A4_4", "A4_5")]
A5<-dt1[c("A5_1", "A5_2", "A5_3", "A5_4", "A5_5")]
A6<-dt1[c("A6_1", "A6_2", "A6_3", "A6_4", "A6_5")]
A7<-dt1[c("A7_1", "A7_2", "A7_3", "A7_4", "A7_5")]
A8<-dt1[c("A8_1", "A8_2", "A8_3", "A8_4", "A8_5")]

C1<-dt1[c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5")]
C2<-dt1[c("C2_1", "C2_2", "C2_3", "C2_4", "C2_5")]
C3<-dt1[c("C3_1", "C3_2", "C3_3", "C3_4", "C3_5")]
C4<-dt1[c("C4_1", "C4_2", "C4_3", "C4_4", "C4_5")]
C5<-dt1[c("C5_1", "C5_2", "C5_3", "C5_4", "C5_5")]
C6<-dt1[c("C6_1", "C6_2", "C6_3", "C6_4", "C6_5")]
C7<-dt1[c("C7_1", "C7_2", "C7_3", "C7_4", "C7_5")]
C8<-dt1[c("C8_1", "C8_2", "C8_3", "C8_4", "C8_5")]
C9<-dt1[c("C9_1", "C9_2", "C9_3", "C9_4", "C9_5")]

E1<-dt1[c("E1_1", "E1_2", "E1_3", "E1_4", "E1_5")]
E2<-dt1[c("E2_1", "E2_2", "E2_3", "E2_4", "E2_5")]
E3<-dt1[c("E3_1", "E3_2", "E3_3", "E3_4", "E3_5")]
E4<-dt1[c("E4_1", "E4_2", "E4_3", "E4_4", "E4_5")]
E5<-dt1[c("E5_1", "E5_2", "E5_3", "E5_4", "E5_5")]
E6<-dt1[c("E6_1", "E6_2", "E6_3", "E6_4", "E6_5")]
E7<-dt1[c("E7_1", "E7_2", "E7_3", "E7_4", "E7_5")]
E8<-dt1[c("E8_1", "E8_2", "E8_3", "E8_4", "E8_5")]
E9<-dt1[c("E9_1", "E9_2", "E9_3")]

N1<-dt1[c("N1_1", "N1_2", "N1_3", "N1_4", "N1_5")]
N2<-dt1[c("N2_1", "N2_2", "N2_3", "N2_4", "N2_5")]
N3<-dt1[c("N3_1", "N3_2", "N3_3", "N3_4", "N3_5")]
N4<-dt1[c("N4_1", "N4_2", "N4_3", "N4_4", "N4_5")]
N5<-dt1[c("N5_1", "N5_2", "N5_3", "N5_4", "N5_5")]
N6<-dt1[c("N6_1", "N6_2", "N6_3", "N6_4", "N6_5")]
N7<-dt1[c("N7_1", "N7_2", "N7_3")]

O1<-dt1[c("O1_1", "O1_2", "O1_3", "O1_4", "O1_5")]
O2<-dt1[c("O2_1", "O2_2", "O2_3", "O2_4", "O2_5")]
O3<-dt1[c("O3_1", "O3_2", "O3_3", "O3_4", "O3_5")]
O4<-dt1[c("O4_1", "O4_2", "O4_3", "O4_4", "O4_5")]
O5<-dt1[c("O5_1", "O5_2", "O5_3", "O5_4", "O5_5")]
O6<-dt1[c("O6_1", "O6_2", "O6_3", "O6_4", "O6_5")]
O7<-dt1[c("O7_1", "O7_2", "O7_3", "O7_4", "O7_5")]
O8<-dt1[c("O8_1", "O8_2", "O8_3", "O8_4")]
O9<-dt1[c("O9_1", "O9_2", "O9_4")]

A<-dt1[c("A1_1", "A1_2", "A1_3", "A1_4", "A1_5", "A2_1", "A2_2", "A2_3", "A2_4", "A2_5", "A3_1", "A3_2", 
         "A3_3", "A3_4", "A3_5", "A4_1", "A4_2", "A4_3", "A4_4", "A4_5", "A5_1", "A5_2", "A5_3", "A5_4", 
         "A5_5", "A6_1", "A6_2", "A6_3", "A6_4", "A6_5", "A7_1", "A7_2", "A7_3", "A7_4", "A7_5", "A8_1", 
         "A8_2", "A8_3", "A8_4", "A8_5")]

C<-dt1[c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5", "C2_1", "C2_2", "C2_3", "C2_4", "C2_5", "C3_1", "C3_2", 
         "C3_3", "C3_4", "C3_5", "C4_1", "C4_2", "C4_3", "C4_4", "C4_5", "C5_1", "C5_2", "C5_3", "C5_4", 
         "C5_5", "C6_1", "C6_2", "C6_3", "C6_4", "C6_5", "C7_1", "C7_2", "C7_3", "C7_4", "C7_5", "C8_1", 
         "C8_2", "C8_3", "C8_4", "C8_5","C9_1", "C9_2", "C9_3", "C9_4", "C9_5")]

E<-dt1[c("E1_1", "E1_2", "E1_3", "E1_4", "E1_5","E2_1", "E2_2", "E2_3", "E2_4", "E2_5","E3_1", "E3_2", 
         "E3_3", "E3_4", "E3_5","E4_1", "E4_2", "E4_3", "E4_4", "E4_5", "E6_1", "E6_2", "E6_3", "E6_4", 
         "E6_5","E7_1", "E7_2", "E7_3", "E7_4", "E7_5", "E8_1", "E8_2", "E8_3", "E8_4", "E8_5", "E9_1", 
         "E9_2", "E9_3")]

N<-dt1[c("N1_1", "N1_2", "N1_3", "N1_4", "N1_5", "N2_1", "N2_2", "N2_3", "N2_4", "N2_5", "N3_1", "N3_2",
      "N3_3", "N3_4", "N3_5", "N4_1", "N4_2", "N4_3", "N4_4", "N4_5", "N5_1", "N5_2", "N5_3", "N5_4", 
      "N5_5", "N6_1", "N6_2", "N6_3", "N6_4", "N6_5", "N7_1", "N7_2", "N7_3")]

O<-dt1[c("O1_1", "O1_2", "O1_3", "O1_4", "O1_5", "O2_1", "O2_2", "O2_3", "O2_4", "O2_5", "O3_1", "O3_2", 
         "O3_3", "O3_4", "O3_5", "O4_1", "O4_2", "O4_3", "O4_4", "O4_5", "O5_1", "O5_2", "O5_3", "O5_4", 
         "O5_5", "O6_1", "O6_2", "O6_3", "O6_4", "O6_5", "O7_1", "O7_2", "O7_3", "O7_4", "O7_5", "O8_1", 
         "O8_2", "O8_3", "O8_4", "O9_1", "O9_2", "O9_4")]

library(psych)
describe(A)
describe(A1)
describe(A2)
describe(A3)
describe(A4)
describe(A5)
describe(A6)
describe(A7)
describe(A8)

describe(C)
describe(C1)
describe(C2)
describe(C3)
describe(C4)
describe(C5)
describe(C6)
describe(C7)
describe(C8)
describe(C9)

describe(E)
describe(E1)
describe(E2)
describe(E3)
describe(E4)
describe(E5)
describe(E6)
describe(E7)
describe(E8)
describe(E9)

describe(N)
describe(N1)
describe(N2)
describe(N3)
describe(N4)
describe(N5)
describe(N6)
describe(N7)

describe(O)
describe(O1)
describe(O2)
describe(O3)
describe(O4)
describe(O5)
describe(O6)
describe(O7)
describe(O8)
describe(O9)

alpha(A)
alpha(A1)
alpha(A2)
alpha(A3)
alpha(A4)
alpha(A5)
alpha(A6)
alpha(A7)
alpha(A8)

alpha(C)
alpha(C1)
alpha(C2)
alpha(C3)
alpha(C4)
alpha(C5)
alpha(C6)
alpha(C7)
alpha(C8)
alpha(C9)

alpha(E)
alpha(E1)
alpha(E2)
alpha(E3)
alpha(E4)
alpha(E5)
alpha(E6)
alpha(E7)
alpha(E8)
alpha(E9)

alpha(N)
alpha(N1)
alpha(N2)
alpha(N3)
alpha(N4)
alpha(N5)
alpha(N6)
alpha(N7)

alpha(O)
alpha(O1)
alpha(O2)
alpha(O3)
alpha(O4)
alpha(O5)
alpha(O6)
alpha(O7)
alpha(O8)
alpha(O9)

library(psych)
alpha(A, check.keys=FALSE)
alpha(C, check.keys=FALSE)
alpha(E, check.keys=FALSE)
alpha(N, check.keys=FALSE)
alpha(O, check.keys=FALSE)

#sumscores
dt1$sumsA1<-rowSums(A1)
dt1$sumsA2<-rowSums(A2)
dt1$sumsA3<-rowSums(A3)
dt1$sumsA4<-rowSums(A4)
dt1$sumsA5<-rowSums(A5)
dt1$sumsA6<-rowSums(A6)
dt1$sumsA7<-rowSums(A7)
dt1$sumsA8<-rowSums(A8)

dt1$sumsC1<-rowSums(C1)
dt1$sumsC2<-rowSums(C2)
dt1$sumsC3<-rowSums(C3)
dt1$sumsC4<-rowSums(C4)
dt1$sumsC5<-rowSums(C5)
dt1$sumsC6<-rowSums(C6)
dt1$sumsC7<-rowSums(C7)
dt1$sumsC8<-rowSums(C8)
dt1$sumsC9<-rowSums(C9)

dt1$sumsE1<-rowSums(E1)
dt1$sumsE2<-rowSums(E2)
dt1$sumsE3<-rowSums(E3)
dt1$sumsE4<-rowSums(E4)
dt1$sumsE5<-rowSums(E5)
dt1$sumsE6<-rowSums(E6)
dt1$sumsE7<-rowSums(E7)
dt1$sumsE8<-rowSums(E8)
dt1$sumsE9<-rowSums(E9)

dt1$sumsN1<-rowSums(N1)
dt1$sumsN2<-rowSums(N2)
dt1$sumsN3<-rowSums(N3)
dt1$sumsN4<-rowSums(N4)
dt1$sumsN5<-rowSums(N5)
dt1$sumsN6<-rowSums(N6)
dt1$sumsN7<-rowSums(N7)

dt1$sumsO1<-rowSums(O1)
dt1$sumsO2<-rowSums(O2)
dt1$sumsO3<-rowSums(O3)
dt1$sumsO4<-rowSums(O4)
dt1$sumsO5<-rowSums(O5)
dt1$sumsO6<-rowSums(O6)
dt1$sumsO7<-rowSums(O7)
dt1$sumsO8<-rowSums(O8)
dt1$sumsO9<-rowSums(O9)

dt1$sumsA<-rowSums(A)
dt1$sumsC<-rowSums(C)
dt1$sumsE<-rowSums(E)
dt1$sumsN<-rowSums(N)
dt1$sumsO<-rowSums(O)

#new data set
setwd("/Users/cw/Dropbox/ETS_IPIP")
write.xlsx(dt1, "ETS_recoded.xlsx")

dtn<-read.xlsx("ETS_recoded.xlsx", sheetIndex=1)


dt1$A1<-A1
dt1$A2<-A2
dt1$A3<-A3
dt1$A4<-A4
dt1$A5<-A5
dt1$A6<-A6
dt1$A7<-A7
dt1$A8<-A8

dt1$C1<-C1
dt1$C2<-C2
dt1$C3<-C3
dt1$C4<-C4
dt1$C5<-C5
dt1$C6<-C6
dt1$C7<-C7
dt1$C8<-C8
dt1$C9<-C9

dt1$E1<-E1
dt1$E2<-E2
dt1$E3<-E3
dt1$E4<-E4
dt1$E5<-E5
dt1$E6<-E6
dt1$E7<-E7
dt1$E8<-E8
dt1$E9<-E9

dt1$N1<-N1
dt1$N2<-N2
dt1$N3<-N3
dt1$N4<-N4
dt1$N5<-N5
dt1$N6<-N6
dt1$N7<-N7

dt1$O1<-O1
dt1$O2<-O2
dt1$O3<-O3
dt1$O4<-O4
dt1$O5<-O5
dt1$O6<-O6
dt1$O7<-O7
dt1$O8<-O8
dt1$O9<-O9

dt1$A<-A
dt1$C<-C
dt1$E<-E
dt1$N<-N
dt1$O<-O

is.data.frame(dt1)
library(xlsx)
write.xlsx(dt1, "dt1.xlsx")
