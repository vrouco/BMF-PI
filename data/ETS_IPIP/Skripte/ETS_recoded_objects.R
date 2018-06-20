setwd("/Users/cw/Dropbox/ETS_IPIP")
library(xlsx)
dt2<-read.xlsx("ETS_recoded.xlsx", sheetIndex=1)

#objects facetts
A1<-dt2[c("A1_1", "A1_2", "A1_3", "A1_4", "A1_5")]
A2<-dt2[c("A2_1", "A2_2", "A2_3", "A2_4", "A2_5")]
A3<-dt2[c("A3_1", "A3_2", "A3_3", "A3_4", "A3_5")]
A4<-dt2[c("A4_1", "A4_2", "A4_3", "A4_4", "A4_5")]
A5<-dt2[c("A5_1", "A5_2", "A5_3", "A5_4", "A5_5")]
A6<-dt2[c("A6_1", "A6_2", "A6_3", "A6_4", "A6_5")]
A7<-dt2[c("A7_1", "A7_2", "A7_3", "A7_4", "A7_5")]
A8<-dt2[c("A8_1", "A8_2", "A8_3", "A8_4", "A8_5")]

C1<-dt2[c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5")]
C2<-dt2[c("C2_1", "C2_2", "C2_3", "C2_4", "C2_5")]
C3<-dt2[c("C3_1", "C3_2", "C3_3", "C3_4", "C3_5")]
C4<-dt2[c("C4_1", "C4_2", "C4_3", "C4_4", "C4_5")]
C5<-dt2[c("C5_1", "C5_2", "C5_3", "C5_4", "C5_5")]
C6<-dt2[c("C6_1", "C6_2", "C6_3", "C6_4", "C6_5")]
C7<-dt2[c("C7_1", "C7_2", "C7_3", "C7_4", "C7_5")]
C8<-dt2[c("C8_1", "C8_2", "C8_3", "C8_4", "C8_5")]
C9<-dt2[c("C9_1", "C9_2", "C9_3", "C9_4", "C9_5")]

E1<-dt2[c("E1_1", "E1_2", "E1_3", "E1_4", "E1_5")]
E2<-dt2[c("E2_1", "E2_2", "E2_3", "E2_4", "E2_5")]
E3<-dt2[c("E3_1", "E3_2", "E3_3", "E3_4", "E3_5")]
E4<-dt2[c("E4_1", "E4_2", "E4_3", "E4_4", "E4_5")]
E5<-dt2[c("E5_1", "E5_2", "E5_3", "E5_4", "E5_5")]
E6<-dt2[c("E6_1", "E6_2", "E6_3", "E6_4", "E6_5")]
E7<-dt2[c("E7_1", "E7_2", "E7_3", "E7_4", "E7_5")]
E8<-dt2[c("E8_1", "E8_2", "E8_3", "E8_4", "E8_5")]
E9<-dt2[c("E9_1", "E9_2", "E9_3")]

N1<-dt2[c("N1_1", "N1_2", "N1_3", "N1_4", "N1_5")]
N2<-dt2[c("N2_1", "N2_2", "N2_3", "N2_4", "N2_5")]
N3<-dt2[c("N3_1", "N3_2", "N3_3", "N3_4", "N3_5")]
N4<-dt2[c("N4_1", "N4_2", "N4_3", "N4_4", "N4_5")]
N5<-dt2[c("N5_1", "N5_2", "N5_3", "N5_4", "N5_5")]
N6<-dt2[c("N6_1", "N6_2", "N6_3", "N6_4", "N6_5")]
N7<-dt2[c("N7_1", "N7_2", "N7_3")]

O1<-dt2[c("O1_1", "O1_2", "O1_3", "O1_4", "O1_5")]
O2<-dt2[c("O2_1", "O2_2", "O2_3", "O2_4", "O2_5")]
O3<-dt2[c("O3_1", "O3_2", "O3_3", "O3_4", "O3_5")]
O4<-dt2[c("O4_1", "O4_2", "O4_3", "O4_4", "O4_5")]
O5<-dt2[c("O5_1", "O5_2", "O5_3", "O5_4", "O5_5")]
O6<-dt2[c("O6_1", "O6_2", "O6_3", "O6_4", "O6_5")]
O7<-dt2[c("O7_1", "O7_2", "O7_3", "O7_4", "O7_5")]
O8<-dt2[c("O8_1", "O8_2", "O8_3", "O8_4")]
O9<-dt2[c("O9_1", "O9_2", "O9_4")]

A<-dt2[c("A1_1", "A1_2", "A1_3", "A1_4", "A1_5", "A2_1", "A2_2", "A2_3", "A2_4", "A2_5", "A3_1", "A3_2", 
         "A3_3", "A3_4", "A3_5", "A4_1", "A4_2", "A4_3", "A4_4", "A4_5", "A5_1", "A5_2", "A5_3", "A5_4", 
         "A5_5", "A6_1", "A6_2", "A6_3", "A6_4", "A6_5", "A7_1", "A7_2", "A7_3", "A7_4", "A7_5", "A8_1", 
         "A8_2", "A8_3", "A8_4", "A8_5")]

C<-dt2[c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5", "C2_1", "C2_2", "C2_3", "C2_4", "C2_5", "C3_1", "C3_2", 
         "C3_3", "C3_4", "C3_5", "C4_1", "C4_2", "C4_3", "C4_4", "C4_5", "C5_1", "C5_2", "C5_3", "C5_4", 
         "C5_5", "C6_1", "C6_2", "C6_3", "C6_4", "C6_5", "C7_1", "C7_2", "C7_3", "C7_4", "C7_5", "C8_1", 
         "C8_2", "C8_3", "C8_4", "C8_5","C9_1", "C9_2", "C9_3", "C9_4", "C9_5")]

E<-dt2[c("E1_1", "E1_2", "E1_3", "E1_4", "E1_5","E2_1", "E2_2", "E2_3", "E2_4", "E2_5","E3_1", "E3_2", 
         "E3_3", "E3_4", "E3_5","E4_1", "E4_2", "E4_3", "E4_4", "E4_5", "E6_1", "E6_2", "E6_3", "E6_4", 
         "E6_5","E7_1", "E7_2", "E7_3", "E7_4", "E7_5", "E8_1", "E8_2", "E8_3", "E8_4", "E8_5", "E9_1", 
         "E9_2", "E9_3")]

N<-dt2[c("N1_1", "N1_2", "N1_3", "N1_4", "N1_5", "N2_1", "N2_2", "N2_3", "N2_4", "N2_5", "N3_1", "N3_2",
         "N3_3", "N3_4", "N3_5", "N4_1", "N4_2", "N4_3", "N4_4", "N4_5", "N5_1", "N5_2", "N5_3", "N5_4", 
         "N5_5", "N6_1", "N6_2", "N6_3", "N6_4", "N6_5", "N7_1", "N7_2", "N7_3")]

library(psych)
pairs.panels(A1)
pairs.panels(A2)
pairs.panels(A3)
pairs.panels(A4)
pairs.panels(A5)
pairs.panels(A6)
pairs.panels(A7)
pairs.panels(A8)

pairs.panels(C1)
pairs.panels(C2)
pairs.panels(C3)
pairs.panels(C4)
pairs.panels(C5)
pairs.panels(C6)
pairs.panels(C7)
pairs.panels(C8)
pairs.panels(C9)

pairs.panels(E1)
pairs.panels(E2)
pairs.panels(E3)
pairs.panels(E4)
pairs.panels(E5)
pairs.panels(E6)
pairs.panels(E7)
pairs.panels(E8)
pairs.panels(E9)

pairs.panels(N1)
pairs.panels(N2)
pairs.panels(N3)
pairs.panels(N4)
pairs.panels(N5)
pairs.panels(N6)
pairs.panels(N7)

pairs.panels(O1)
pairs.panels(O2)
pairs.panels(O3)
pairs.panels(O4)
pairs.panels(O5)
pairs.panels(O6)
pairs.panels(O7)
pairs.panels(O8)
pairs.panels(O9)