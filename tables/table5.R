library(here)
library(readxl)
library(tidyverse)
setwd(here("/data/ETS_IPIP/Korrelationstabellen"))
cors <- read_excel("corr_matrix_facetts_r.xlsx", skip = 1)
colnames(cors)[1] <- "facets"
cors$facets <- sub("sums","",cors$facets)

cors_req <- cors[,c(1,3,5,7,8,9,10,11,12,13,15,16,17,18,19,20)]

colnames(cors_req) <- c("Facets", "GPAcc", "GPAuniv", "GPAhs", "SWLS", "SATt", "SATr","SATm", "SATw",
                        "ACTt", "ACTm", "ACTr", "ACTw", "ACTs", "ABS2", "ABS4")

cors_req <- cors_req[-which(cors_req$Facets=="A4"),]
cors_req <- cors_req[-which(cors_req$Facets=="A5"),]
cors_req <- cors_req[-which(cors_req$Facets=="O8"),]
cors_req <- cors_req[-which(cors_req$Facets=="E2"),]

cors_req <- cors_req[,c(1,5,2,3,4,6,7,8,9,10,11,12,13,14,15,16)]

setwd(here("tables"))
write_csv(cors_req, "table5abrev.csv")
