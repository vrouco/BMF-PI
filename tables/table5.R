setwd(here("/data/ETS_IPIP/Korrelationstabellen"))
cors <- read_excel("corr_matrix_facetts_r.xlsx", skip = 1)
colnames(cors)[1] <- "facets"

cors[,-1] <- as.data.frame(sapply(cors[,-1], function(x) ifelse(x >= 0.2 | x <= -0.2, 
                                                                paste("\\textbf{", x, "}", sep=""), x)))
corsabrev <- cors[,c(1,8,9,10,11,2,4,6)]

corsabrev[,-1] <- as.data.frame(sapply(corsabrev[,-1], function(x) ifelse(x >= 0.2 | x <= -0.2, 
                                                                paste("\\textbf{", x, "}", sep=""), x)))

cors <- as.data.frame(lapply(cors, as.character))
corsabrev <- as.data.frame(lapply(corsabrev, as.character))


print(xtable(cors, caption="Criterion correlations"),
      type="latex",include.rownames=F, size="\\fontsize{5.5pt}{4.5pt}\\selectfont",
      caption.placement = "top", rotate.colnames = TRUE,
      sanitize.text.function=Hmisc::latexTranslate, floating.environment = "sidewaystable")

print(xtable(corsabrev, caption="Criterion correlations"),
      type="latex",include.rownames=F, size="\\fontsize{5.5pt}{4.5pt}\\selectfont",
      caption.placement = "top", rotate.colnames = TRUE,
      sanitize.text.function=Hmisc::latexTranslate, floating.environment = "sidewaystable")
