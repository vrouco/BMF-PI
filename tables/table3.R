
#need key, table3DE, table3unlimited & table3-5

x$abrev <- key$abrev[match(x$facets, key$facets)]
x <- x[order(x$abrev),]
x$facets <- x$abrev
x <- x[,-7]
colnames(x)[3]<-"chisq(df)"
x$pvalue <- ifelse(x$pvalue < 0.001, "<.001", x$pvalue)

y$abrev <- key$abrev[match(y$facets, key$facets)]
y <- y[order(y$abrev),]
y$facets <- y$abrev
y <- y[,-7]
colnames(y)[3]<-"chisq(df)"
y$pvalue <- ifelse(y$pvalue < 0.001, "<.001", y$pvalue)
##need y from table 3


table3 <- merge(x,y,by=c("facets"), suffixes = c("",""))
table3 <- merge(table3, z, by="facets", suffixes=c("",""))

library(kableExtra)

kable(table3) %>%
  kable_styling("striped") %>%
  kable_styling(font_size = 7) %>% 
  add_header_above(c(" " = 1, "Full items" = 5, "5 items" = 5, " " = 5)) %>% 
  add_header_above(c(" " = 1, "USA sample" = 10, "German sample" = 5))


library(kableExtra)


kable(table3, "latex", align = "c", booktabs = T)%>%
  kable_styling("striped") %>%
  kable_styling(font_size = 7) %>% 
  add_header_above(c(" " = 2, "Full items" = 4, "5 items" = 4))
# print(xtable(table1, caption="Model fit for each facet"),include.rownames=FALSE,
#       size="\\fontsize{9.5pt}{9pt}\\selectfont", caption.placement = "top")
