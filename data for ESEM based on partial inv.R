library(here)

source(here("partial invariance.R"))

factor_scores <- data.frame(country = data$country)


for(i in 1:length(partial.mi$facet)){
  
  if(!is.na(partial.mi$scalar[i]) && partial.mi$scalar[i]==T){
    
    factor_scores[[as.character(partial.mi$facet.short[i])]]<-as.numeric(
      unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
      collapse=" + ")),group="country", group.equal=c("loadings","intercepts")),type = "lv")))
    
  }else if(!is.na(partial.mi$partial.scalar[i]) && partial.mi$partial.scalar[i]==T){
    
    factor_scores[[as.character(partial.mi$facet.short[i])]]<-as.numeric(
      unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
      collapse=" + ")),group="country", group.equal=c("loadings","intercepts"),
      group.partial=c(paste(compare.int(1), "~1"), 
                      paste(compare.int(2), "~1"))),type = "lv")))
    
  }else if(!is.na(partial.mi$metric[i]) && partial.mi$metric[i]==T){
  
    factor_scores[[as.character(partial.mi$facet.short[i])]]<-as.numeric(
      unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
                                                                                            collapse=" + ")),group="country", group.equal="loadings"),type = "lv")))
  }else if(!is.na(partial.mi$partial.metric[i]) && partial.mi$partial.metric[i]==T){
    
    factor_scores[[as.character(partial.mi$facet.short[i])]]<-as.numeric(
      unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
                                                                                            collapse=" + ")),group="country", group.equal="intercepts",
                            group.partial=c(paste(partial.mi$facet.short[i], "=~",compare(1)), 
                                            paste(partial.mi$facet.short[i], "=~",compare(2)))),type = "lv")))
  }else if(!is.na(partial.mi$partial.metric[i]) && partial.mi$partial.metric[i]==F && partial.mi$metric[i]==F){
    factor_scores[[as.character(partial.mi$facet.short[i])]]<-as.numeric(
      unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
                                                                                            collapse=" + ")),group="country"),type = "lv")))
  }
    
}  
<<<<<<< HEAD
#   
# write.csv(x = factor_scores, file=here::here("factor scores after invariance.csv"))
# write.table(x = factor_scores, file=here::here("factor scores after invariance.txt"), na = "-999", row.names = F, col.names = F)
# 
# write.table(x = factor_scores[factor_scores$country=="USA",], file=here::here("factor scores USA after invariance.txt"), na = "-999", row.names = F, col.names = F)
# write.table(x = factor_scores[factor_scores$country=="DE",], file=here::here("factor scores Germany after invariance.txt"), na = "-999", row.names = F, col.names = F)
# 
=======
  
write.csv(x = factor_scores, file=here::here("factor scores after invariance.csv"))
write.table(x = factor_scores, file=here::here("factor scores after invariance.txt"), na = "-999", row.names = F, col.names = F)

write.table(x = factor_scores[factor_scores$country=="USA",], file=here::here("factor scores USA after invariance.txt"), na = "-999", row.names = F, col.names = F)
write.table(x = factor_scores[factor_scores$country=="DE",], file=here::here("factor scores Germany after invariance.txt"), na = "-999", row.names = F, col.names = F)

>>>>>>> e79f6a47ba42af79b58c5a288fe150b06e5cad4d

