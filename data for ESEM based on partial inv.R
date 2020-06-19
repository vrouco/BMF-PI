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
    
  }else if(!is.na(partial.mi$metric[i]) && partial.mi$metric[i]==T)
  
  
  
}
  
  
  
  
  cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"], collapse=" + ")),
      group="country", group.equal=c("loadings","intercepts"))
  
  factor_scores[,2] <- lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"], collapse=" + ")),
      group="country", group.equal=c("loadings","intercepts")),type = "lv")
  
  
  as.numeric(unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
                                                                                                   collapse=" + ")),
                                   group="country", group.equal=c("loadings","intercepts")),type = "lv")))
  
  
  factor_scores[[as.character(partial.mi$facet.short[i])]]<-as.numeric(unlist(lavPredict(cfa(data=data, model = paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"],
                                                                                                                                           collapse=" + ")),
                                                                           group="country", group.equal=c("loadings","intercepts")),type = "lv")))
  