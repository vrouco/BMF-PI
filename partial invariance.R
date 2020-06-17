library(here)
library(lavaan)

data <- read.csv(here("USA 2nd subsample and german joint item level.csv"))
key <- read.csv(here("longkey.csv"), sep = ";")


##############
configural <- function(x){
  
  config.both <- cfa(model=model, data=data, group="country", estimator="MLR")
  fit.config <- fitMeasures(config.both)["cfi.scaled"]
  return(fit.config)
  
}

metric <- function(x){
  metric <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal="loadings")
  fit.metric <- fitMeasures(metric)["cfi.scaled"]
  
  partial.mi$metric[i] <- ifelse(configural() - fit.metric < 0.01, T, F)
  return(partial.mi)
}

scalar <- function(x){
  metric <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal="loadings")
  fit.metric <- fitMeasures(metric)["cfi.scaled"]
  scalar <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal=c("loadings", "intercepts"))
  fit.scalar <- fitMeasures(scalar)["cfi.scaled"]
  
  partial.mi$scalar[i] <- ifelse(fit.metric - fit.scalar < 0.01, T, F)
  return(partial.mi)
}

compare <- function(x){
  compare.par <- merge(par.usa[par.usa$op=="=~",c("rhs", "est")], par.germany[par.germany$op=="=~",c("rhs", "est")],
                       by="rhs")
  compare.par$compare <- abs(compare.par[,2]-compare.par[,3])
  free.new <- compare.par[order(compare.par$compare, decreasing = T),][x,1]
  return(free.new)
}


partial.metric <- function(x){
  
  
  metric.partial <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal="loadings",
                        group.partial=paste(partial.mi$facet.short[i], "=~",compare(1)))
  
  fit.metric.partial <- fitMeasures(metric.partial)["cfi.scaled"]
  
  partial.mi$partial.metric[i] <- ifelse(configural() - fit.metric.partial < 0.01, T, F)
  
  if(partial.mi$partial.metric[i]==T){
    return(list(df = partial.mi, param = compare(1)))
  }else{
    if(partial.mi$partial.metric[i]==F){
      metric.partial <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal="loadings",
                            group.partial=c(paste(partial.mi$facet.short[i], "=~",compare(1)), 
                                            paste(partial.mi$facet.short[i], "=~",compare(2))))
      fit.metric.partial <- fitMeasures(metric.partial)["cfi.scaled"]
      
      partial.mi$partial.metric[i] <- ifelse(configural() - fit.metric.partial < 0.01, T, F)
    }
    
    return(list(df = partial.mi, param = paste(compare(1), compare(2), sep=", ")))}
  
}

compare.int <- function(x){
  compare.par <- merge(par.usa[par.usa$op=="~1",c("lhs", "est")], par.germany[par.germany$op=="~1",c("lhs", "est")],
                       by="lhs")
  compare.par$compare <- abs(compare.par[,2]-compare.par[,3])
  free.new <- compare.par[order(compare.par$compare, decreasing = T),][x,1]
  return(free.new)
}

partial.scalar <- function(x){
  
  
  scalar.partial <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal=c("loadings", "intercepts"),
                        group.partial=paste(partial.mi$facet.short[i], "=~",compare.int(1)))
  
  fit.scalar.partial <- fitMeasures(scalar.partial)["cfi.scaled"]
  
  metric <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal="loadings")
  fit.metric <- fitMeasures(metric)["cfi.scaled"]
  
  partial.mi$partial.scalar[i] <- ifelse(fit.metric - fit.scalar.partial < 0.01, T, F)
  
  if(partial.mi$partial.scalar[i]==T){
    return(list(df = partial.mi, param = compare.int(1)))
  }else{
    if(partial.mi$partial.scalar[i]==F){
      scalar.partial <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal=c("loadings", "intercepts"),
                            group.partial=c(paste(partial.mi$facet.short[i], "=~",compare.int(1)), 
                                            paste(partial.mi$facet.short[i], "=~",compare.int(2))))
      fit.scalar.partial <- fitMeasures(scalar.partial)["cfi.scaled"]
      
      partial.mi$partial.scalar[i] <- ifelse(fit.metric - fit.scalar.partial < 0.01, T, F)
    }
    
    return(list(df = partial.mi, param = paste(compare.int(1), compare.int(2), sep=", ")))}
  
}


###############



partial.mi <- data.frame(facet = unique(key$facetlabel_asinB5PS),
                         facet.short = unique(key$facet),
                         metric=NA,
                         partial.metric=NA,
                         partial.metric.params =NA,
                         scalar=NA,
                         partial.scalar=NA,
                         partial.scalar.params = NA)





for(i in 1:length(partial.mi$facet)){
  
  model <- paste(partial.mi$facet.short[i], "=~", paste(key[key$facet==unique(key$facet)[i], "items"], collapse=" + "))
  
  fit.usa <- cfa(model=model, data=data[data$country=="USA",], group="country", estimator="MLR")
  par.usa <- parTable(fit.usa)
  fit.germany <- cfa(model=model, data=data[data$country=="DE",], group="country", estimator="MLR")
  par.germany <- parTable(fit.germany)
  
  
  
  partial.mi <- metric()
  if(partial.mi$metric[i]==F){
    partial.mi <- partial.metric()$df}
  
  if(partial.mi$partial.metric[i]==T && partial.mi$metric[i]==F){
    partial.mi$partial.metric.params[i] <- partial.metric()$param}
  
  
  if(partial.mi$metric[i]==T | partial.mi$partial.metric[i]==T){
    partial.mi <- scalar()
  }
  
  if(is.na(partial.mi$scalar)[i]){partial.mi$scalar[i]<-NA
  }else if(partial.mi$scalar[i]==F){
    partial.mi <- partial.scalar()$df
  }
  
  if(!is.na(partial.mi$scalar)[i] && partial.mi$partial.scalar[i]==T && partial.mi$scalar[i]==F){
  partial.mi$partial.scalar.params[i] <- partial.scalar()$param}
}



