library(here)
library(lavaan)

#Sorry - Only German data is available
#data <- read.csv(here("USA 2nd subsample and german joint item level.csv"))
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
                        group.partial=paste(compare.int(1), "~1"))
  
  fit.scalar.partial <- fitMeasures(scalar.partial)["cfi.scaled"]
  
  metric <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal="loadings")
  fit.metric <- fitMeasures(metric)["cfi.scaled"]
  
  partial.mi$partial.scalar[i] <- ifelse(fit.metric - fit.scalar.partial < 0.01, T, F)
  
  if(partial.mi$partial.scalar[i]==T){
    return(list(df = partial.mi, param = compare.int(1)))
  }else{
    if(partial.mi$partial.scalar[i]==F){
      scalar.partial <- cfa(model=model, data=data, group="country", estimator="MLR", group.equal=c("loadings", "intercepts"),
                            group.partial=c(paste(compare.int(1), "~1"), 
                                            paste(compare.int(2), "~1")))
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



metric.free <- sub(" ","",unlist(strsplit(x = partial.mi$partial.metric.params[which(!is.na(partial.mi$partial.metric.params))], split = ",")))
key[which(key$items %in% metric.free), c("items","labels")]


#########table of partial parameters



trial <- sub(" ","",unlist(strsplit(partial.mi$partial.metric.params, split=",")))
trial <- trial[which(!is.na(trial))]

metric.partial <- data.frame(item = trial)
metric.partial$labels <- key$labels[match(trial, key$items)]
metric.partial$facet <- key$facetlabel_asinB5PS[match(trial, key$items)]
metric.partial$domain <- key$domain[match(trial, key$items)]

#write.csv(metric.partial[,c("item","labels","facet")], here::here("metric invariant params.csv"))

trial <- sub(" ","",unlist(strsplit(partial.mi$partial.scalar.params, split=",")))
trial <- trial[which(!is.na(trial))]

scalar.partial <- data.frame(item = trial)
scalar.partial$labels <- key$labels[match(trial, key$items)]
scalar.partial$facet <- key$facetlabel_asinB5PS[match(trial, key$items)]
scalar.partial$domain <- key$domain[match(trial, key$items)]

#write.csv(scalar.partial[,c("item","labels","facet")], here::here("scalar invariant params.csv"))




################save parameters
##lambdas

pardf <- data.frame(item=metric.partial$item, lambda.usa = NA, se.usa = NA, lambda.ger = NA, se.ger =NA)

for(i in 1:length(metric.partial$item)){
  this.row <- metric.partial$item[i]

  this.facet <- key$facet[as.character(key$items)==this.row]

  model <- paste(this.facet,
               "=~", paste(key[key$facet==this.facet,
                               "items"], collapse=" + "))


  this.partial <- key[key$facet==this.facet,"items"][!is.na(match(key[key$facet==this.facet,"items"],metric.partial$item))]


if(length(this.partial)==1){
  fit <- cfa(model=model, data=data, group="country", group.equal="loadings", estimator="MLR",
                 group.partial=paste0(this.facet, "=~",this.partial ))
}else if(length(this.partial)==2){
  fit <- cfa(model=model, data=data, group="country", group.equal="loadings", estimator="MLR",
                 group.partial=c(paste0(this.facet, "=~",this.partial[1]),
                                      paste0(this.facet, "=~",this.partial[2])))
  }


par <- parameterestimates(fit)



par <- par[par$op=="=~",]

#par <- par[which(par$label=="" & par$est != 1),]

pardf$lambda.usa[i]<-par[par$group==1 & par$rhs==pardf$item[i], "est"]
pardf$se.usa[i]<-par[par$group==1 & par$rhs==pardf$item[i], "se"]

pardf$lambda.ger[i]<-par[par$group==2 & par$rhs==pardf$item[i], "est"]
pardf$se.ger[i]<-par[par$group==2 & par$rhs==pardf$item[i], "se"]

}


pardf[,-1] <- as.data.frame(sapply(pardf[,-1], round, 2))

metric.partial <- merge(metric.partial, pardf, by="item")

metric.partial2 <- metric.partial

metric.partial2$lambda.usa <- paste0(metric.partial2$lambda.usa, " (", metric.partial2$se.usa, ")")
metric.partial2$lambda.ger <- paste0(metric.partial2$lambda.ger, " (", metric.partial2$se.ger, ")")

#write.csv(metric.partial2[order(metric.partial2$domain, metric.partial2$facet),
                         #c("facet","item","labels","lambda.usa", "lambda.ger")], here::here("metric invariant params.csv"))

##taus

pardf <- data.frame(item=scalar.partial$item, tau.usa = NA, se.usa = NA, tau.ger = NA, se.ger =NA)

for(i in 1:length(scalar.partial$item)){
  this.row <- scalar.partial$item[i]
  
  this.facet <- key$facet[as.character(key$items)==this.row]
  
  model <- paste(this.facet,
                 "=~", paste(key[key$facet==this.facet,
                                 "items"], collapse=" + "))
  
  
  this.partial <- key[key$facet==this.facet,"items"][!is.na(match(key[key$facet==this.facet,"items"],scalar.partial$item))]
  
  
  if(length(this.partial)==1){
    fit <- cfa(model=model, data=data, group="country", group.equal=c("loadings", "intercepts"), estimator="MLR",
               group.partial=paste0(this.partial, "~1" ))
  }else if(length(this.partial)==2){
    fit <- cfa(model=model, data=data, group="country", group.equal=c("loadings", "intercepts"), estimator="MLR",
               group.partial=c(paste0(this.partial[1], "~1"),
                               paste0(this.partial[2], "~1")))
  }
  
  
  par <- parameterestimates(fit)
  
  
  
  par <- par[par$op=="~1",]
  
  #par <- par[which(par$label=="" & par$est != 1),]
  
  pardf$tau.usa[i]<-par[par$group==1 & par$lhs==pardf$item[i], "est"]
  pardf$se.usa[i]<-par[par$group==1 & par$lhs==pardf$item[i], "se"]
  
  pardf$tau.ger[i]<-par[par$group==2 & par$lhs==pardf$item[i], "est"]
  pardf$se.ger[i]<-par[par$group==2 & par$lhs==pardf$item[i], "se"]
  
}


pardf[,-1] <- as.data.frame(sapply(pardf[,-1], round, 2))

scalar.partial <- merge(scalar.partial, pardf, by="item")

scalar.partial2 <- scalar.partial

scalar.partial2$tau.usa <- paste0(scalar.partial2$tau.usa, " (", scalar.partial2$se.usa, ")")
scalar.partial2$tau.ger <- paste0(scalar.partial2$tau.ger, " (", scalar.partial2$se.ger, ")")

#write.csv(scalar.partial2[order(scalar.partial2$domain, as.character(scalar.partial2$facet)),
                         #c("facet","item","labels","tau.usa", "tau.ger")], here::here("scalar invariant params.csv"))



########which parameters overlap?

find_overlap <- function(x, y, se.x, se.y, loadings=F){
if(loadings == T){ #to get rid of negative values
  x = x+2
  y = y+2
}
if( x > y){
  return((x - (1.96*se.x))-(y + (1.96*se.y)))
}else if(x < y){
  return((y - (1.96*se.y))- (x + (1.96*se.x)))
}
}



for(i in 1:length(metric.partial$item)){
  metric.partial$overlap[i] <- find_overlap(loadings=T, 
                                            metric.partial$lambda.usa[i], metric.partial$lambda.ger[i], metric.partial$se.usa[i], metric.partial$se.ger[i])
}



for(i in 1:length(scalar.partial$item)){
scalar.partial$overlap[i] <- find_overlap(scalar.partial$tau.usa[i], scalar.partial$tau.ger[i], scalar.partial$se.usa[i], scalar.partial$se.ger[i])
}



####check goal orientation directionality

library(psych)
c5items <- key[key$facet==unique(key$facet)[11], "items"]

alpha(data[data$country=="USA", as.character(c5items)], check.keys=T)
alpha(data[data$country=="DE", as.character(c5items)], check.keys=T)
alpha(data[, as.character(c5items)], check.keys=T)
omega(data[, as.character(c5items)])

