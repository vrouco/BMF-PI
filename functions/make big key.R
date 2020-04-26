library(readxl)
library(here)
library(MplusAutomation)
library(haven)
library(sjlabelled)

key <- read_sav(here("data/ETS_IPIP/SPSS/ETS_alle Variablen.sav"))


domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
domainsshort <- c("agree", "con", "extra", "neuro", "open")

facetstring <- as.character()
itemstring <- as.character()
facetlabelstring<-as.character()

for(i in 1:5){
  setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], "/5 Items", sep="")))
  files.here <- list.files()
  files.here <- files.here[grep("out", files.here)]
  files.here <- files.here[-grep("gesamtmodell", files.here)]
  
  for(j in 1:length(files.here)){
    model <- readModels(files.here[j])
    itemsinmodel <- unlist(strsplit(model$input$variable$usevariables, " "))
    #facet <- rep(paste0(substring(domainsshort[i], 1, 1),j), length(itemsinmodel))
    #This is not correct. get the facet acronym by seeing the ppt in EFA -> mplus to excel
    items <- itemsinmodel
    facet_label <- if(any(grepl("_5",unlist(strsplit(files.here[j], " "))))){
      rep(sub("_5", "", grep("_5",unlist(strsplit(files.here[j], " ")), value=T)), length(itemsinmodel))
    }else if(any(grepl("_3",unlist(strsplit(files.here[j], " "))))){
      rep(sub("_3", "", grep("_3",unlist(strsplit(files.here[j], " ")), value=T)), length(itemsinmodel))
      }
    
    
    facetlabelstring <- c(facetlabelstring, facet_label)
    #facetstring <- c(facetstring, facet)
    itemstring <- c(itemstring, items)
  }
}
itemstring<-sub("^e", "extra", itemstring)
itemstring<-sub("^n", "neuro", itemstring)

keylong <- data.frame(items = itemstring)

mykey <- key[,which(colnames(key)%in%itemstring)]
mykey <- mykey[,order(match(colnames(mykey),itemstring)) ]

labels <- as.character(get_label(mykey))
keylong$labels <- unlist(strsplit(labels, ": "))[-grep("personality set", unlist(strsplit(labels, ": ")))]


#keylong$facet <- facetstring
keylong$facetlabel1 <- facetlabelstring


shortkey <- read_excel(here("keyVictor.xlsx"))

keylong$facetlabel2 <- shortkey$CFAlabel[match(keylong$facet,shortkey$Short)]
keylong$facetlabel3 <- shortkey$EFAlabel[match(keylong$facet,shortkey$Short)]
keylong$facetlabel4 <- shortkey$`Victor Proposal`[match(keylong$facet,shortkey$Short)]

key2 <- read.csv(here("longkey.csv"), sep=";") #to include esem info we need the true facet acronyms
#I added them manually to the longkey.csv after seeing the ppt described above
keylong$esem <- shortkey$`ESEM loading`[match(key2$facet,shortkey$Short)]  
keylong$domain <- substr(keylong$facet, 1, 1)

write.csv(keylong[,c("domain", "labels", 
                     "facetlabel1", "facetlabel2", 
                     "facetlabel3", "esem", "items")],
          here("longkey.csv"))

