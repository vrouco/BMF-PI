library(here)

key <- read.csv(here("longkey.csv"), sep = ";")

domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")

fulldata <- data.frame(country="USA")

for(i in 1:5){
  fileshere <- grep(pattern = ".txt", list.files(here(paste0("data/Sample_USA_EFA+CFA/CFA/"), domains[i], "5 items")), value=T)
  fileshere <- fileshere[-grep("CFA", fileshere)]
  for(j in 1:length(fileshere)){
    localdata <- read.table(here("data/Sample_USA_EFA+CFA/CFA", domains[i],"5 items", fileshere[j]))
    localdata <- localdata[,-dim(localdata)[2]]
    colnames(localdata)<-key[key$facet==sub(".txt", "", fileshere[j]),"items"]
    fulldata <- cbind(fulldata, localdata)
    
  
  }
}

##german data

domains <- c("Extraversion", "Gewissenhaftigkeit", "Neurotizismus", "Offenheit", "Verträglichkeit")

germandata <- data.frame(country="DE")
for(i in 1:5){
  fileshere <- grep(pattern = ".txt", list.files(here(paste0("data/Sample_Deutsch_CFA/CFA/"), domains[i])), value=T)
  fileshere <- fileshere[-grep("IQIPIP", fileshere)]
  for(j in 1:length(fileshere)){
    localdata <- read.table(here("data/Sample_Deutsch_CFA/CFA", domains[i], fileshere[j]))
    localdata <- localdata[,-dim(localdata)[2]]
    colnames(localdata) <- key$items[key$facet == sub(".txt", "", fileshere[j])]
    germandata <- cbind(germandata, localdata)
  }
}

fulldata <- rbind(fulldata, germandata)

write.csv(x = fulldata, file = here::here("USA 2nd subsample and german joint item level.csv"))
