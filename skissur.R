library(DT)
library(tidyverse)

stodvar <- read.csv("skjol/stodvar.csv",header = T,encoding = "UTF-8",row.names = NULL)
Names2017 <- make.names(stodvar[, 1], unique=TRUE) # Make a vector of names from the first column
stodvar <- stodvar[, -1] #delete the first column

stodvar <- stodvar %>% 
  mutate_all(~na_if(., '#VALUE!')) %>% replace(is.na(.), 0) %>% #replace "#VALUE!" with zeros
  mutate_if(is.character,as.integer)

rownames(stodvar) <- Names2017#[as.numeric(rownames(stodvar))] 

stodvar <- stodvar %>% 
  filter_all(any_vars(. != 0)) #remove rows with only zeros

#Names <- make.names(rownames(stodvar)) 
#rownames(stodvar) <- Names#[as.numeric(rownames(stodvar))]

DT::datatable(stodvar,rownames = T, caption = "2017")


stodvar <- read.csv("skjol/stod2015.csv",header = T,encoding = "UTF-8",row.names = NULL)
Names2015 <- make.names(stodvar[, 1], unique=TRUE)
stodvar <- stodvar[, -1]
stodvar <- stodvar %>% filter_all(any_vars(. != 0))
str(stodvar)

rownames(stodvar) <- Names2015[as.numeric(rownames(stodvar))] #bull
DT::datatable(stodvar,rownames = T, caption = "2015")

####

stodvar <- read.csv("skjol/stodvar.csv",header = T,encoding = "UTF-8",row.names = NULL)
stodvar <- stodvar[rowSums(stodvar[,-1])!=0,]
Names2017 <- stodvar[, 1]
stodvar <- read.csv("skjol/stod2015.csv",header = T,encoding = "UTF-8",row.names = NULL)
stodvar <- stodvar[rowSums(stodvar[,-1])!=0,]
Names2015 <- stodvar[, 1]
Names <- list(c(Names2015,Names2017))

sapply(Names, function(x) trimws(x))


sapply(Names, function(x) gsub("\\.|\\ TUNICATA EÐA FLEIRI?|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))

ormanofn$Flokkun <- gsub("\\.|\\ sp|\\(p)|\\/.*","",ormanofn$Flokkun)
ormanofn <- as.list(ormanofn$Flokkun)





Names <- gsub("."," ",Names)
ormanofn <- Names

ormanofn <- cbind(Names2015,Names2017)
ormanofn <- ormanofn[!duplicated(ormanofn),]





##



pngfile <- pdftools::pdf_convert('skjol/lykill.pdf', dpi = 600)
(islenska <- tesseract("isl"))
text <- tesseract::ocr(pngfile, engine = islenska)
#cat(text)

#(islenska <- tesseract("isl"))








x <- readLines("Kolgr2016/A7A/talning.txt",encoding = "UTF-8", skipNul = T)
write.table(x[grep("^[#].*",x,invert = F)], "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)

x <- read.csv("Kolgr2016/A7A/talning.csv")
DT::datatable(x, caption = 'Talning A7A 2016')


x$Flokkun <-  trimws(x$Flokkun,which = "left")
x$Flokkun <-  stringr::str_to_sentence(x$Flokkun)

x
write.csv(x, "Kolgr2016/A7A/talningB.csv", row.names=FALSE)




file_names <- list.files("skjol", pattern = "csv", full.names = T)
ormanofn <- do.call(rbind,lapply(file_names,read.csv))
Names <- make.names(ormanofn[, 1], unique=TRUE) # Make a vector of names from the first column
ormanofn <- ormanofn[, -1] #delete the first column
ormanofn <- ormanofn %>% 
  mutate_all(~na_if(., '#VALUE!')) %>% replace(is.na(.), 0) %>% #replace "#VALUE!" with zeros
  mutate_if(is.character,as.integer)

rownames(ormanofn) <- Names#[as.numeric(rownames(stodvar))] 
ormanofn <- ormanofn %>% 
  filter_all(any_vars(. != 0)) #remove rows with only zeros
ormanofn <- ormanofn[!duplicated(ormanofn),]
DT::datatable(ormanofn,rownames = T, caption = "2017")




dir <- list.dirs()
library(data.table)  
files <- list.files("Kolgr2016",recursive = T,pattern = ".csv", full.names = T)
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
#DT::datatable(data,rownames = T, caption = "2016")

DT::datatable(data,rownames = T, caption = "2016")





"skjol/species-identification/Arthropoda.csv"
files <- list.files("skjol/species-identification",recursive = T,pattern = ".csv", full.names = T)
temp <- lapply(files, fread, sep=";")
data <- rbindlist( temp )





tafla <- families(list(Names2017))
taff <- tafla
stodvar$Flokkun

for (i in 1:dim(taff)[1]) {
  cbind(taff[i,], stodvar[stodvar$Flokkun==trimws(taff[i,max.col(!is.na(taff[i,]),'last')]),-1])
  df <- stodvar[stodvar$Flokkun==trimws(taff[i,max.col(!is.na(taff[i,]),'last')]),]
  print(df)
}
 
tafla[i,max.col(!is.na(tafla[i,]),'last')]



 df2 <- data.frame()
  DF <- data.frame()
  
  ifelse(
    !is.na(tafla$Species[i,]),
    df2 <- #búa til nýjan data frame df2 með nýjum dálki "ranks" þar sem orðið species, genus, subfamily osfrv. kemur fyrir í línu þeirrar tegundar "i" sem um ræðir.
      Rank %>%
      mutate(ranks = case_when(
        tolower(Rank$Species) %like% tolower(species[i]) == 1 ~ 'Species')),
    df2 <-
      Rank %>%
      mutate(ranks = case_when(
        tolower(Rank$Genus) %like% tolower(species[i]) == 1 ~ 'Genus',
        tolower(Rank$Subfamily) %like% tolower(species[i]) == 1 ~ 'Subfamily',
        tolower(Rank$Family) %like% tolower(species[i]) == 1 ~ 'Family',
        tolower(Rank$Superfamily) %like% tolower(species[i]) == 1 ~ 'Superfamily',
        tolower(Rank$Order) %like% tolower(species[i]) == 1 ~ 'Order',
        tolower(Rank$Class) %like% tolower(species[i]) == 1 ~ 'Class'
      )
      ))
  
  
  
  
  
  
  
  df <- tafla[1,]
  
  #Stöðvar 2017
  stodvar <- read.csv("skjol/stodvar.csv",header = T,encoding = "UTF-8",row.names = NULL)
  stodvar <- stodvar[rowSums(stodvar[,-1])!=0,]
  stodvar$Flokkun <- lapply(stodvar$Flokkun, function(x) trimws(x))
  stodvar$Flokkun <- sapply(stodvar$Flokkun, function(x) gsub("\\.|\\ TUNICATA EÐA FLEIRI?|\\ lirfur|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))
  stodvar$Flokkun <- sapply(stodvar$Flokkun, function(x) gsub("\\.|\\ sp|\\(p)|\\/.*","",x))
  #stodvar$Flokkun <- as.list(stodvar$Flokkun[,1])
  #stodvar$Flokkun <- stodvar$Flokkun[stringi::stri_enc_isascii(stodvar$Flokkun) & !is.na(stodvar$Flokkun) & stodvar$Flokkun != ""]
  stodvar$Flokkun <- stodvar$Flokkun[!is.na(stodvar$Flokkun) & stodvar$Flokkun != ""]
  #stodvar$Flokkun <- as.list(stodvar$Flokkun)
  
  
  library(plyr)
  ddply(stodvar,"Flokkun",numcolwise(sum))
  
  #tafla <- families(list(Names2017)) #hafa return(TheTable)
  taff <- tafla
  stodvar$Flokkun
  
  A <- list()
  for (i in 1:dim(taff)[1]) {
    A[[i]] <- cbind(taff[i,], stodvar[stodvar$Flokkun==trimws(taff[i,max.col(!is.na(taff[i,]),'last')]),-1])
    #df <- stodvar[stodvar$Flokkun==trimws(taff[i,max.col(!is.na(taff[i,]),'last')]),]
    #print(df)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  A <- list()
  for (i in 1:dim(taff)[1]) {
    A[[i]] <- cbind(taff[i,], stodvar17[stodvar17$Flokkun==trimws(taff[i,max.col(!is.na(taff[i,]),'last')]),-1])
  
  }
  
  