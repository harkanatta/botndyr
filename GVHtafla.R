#Greiningar GVH:
stodvar <- read.csv("skjol/stodvar.csv",header = T,encoding = "UTF-8",row.names = NULL)
stodvar <- stodvar[rowSums(stodvar[,-1])!=0,]
Sum2017 <- apply(stodvar[,-1], 1, sum)
Mean2017 <- apply(stodvar[,-1], 1, mean)
SD2017 <- apply(stodvar[,-1], 1, sd)
Names2017 <- stodvar[, 1]
DF2017 <- data.frame(Names=Names2017, Sum=Sum2017, Mean=round(Mean2017), SD=round(SD2017))

stodvar <- read.csv("skjol/stod2015.csv",header = T,encoding = "UTF-8",row.names = NULL)
stodvar <- stodvar[rowSums(stodvar[,-1])!=0,]
Mean2015 <- apply(stodvar[,-1], 1, mean)
Names2015 <- stodvar[, 1]
Names <- list(c(Names2015,Names2017))

#Kolgrafafjörður '16
library(data.table)  
files <- list.files("Kolgr2016",recursive = T,pattern = ".csv", full.names = T)
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
data <- list(unique(data$Flokkun))

families <- function(species){
  require(dplyr)
  require(data.table)
  require(tidyverse)
  require(DT)
  
  #species <- Names
  species <- lapply(species, function(x) trimws(x))
#  species <- sapply(species, function(x) gsub("\\.|\\ TUNICATA EÐA FLEIRI?|\\ lirfur|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))
  species <- sapply(species, function(x) gsub("\\.|\\ sp|\\(p)|\\/.*","",x))
  #species <- as.list(species[,1])
  #species <- species[stringi::stri_enc_isascii(species) & !is.na(species) & species != ""]
  species <- species[!is.na(species) & species != ""]
  species <- as.list(species)
  
  Rank <- read.csv("skjol/species-identification/Rank.csv")
  
  
  
  AList <- list()
  Anotherlist <- list()
  
  for (i in 1:length(species)) {
    df2 <- data.frame()
    DF <- data.frame()
    
    ifelse(
      lengths(strsplit(species[[i]], "\\W+")) > 1,
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
    
    ifelse(all(is.na(df2$ranks)),
    Anotherlist[i] <- species[i],NA) #Ef df2$ranks er bara með NA þá er species[i] sett til hliðar með öðrum sem passa ekki við neitt í Rank.csv
    
    if (all(is.na(df2$ranks))){next} #Ef df2$ranks er bara með NA þá er farið í næsta i
    
    fjD = seq(1:match(unique(na.omit(df2$ranks)), colnames(Rank))) #fjöldi dálka til að sækja úr Rank.csv fyrir tiltekna línu í species
    DF <- Rank[!is.na(df2$ranks),fjD]
    DF <- DF[!duplicated(DF), ] # Fyrir tilfelli þar sem species[[i]] er hærri flokkunareining en tegund. Til dæmis er Amphipoda yfir 400
    
    # ifelse(dim(DF)[2]<13,
    # for(i in (13-dim(DF))[2]:13){
    #   DF[,i] <- NA},NA)
    # 
    # DF <- cbind(DF[i,1:13], stodvar[stodvar$Flokkun==species[[i]],2:6])
    
    AList[i] <- list(DF) #listi með öllum species[i] sem passa við Rank.csv
    
  }
  
  
  TheTable <- do.call(dplyr::bind_rows, AList)
  #return(TheTable)
  DT::datatable(TheTable,caption = "Species-identification-portal")

}


#families(data)
families(Names)


ekkimed <- function(species){
  require(dplyr)
  require(data.table)
  require(tidyverse)
  require(DT)
  
  #species <- Names
  species <- lapply(species, function(x) trimws(x))
  #  species <- sapply(species, function(x) gsub("\\.|\\ TUNICATA EÐA FLEIRI?|\\ lirfur|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))
  species <- sapply(species, function(x) gsub("\\.|\\ sp|\\(p)|\\/.*","",x))
  #species <- as.list(species[,1])
  #species <- species[stringi::stri_enc_isascii(species) & !is.na(species) & species != ""]
  species <- species[!is.na(species) & species != ""]
  species <- as.list(species)
  
  Rank <- read.csv("skjol/species-identification/Rank.csv")
  
  DF <- data.frame()
  AList <- list()
  Anotherlist <- list()
  
  for (i in 1:length(species)) {
    
    ifelse(
      lengths(strsplit(species[[i]], "\\W+")) > 1,
      df2 <-
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
    ifelse(all(is.na(df2$ranks)),
           Anotherlist[i] <- species[i],NA)
    
    if (all(is.na(df2$ranks))){next}
    
    DF <- Rank[!is.na(df2$ranks),seq(1:match(unique(na.omit(df2$ranks)), colnames(Rank)))]
    AList[i] <- list(DF)
    
  }
  
  TheTable <- do.call(dplyr::bind_rows, AList)
  TheTable <- TheTable[!duplicated(TheTable), ]
  #DT::datatable(TheTable,caption = "Flokkun")
  
  #Til að sjá hvað er ekki tekið með (oft villur í nöfnum):
  NotinTheTable <- do.call(rbind,Anotherlist)
  DT::datatable(NotinTheTable,caption = "Ekki með á listanum frá Species-identification-portal")
  #print(NotinTheTable)
  
  
}




ekkimed(data)










isor siguruður kristinnson hitaveita skags
laugun@rarik.is







syni <- TheTable$Species
tegundir <- unlist(Names)
syni[match(tegundir,syni)]






rass <- c(Names[[1]][1:3])
sapply(rass, function(x) gsub("\\ ","+",x))
numer <- worrms::wm_name2id_(name = rass)
sapply(numer, function(x) wm_classification(unlist(unname(x))))







