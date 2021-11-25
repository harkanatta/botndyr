families <- function(species){
  require(dplyr)
  require(data.table)
  require(tidyverse)
  require(DT)
  
  #species <- Names
  species <- lapply(species, function(x) trimws(x))
  species <- sapply(species, function(x) gsub("\\.|\\ TUNICATA EÐA FLEIRI?|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))
  species <- as.list(species[,1])
  
  ormar <- read.csv("skjol/species-identification/Polychaeta.csv")
  ormarA <- read.csv("skjol/species-identification/mollusca.csv")
  ormarB <- read.csv("skjol/species-identification/nemertea.csv")
  ormarC <- read.csv("skjol/species-identification/Arthropoda.csv")
  ormarD <- read.csv("skjol/species-identification/Arthropoda.csv")
  ormar <- (c(unname(ormar)[,2],unname(ormarA)[,2],unname(ormarB)[,2],unname(ormarC)[,2],unname(ormarD)[,2])) 
  
  rass <- list()
  DFb <- data.frame()
  for (z in 1:length(ormar)) {
    x <- strsplit(ormar[z], "\n", fixed = TRUE)
    sellur <- sub('^\\w+', '', x[[1]])
    titlar <- sapply(strsplit(x[[1]], ' '), function(i)paste(i[-c(2:4)], collapse = ' '))
    DFb <- data.frame(as.list(sellur))
    names(DFb) <- titlar
    rass[z] <- list(DFb)
  }
  
  DF <- data.frame(matrix(ncol = 13, nrow = 0))
  dalkar <- c("Kingdom","Phylum","Subphylum","Class", "Subclass", "Superorder","Order","Suborder","Superfamily","Family","Subfamily","Genus","Species")
  colnames(DF) <- dalkar
  rass[z+1] <- list(DF)
  
  rass <- rass[order(sapply(rass,ncol),decreasing = T)]
  rass[[1]] <- rass[[1]] %>% mutate(across(where(is.logical), as.character))
  
  Rank <- do.call(dplyr::bind_rows, rass)
  
  
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
  DT::datatable(TheTable,caption = "Tegundir frá GVH")
  NotinTheTable <- do.call(rbind,Anotherlist)
  print(NotinTheTable)
  
}


syni <- TheTable$Species
tegundir <- unlist(Names)
syni[match(tegundir,syni)]
