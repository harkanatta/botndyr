
families <- function(ormanofn){
  require(dplyr)
  require(data.table)
  require(tidyverse)
  require(DT)
  DF <- data.frame()
  A7Clisti <- list()
  
  
  
  
  ormanofn <- sapply(ormanofn, function(x) trimws(x))
  ormanofn <- as.list(ormanofn[,1])
  ormanofn <- sapply(ormanofn, function(x) gsub("\\.|\\ TUNICATA EÐA FLEIRI?|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))
  ormanofn <- as.list(ormanofn[,1])
  
  ormar <- read.csv("skjol/species-identification/Polychaeta.csv")
  ormarA <- read.csv("skjol/species-identification/mollusca.csv")
  ormarB <- read.csv("skjol/species-identification/nemertea.csv")
  ormar <- (c(unname(ormar)[,2],unname(ormarA)[,2],unname(ormarB)[,2])) 
    
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
  
  Polychaeta <- do.call(dplyr::bind_rows, rass)
  
  for (i in 1:length(ormanofn)) {
    #for (i in 10) {
    
    ifelse(
      lengths(strsplit(ormanofn[[i]], "\\W+")) > 1,
      df2 <-
        Polychaeta %>%
        mutate(
          starts_vowel =
            case_when(
              # left hand side of case_when must be a logical
              tolower(Polychaeta$Species) %like% tolower(ormanofn[i]) == 1 ~ 'Species')),
      df2 <-
        Polychaeta %>%
        mutate(
          starts_vowel =
            case_when(
              # left hand side of case_when must be a logical
              tolower(Polychaeta$Genus) %like% tolower(ormanofn[i]) == 1 ~ 'Genus',
              tolower(Polychaeta$Subfamily) %like% tolower(ormanofn[i]) == 1 ~ 'Subfamily',
              tolower(Polychaeta$Family) %like% tolower(ormanofn[i]) == 1 ~ 'Family',
              tolower(Polychaeta$Superfamily) %like% tolower(ormanofn[i]) == 1 ~ 'Superfamily',
              tolower(Polychaeta$Order) %like% tolower(ormanofn[i]) == 1 ~ 'Order',
              tolower(Polychaeta$Class) %like% tolower(ormanofn[i]) == 1 ~ 'Class'
            )
        ))
    
    if (all(is.na(df2$starts_vowel))){next}
    
    DF <- Polychaeta[!is.na(df2$starts_vowel),seq(1:match(unique(na.omit(df2$starts_vowel)), colnames(Polychaeta)))]
    A7Clisti[i] <- list(DF)
    
  }
  
  daemi <- do.call(dplyr::bind_rows, A7Clisti)
  daemi <- daemi[!duplicated(daemi),]
  DT::datatable(daemi[,-c(1,2,3)])
}
