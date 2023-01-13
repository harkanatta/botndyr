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
  tala <- c()
  for (i in 1:dim(taff)[1]) {
    tala[i] <- max.col(!is.na(taff[i,]),'last')
    A[i] <- cbind(taff[i,], stodvar17[stodvar17$Flokkun==trimws(taff[i,tala[i]]),-1])
  }
  
  
  
  
  A <- list()
  for (i in 1:dim(taff)[1]) {
    A[[i]] <- cbind(taff[i,], stodvar17[stodvar17$Flokkun==trimws(taff[i,max.col(!is.na(taff[i,]),'last')]),-1])
  
  }
  
  
  
  B <- list()
  for (w in 1:length(Rank$Species)) {
  B[[w]] <- lengths(strsplit(trimws(Rank$Species[w]), "\\W+")) < 2
  }
  rass <- do.call(rbind,B[])
  table(rass)
  
  ress <- list()
  for (i in 957:958) {
    print(Rank[i,])
  ress[[i]] <- Rank %>% 
    mutate(Species = ifelse(lengths(strsplit(trimws(Rank$Species[i]), "\\W+")) < 2,
                            paste(trimws(Rank$Genus[i]),trimws(Rank$Species[i])),
                            NA))
  }
  
  
  
  
  xess <- Rank[957:958,]
  ress <- list()
  for (i in 1:length(xess$Species)) {
    print(xess[i,])
    ress[[i]] <- xess %>% 
      mutate(Species = ifelse(lengths(strsplit(trimws(xess$Species[i]), "\\W+")) < 2,
                              paste(trimws(xess$Genus[i]),trimws(xess$Species[i])),
                              NA))
  }
  
  
  xess <- Rank[957:958,]
  ress <- list()
  for (i in 1:length(xess$Species)) {
    
    ress[[i]] <- xess %>% 
      mutate(Species = ifelse(lengths(strsplit(trimws(xess$Species[i]), "\\W+")) < 2,
                              paste(trimws(xess$Genus[i]),trimws(xess$Species[i])),
                              xess$Species[i]))
  }
  
  B <- do.call(cbind, ress)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(benthos)
  library(tidyverse)
  library(plyr)
 
  
  
  
  
  
  
  
  dataPath <- here::here("Kolgr2016")
  datafiles <- list.files(path=dataPath, pattern = "csv", full.names = TRUE, recursive = T)
  datafiles <- datafiles[-19]
  classes <- c("character","integer", "factor", "character")
  
  # build function to load data
  load_data <- function(dataPath, classes) { 
    tables <- lapply(datafiles, read.csv, colClasses=classes, na.strings=c("NA", ""))
    names(tables) <- substr(dirname( datafiles ),57,59)
    data.table::rbindlist(tables, idcol = "id" )
  }
  
  data <- load_data(dataPath, classes)
  
  data$stod <- substr(data$id,1,2)
  data$skipting <- gsub("¼|1/4|0.25",4,data$skipting) # laga misræmi í skráningu á súbbuninni
  data$Flokkun <- sapply(data$Flokkun, function(x) gsub("\\.|\\ sp|\\(p)|\\/.*","",x)) #Laga heitin. Eykur benthos::is_accepted(taxon = Flokkun) úr 432 í 522
  data$Flokkun <- str_to_sentence(data$Flokkun) #Stór stafur í byrjun heitis
  data <- data[!is.na(data$N) & !is.na(data$Flokkun) & data$N!="NA",]

  #ath með að skipta út Terebellinae fyrir Terebellidae því það er is_accepted(taxon="Terebellidae") == TRUE
  
  remove_list <- paste(c("—Ssekkt",
                         "Foraminifera",
                         "Götungar",
                         "Harpacticoida",
                         "Hydrozoa",
                         "Lirfur",
                         "Skordýr",
                         "Ranaormur",
                         "Lirfurogdrasl",
                         "Bandormur",
                         "Lirfa",
                         "Skeljar",
                         "Óþekkt",
                         "Rækjulirfa",
                         "Nematoda",
                         "Nemertea",
                         "Ostracoda"
                         ), collapse = '|')
  
  
  remove_ind <- lapply(strsplit(remove_list , "\\|")[[1]] , \(x) grep(x , data$Flokkun , fixed = T)) |> 
    unlist() |> 
    unique()
  data <- data[-remove_ind,]
  
  ##Shannon 
  shannon2016 <- lapply(split(data,data$stod,drop=T ), \(x) shannon(taxon = x$Flokkun, count = x$N))
  unlist(shannon2016)
  plot(unlist(shannon2016))
  
  ggplot(df2, aes(x=dose, y=len, group=supp, color=supp)) + 
    geom_pointrange(aes(ymin=len-sd, ymax=len+sd))
  
  
  library(plyr)
  library(benthos)
  library(tidyverse)
  #rass <- ddply(data,.(id,Flokkun),summarize, N=sum(N)) %>% 
        mutate(COMPLIANT = is_accepted(taxon = Flokkun)) 
  
  #write.csv(rass[rass$COMPLIANT=="FALSE",],"rass.csv",row.names = F)
  
  
  
  
  
  
  
  #gogn <- readr::read_csv(here::here('Kolgr2016/E4B/talning.csv')) %>% 
  gogn <- as.data.frame(data) %>% 
    ddply(.(Flokkun,id,N,skipting,stod),summarize, Artal=2016, Nu=sum(N*as.integer(skipting))) #%>% 
    #drop_na(N) %>% 
    #mutate(klass_id=wm_name2id(name = Flokkun)) %>% 
    #mutate(HAS_GROUP = has_ambi(taxon = Flokkun))
 
    
  gogn %>%
    mutate(HAS_GROUP = has_ambi(taxon = Flokkun))
  
  gogn %>%
    mutate(HAS_GROUP = has_ambi(taxon = Flokkun)) %>%
    summarise(percentage = 100 * sum(N[!HAS_GROUP]) / sum(N)) %>%
    as.numeric
  
  gogn %>% 
    iti(taxon = Flokkun, count = N)
  
  gogn %>%
    mutate(HAS_GROUP = has_iti(taxon = Flokkun))
  
  gogn %>% 
    group_by(stod) %>% 
    summarise(
      Nn = total_abundance(count = N),
      S = species_richness(taxon = Flokkun, count = N),
      D = margalef(taxon = Flokkun, count = N),
      SN = rygg(taxon = Flokkun, count = N),
      SNa = rygg(taxon = Flokkun, count = N, adjusted = TRUE),
      H = shannon(taxon = Flokkun, count = N),
      AMBI=ambi(taxon = Flokkun, count = N)
    )
  
  
  
  tegundalisti <- ddply(gogn,.(Flokkun),summarize, N=sum(N))
  DT::datatable(tegundalisti)
  
  AMBI2016 <- DF[DF$A %in% gogn$Flokkun,]
  
  AlltAMBI <- DF[DF$A %in% allartalningar$Flokkun,]
  
  
  
  
  
  
  #### 2ö14
  
  t2014 <- read.csv(here::here("Kolgr2014",list.files(here::here("Kolgr2014"))),encoding="latin1") %>%
    pivot_longer(!Flokkun, names_to = "dolla", values_to = "N") %>% 
    na.omit(N) %>% 
    mutate(skipting = substr(dolla, nchar(dolla), nchar(dolla)),
           skipting =as.numeric(skipting),
           skipting =ifelse(is.na(skipting),1,skipting*1),
           dolla = substr(dolla,1,3),
           stod = substr(dolla,1,2),
           Artal = 2014,
           Nu = N*skipting) %>%
    rename(id = dolla)
  
  
  t2014 %>% 
    group_by(stod) %>% 
    summarise(
      Nn = total_abundance(count = N),
      S = species_richness(taxon = Flokkun, count = N),
      D = margalef(taxon = Flokkun, count = N),
      SN = rygg(taxon = Flokkun, count = N),
      SNa = rygg(taxon = Flokkun, count = N, adjusted = TRUE),
      H = shannon(taxon = Flokkun, count = N),
      AMBI=ambi(taxon = Flokkun, count = N)
    )
  

  
  
  #### 2013
  
  t2013 <- read.csv(here::here("Kolgr2013",list.files(here::here("Kolgr2013"), pattern = "csv")),encoding="latin1")[ ,-c(1,2)] %>%
    pivot_longer(!Flokkun, names_to = "dolla", values_to = "N") %>% 
    na.omit(N) %>% 
    mutate(skipting = substr(dolla, nchar(dolla), nchar(dolla))) %>% 
    mutate(skipting = case_when(grepl("[[:alpha:]]",skipting) ~ 1,
                                TRUE ~ as.numeric(as.character(skipting))),
           dolla = substr(dolla,1,3),
           stod = substr(dolla,1,2),
           Artal = 2013,
           Nu = N*skipting) %>%
    rename("id" = dolla)
  
  t2013 <- t2013[t2013$stod != "U1" & t2013$stod != "U2",] #Utanbrúarstöðvarnar teknar út 
  
  t2013 %>% 
    group_by(stod) %>% 
    summarise(
      Nn = total_abundance(count = N),
      S = species_richness(taxon = Flokkun, count = N),
      D = margalef(taxon = Flokkun, count = N),
      SN = rygg(taxon = Flokkun, count = N),
      SNa = rygg(taxon = Flokkun, count = N, adjusted = TRUE),
      H = shannon(taxon = Flokkun, count = N),
      AMBI=ambi(taxon = Flokkun, count = N)
    )
  
    
  #### 2015
  
  t2015 <- read.csv(here::here("Kolgr2015",list.files(here::here("Kolgr2015"), pattern = "csv")),encoding="latin1") %>%
    pivot_longer(!Flokkun, names_to = "dolla", values_to = "N") %>% 
    na.omit(N) %>% 
    mutate(skipting = substr(dolla, nchar(dolla), nchar(dolla)),
           skipting =as.numeric(skipting),
           skipting =ifelse(is.na(skipting),1,skipting*1),
           dolla = substr(dolla,1,3),
           stod = substr(dolla,1,2),
           Artal = 2015,
           Nu = N*skipting) %>%
    rename(id = dolla)
  
  
  t2015 %>% 
    group_by(stod) %>% 
    summarise(
      Nn = total_abundance(count = N),
      S = species_richness(taxon = Flokkun, count = N),
      D = margalef(taxon = Flokkun, count = N),
      SN = rygg(taxon = Flokkun, count = N),
      SNa = rygg(taxon = Flokkun, count = N, adjusted = TRUE),
      H = shannon(taxon = Flokkun, count = N),
      AMBI=ambi(taxon = Flokkun, count = N)
    )
  
  
  #### 2017
  
  t2017 <- read.csv(here::here("Kolgr2017",list.files(here::here("Kolgr2017"), pattern = "csv")),encoding="latin1") %>%
    pivot_longer(!Flokkun, names_to = "dolla", values_to = "N") %>% 
    na.omit(N) %>% 
    mutate(skipting = substr(dolla, nchar(dolla), nchar(dolla)),
           skipting =as.numeric(skipting),
           skipting =ifelse(is.na(skipting),1,skipting*1),
           dolla = substr(dolla,1,3),
           stod = substr(dolla,1,2),
           Artal = 2017,
           Nu = N*skipting) %>%
    rename(id = dolla)
  
  
  t2017 %>% 
    group_by(stod) %>% 
    summarise(
      Nn = total_abundance(count = N),
      S = species_richness(taxon = Flokkun, count = N),
      D = margalef(taxon = Flokkun, count = N),
      SN = rygg(taxon = Flokkun, count = N),
      SNa = rygg(taxon = Flokkun, count = N, adjusted = TRUE),
      H = shannon(taxon = Flokkun, count = N),
      AMBI=ambi(taxon = Flokkun, count = N)
    )
  
  
  
  ########## Heild
  
  allartalningar <- rbind(t2013,t2014,t2015,gogn,t2017)
  
  remove_list <- paste(c("—Ssekkt",
                         "Foraminifera",
                         "Götungar",
                         "Harpacticoida",
                         "Hydrozoa",
                         "Lirfur",
                         "Skordýr",
                         "Ranaormur",
                         "Lirfurogdrasl",
                         "Bandormur",
                         "Lirfa",
                         "Skeljar",
                         "Óþekkt",
                         "Rækjulirfa",
                         "Nematoda",
                         "Ostracoda",
                         "ungv.",
                         "Copepoda sp",
                         "Möttuldýr",
                         "Möttuldýr TUNICATA EÐA FLEIRI?",
                         "Óþekkt",
                         "Ranaormur",
                         "Skeljar",
                         "Skordýr"
                         
                         
  ), collapse = '|')
  
  # annað:  
  allartalningar$Flokkun <- lapply(allartalningar$Flokkun, function(x) trimws(x))
  allartalningar$Flokkun <- sapply(allartalningar$Flokkun, function(x) gsub("\\ kemur líka Heteromastus filiformis|\\ m.|\\ Sipunculidea/|\\.|\\ TUNICATA EÐA FLEIRI?|\\ lirfur|\\ nýsestir|\\ ungv|\\ juv|\\ sp|\\(p)|\\/.*","",x))
  kemur líka Heteromastus filiformis
  nýsestir
  m.
  ungv.
  
  remove_ind <- lapply(strsplit(remove_list , "\\|")[[1]] , \(x) grep(x , allartalningar$Flokkun , fixed = T)) |> 
    unlist() |> 
    unique()
  allartalningar <- allartalningar[-remove_ind,]
  
  
  
  
  
  tafla <- geggjad %>% 
    filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
    ddply(.(Artal, stod),summarise, Nn = total_abundance(count = N),
          S = species_richness(taxon = Flokkun, count = N),
          D = margalef(taxon = Flokkun, count = N),
          SN = rygg(taxon = Flokkun, count = N),
          SNa = rygg(taxon = Flokkun, count = N, adjusted = TRUE),
          H = shannon(taxon = Flokkun, count = N),
          AMBI=ambi(taxon = Flokkun, count = N)) %>% 
    pivot_longer(!c(Artal,stod,Nn),names_to = "index", values_to = "Skor")

  
  
  
  ###myndir
  #litir <- colorRampPalette(c('#045579', 'white','#d75f07','seashell','#069acc'))
  litir <-  colorRampPalette(c("red","blue"),bias=.1,space="rgb")
  tafla %>% 
    ggplot(aes(x = Artal, y = Nn)) +
    geom_bar(aes(fill = stod), stat = "identity", color="black", size =1,position="dodge")  +
    xlab("") + labs(fill = "", title = "Landanir í Húnaflóa 2022", caption = "(Gögn fengin af vef Fiskistofu (fiskistofa.is))") +
    theme_minimal() +
    scale_fill_manual(values = litir(12)) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "", suffix = " \nTonn"))

  
  ggplot(tafla,                    # Einfaldir punktar með línum
         aes(x = Artal,
             y = score,
             col = stod)) +
    geom_line() +
    theme_classic() +
    theme(strip.background = element_blank()) +
    ggh4x::facet_grid2( ~ index, scales = "free_y", independent = "y") +
    geom_point(colour="black", shape=21, size = 4,
               aes(fill=stod)) +
    labs(title = "Fjölbreytileikastuðlar", caption = "(Botndýr í Kolgrafafirði 2013-2017)")
  
  
  
  
  

  B <- list()
  for (i in unique(DF$Flokkun)) {
  A[i] <- try(wm_name2id(name = i) )
  B[[i]] <- tidyr::nest(wm_classification(A[[1]]))
  }
  B
  B[[1]][[1]]
  
    DF %>% nest(DF =wm_name2id(name = "acarina") %>% 
                   wm_classification())
  
#  APHIA ID <–> name
  wm_name2id(name = "Acarina") %>% 
    wm_classification()
  #  APHIA ID <–> name
  wm_name2id(name = "Platanista gangetica") %>% 
    wm_classification()
  
  
  
  ifelse(is.integer(try(wm_name2id(name = "Platanista+gangetica")), ) 
  
  
         DF <- df #%>% nest(data = c(id,N,Artal,skipting,stod,Nu))
         DF$Flokkun <- sapply(DF$Flokkun, function(x) gsub("\\ Sipunculidea/|\\.|\\ TUNICATA EÐA FLEIRI?|\\ lirfur|\\ nýsestir|\\ ungv|\\ sp|\\(p)|\\/.*","",x)) #Laga heitin. Eykur benthos::is_accepted(taxon = Flokkun) úr 432 í 522
         
         A <- c()
        
         for (i in unique(DF$Flokkun)) {
           A[i] <- try(wm_name2id(name = i) )
         }
         A <- as.data.frame(A)
         A$flokkun <- row.names(A)
         
         DF <- merge(DF,A, by.x="Flokkun", by.y="flokkun")
         DF$worms <- as.integer(as.character(DF$A))
         DFekkina <- DF[!is.na(DF$worms),]
         B <- list()
         for (i in 1:length(DFekkina$worms)) {print(i)
           B[[i]] <-  wm_classification(DFekkina$worms[i])
         }
         df_list <- lapply(1:length(B), 
                           function(x) (pivot_wider(B[[x]][-1], names_from = rank, values_from = scientificname)))
         
         rass <- bind_rows(df_list)
         #print(rass,n=50)
         geggjad <- cbind(rass,DFekkina)
         
         
         
         
         DF[is.na(DF$worms),]
         
         
         
         
         
         
         
gogn <- geggjad %>% 
 filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
 mutate(Artal = factor(Artal)) %>% 
 ddply(.(Artal,stod,Flokkun),summarise, N=sum(Nu)) %>% 
 pivot_wider(names_from = stod, values_from = N)

BBIlisti <- list()
BBIastand <- list()
for (i in unique(gogn$Artal)) {
  my_BBI <- gogn %>% filter(Artal %in% c(i)) %>%
    select(-Artal) %>%
    BBI()
# calculating nEQR values and ecological quality status
BBIlisti[[i]] <- as.data.frame(cbind(my_BBI$BBI, Artal=i))
BBIastand[[i]] <- my_BBI$BBIclass
#my_nEQR <- nEQR(my_BBI$BBI) 
}










p <- ggplot()
for (i in 1:5) p <- p + geom_point(data=BBIlisti[[i]], aes(Artal,AMBI))
p
#




pl <- Reduce(f = function(p, d) p + geom_point(data=d, aes(AMBI,Artal)), 
             x = BBIlisti, init = ggplot(), accumulate = TRUE)

p <- ggplot()

plot <- function(df){
  p <- p + geom_line(data=df, aes(AMBI,Artal))
}

lapply(BBIlisti, plot)

p

rass <- do.call(rbind,nEQR)
mm <- as.matrix(rass, ncol = 7)

heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = mm, notecol = "black", notecex = 2,
          trace = "none", key = FALSE, margins = c(7, 11))
