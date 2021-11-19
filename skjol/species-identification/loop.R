

library(httr)
library(rvest)
library(tibble)
#library(xml2)

URL <- "http://species-identification.org"
flokkun <- list()
# Fyrsti ormurinn í Macrobenthos of the North Sea - Polychaeta M.J. de Kluijver et al.:
 Ormur <- "species.php?species_group=macrobenthos_polychaeta&menuentry=soorten&id=402&tab=beschrijving"
#Skel: Ormur <- "species.php?species_group=mollusca&menuentry=soorten&id=513&tab=beschrijving" #i=506
#Ranaormar Ormur <- "species.php?species_group=macrobenthos_nemertina&id=39"
 
# þessi for-lúppa þarf að vera mötuð af hlekknum 'Ormur' til þess að byrja á skrefi 1 þar sem hún klippir og límir töflu af vefsíðunni í listann 'flokkun'. Í skrefi 2 sækir hún hlekkinnn á næsta orm fyrir skref 1 og svo koll af kolli þar til einhver leiðinda "encoding"-villa kemur upp en þá þarf að finna síðasta orminn handvirkt og reyna næsta  og næsta orm þar til lúppan kemst af stað aftur. Það er ekkert stopp á lúppunni en ormarnir eru um 500.
for (i in 1:200) {
  
  #1
  POST(paste(URL,Ormur,sep = "/")) %>%
    content(as = 'parsed', encoding = "Windows-1252") %>%
    html_element(xpath = ".//table[contains(., 'Kingdom')]") %>%
    html_text2() -> flokkun[i]
  
  #2
  Ormur <- POST(paste(URL,Ormur,sep = "/")) %>%
    read_html() %>%
    html_nodes('a')  %>%
    html_attr('href') %>%
    tail(1)
  
}


rass <- do.call(rbind, flokkun)
rass <- rass[!duplicated(rass),]
#write.csv(rass,"Polychaeta.csv")


###<>-<>-<>
###<>-<>-<>
###<>-<>-<>
###<>-<>-<>
###<>-<>-<>


# url <- "https://en.wikipedia.org/wiki/Taxonomic_rank"
# webpage <- read_html(url)
# tbls <- html_nodes(webpage, "table")
# df <- html_table(tbls[grep("Infratribe",tbls,ignore.case = T)],fill = T)[[1]]
# df$Rank <- gsub("([A-Za-z]+).*", "\\1", df$Rank)



ormar <- read.csv("skjol/species-identification/Polychaeta.csv")
ormar <- read.csv("skjol/species-identification/mollusca.csv")
ormar <- read.csv("skjol/species-identification/nemertea.csv")

rass <- list()
DF <- data.frame()
for (z in 1:dim(ormar)[1]) {
  x <- strsplit(ormar[z,2], "\n", fixed = TRUE)
  sellur <- sub('^\\w+', '', x[[1]])
  titlar <- sapply(strsplit(x[[1]], ' '), function(i)paste(i[-c(2:4)], collapse = ' '))
  DF <- data.frame(as.list(sellur))
  names(DF) <- titlar
  rass[z] <- list(DF)
}

DF <- data.frame(matrix(ncol = 13, nrow = 0))
dalkar <- c("Kingdom","Phylum","Subphylum","Class", "Subclass", "Superorder","Order","Suborder","Superfamily","Family","Subfamily","Genus","Species")
colnames(DF) <- dalkar
rass[z+1] <- list(DF)

rass <- rass[order(sapply(rass,ncol),decreasing = T)]
rass[[1]] <- rass[[1]] %>% mutate(across(where(is.logical), as.character))

Polychaeta <- do.call(dplyr::bind_rows, rass)
DT::datatable(Polychaeta[,-c(1,2,3)])


###Gögn fengin úr Náttúrufræðingnum: https://timarit.is/page/6780466#page/n82/mode/2up
###<>-<<>-<<>-<<>-<<>-<<>-<<>-<<>-<
###<>-<<>-<<>-<<>-<<>-<<>-<<>-<<>-<
library(data.table)
asjoc <- read.csv("skjol/species-identification/henda-1.csv" )
asjoc[,2] <- gsub("\\.|\\ sp|\\(p)|\\/.*","",asjoc[,2])

library(naniar)
library(tidyr)
asjoc <- asjoc %>% janitor::clean_names() %>%
  replace_with_na_all(condition = ~.x == "") %>%
  fill(fylking_flokkur_aettbalkur_phylum_class_order, .direction = "down")

ormanofn <- asjoc[asjoc$fylking_flokkur_aettbalkur_phylum_class_order=="Polychaeta",2] %>% na.omit()
ormanofn <- lapply(ormanofn$a_ett_tegund_family_species, trimws)
###<>-<<>-<<>-<<>-<<>-<<>-<<>-<<>-<
###<>-<<>-<<>-<<>-<<>-<<>-<<>-<<>-<



ormanofn <- read.csv("Kolgr2016/A7A/talning.csv")
ormanofn$Flokkun <- gsub("\\.|\\ sp|\\(p)|\\/.*","",ormanofn$Flokkun)
ormanofn <- as.list(ormanofn$Flokkun)


ormanofn <- read.csv("skjol/stodvar.csv",header = T,encoding = "UTF-8",row.names = NULL)
ormanofn <- as.list(ormanofn[,1])

library(dplyr)
DF <- data.frame()
A7Clisti <- list()

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
#ATH algengt: Glycera er bæði genus (Glycera) og species (Glycera gigantea). Þarf að splitta þessu upp þannig að ef tvö orð þá species annars case_when fyrir restina af dæminu.


