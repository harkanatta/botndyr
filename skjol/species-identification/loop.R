

library(httr)
library(rvest)
library(tibble)
#library(xml2)

URL <- "http://species-identification.org"
flokkun <- list()
# Fyrsti ormurinn í Macrobenthos of the North Sea - Polychaeta M.J. de Kluijver et al.:
 Ormur <- "species.php?species_group=macrobenthos_polychaeta&menuentry=soorten&id=402&tab=beschrijving"

 # Krabbi Ormur <- "species.php?species_group=crustacea&menuentry=soorten&id=8&tab=classificatie" #i=644
 # Skel: Ormur <- "species.php?species_group=mollusca&menuentry=soorten&id=513&tab=beschrijving" #i=506
 # Ranaormar (eða hvað sem þetta er) Ormur <- "species.php?species_group=macrobenthos_nemertina&id=39"
 # flatormur (Platyhelminthes) Ormur <- "/species.php?species_group=zmns&menuentry=soorten&id=263&tab=classificatie"
 
# þessi for-lúppa þarf að vera mötuð af hlekknum 'Ormur' til þess að byrja á skrefi 1 þar sem hún klippir og límir töflu af vefsíðunni í listann 'flokkun'. Í skrefi 2 sækir hún hlekkinnn á næsta orm fyrir skref 1 og svo koll af kolli þar til einhver leiðinda "encoding"-villa kemur upp en þá þarf að finna síðasta orminn handvirkt og reyna næsta  og næsta orm þar til lúppan kemst af stað aftur. Það er ekkert stopp á lúppunni en ormarnir eru um 500.
for (i in 1:3) {
  
  #1
  POST(paste(URL,Ormur,sep = "/")) %>%
    content(as = 'parsed', encoding = "Windows-1252") %>%
    html_element(xpath = ".//table[contains(., 'Phylum')]") %>%
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
#write.csv(rass,"Platyhelminthes.csv")


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




### Lesa inn skjölin og gera data frame 'Rank'
ormar <- read.csv("skjol/species-identification/Polychaeta.csv")
ormarA <- read.csv("skjol/species-identification/mollusca.csv")
ormarB <- read.csv("skjol/species-identification/nemertea.csv")
ormarC <- read.csv("skjol/species-identification/Arthropoda.csv")
ormar <- (c(unname(ormar)[,2],unname(ormarA)[,2],unname(ormarB)[,2],unname(ormarC)[,2])) 

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
Rank <- unique(Rank)
write.csv(Rank,"skjol/species-identification/Rank.csv", row.names = F)
