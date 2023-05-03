# Aggregate data to sum 'N' values for duplicate rows
data1_agg <- jorundur %>% group_by(Artal, stod, Flokkun) %>% summarise(N = sum(N))

# Prepare data for ordination
data1_wide <- data1_agg %>% spread(key = Flokkun, value = N, fill = 0)

# setja inn dýpið
new_env_data <- botngerd[,1:3] 
colnames(new_env_data) <- c("stod", "dypi", "Síld 2013")
gledipillan_dypi <- merge(gledipillan, new_env_data, by = "stod")


merged_data <- merge(data1_wide, gledipillan_dypi, by = c("Artal", "stod")) # Hér dettur 1999 út

rownames(merged_data) <- paste(merged_data$Artal, merged_data$stod, sep = "_")
merged_data <- merged_data[, -c(1,2)] # Remove the 'Year' and 'Station' columns

# Separate environmental and species data
env_data <- merged_data[, c("20um","63um","125um","250um","1000um","tap","sd","dypi", "Síld 2013")]
species_data <- merged_data[, !(names(merged_data) %in% c("20um","63um","125um","250um","1000um","tap","sd","dypi", "Síld 2013"))]

#species_data <- log(species_data + 1)


# Perform RDA
rda_result <- rda(species_data ~ ., data = env_data)

# Plot the ordination
plot(rda_result)

# Add points with the labels
text(rda_result, display = "sites", col = "blue")

# Perform permutation test
rda_perm <- anova.cca(rda_result, by = "terms", permutations = 999)
print(rda_perm)


# Perform CA
ca_result <- cca(species_data)

# Perform DCA
dca_result <- decorana(species_data)


# Plot CA results
plot(ca_result)

# Add points with the labels
text(ca_result, display = "sites", col = "blue")

# Plot DCA results
plot(dca_result)

# Add points with the labels
text(dca_result, display = "sites", col = "blue")










library(vegan)

data_mat <- jorundur[, c("Flokkun", "stod", "N")]
data_mat_wide <-data.table::dcast(data_mat, Flokkun ~ stod, value.var = "N")
rownames(data_mat_wide) <- data_mat_wide[, 1]
data_mat_wide <- data_mat_wide[, -1]
data_mat_wide <- decostand(data_mat_wide, method = "hellinger")
my_pca <- rda(data_mat_wide)
eigenvals <- my_pca$CA$eig
eigenvals

diversity(data_mat_wide, index = "simpson")
diversity(data_mat_wide, index = "shannon")

nmds <- metaMDS(data_mat_wide)
stressplot(nmds)
gof <- goodness(nmds)
plot(nmds, display = "sites", type = "n")
points(nmds, display = "spec", cex = 2*gof/mean(gof)) "sites og spec öfugt"
text(nmds, display = "spec", cex = .7)

DCA <- decorana(data_mat_wide)
ordiplot (DCA, display = 'sp', type = 'n')
orditorp (DCA, display = 'sp')

plot(nmds)
plot(deco)
text(deco, display = "sites", cex=0.7, col="blue")
text(deco, display = "spec", cex=1, col="red")


dist_matrix <- vegdist(jorundur[,4], method = "bray")
adonis_result <- adonis2(dist_matrix ~ Artal, data = jorundur)
adonis_result$aov.tab #nota þetta (p<o.oo1)


bp <- boxplot.stats(jorundur$N)
outliers <- bp$out
# Remove outliers
data <- jorundur[!jorundur$N %in% outliers,]
# Create a new box plot without outliers
boxplot(data$N)
bp <- boxplot.stats(data$N)
outliers <- bp$out
# Remove outliers
data <- data[!data$N %in% outliers,]
# Create a new box plot without outliers
boxplot(data$N)


data_mat <- data[, c("Flokkun", "Artal", "N")]
data_mat_wide <-data.table::dcast(data_mat, Flokkun ~ Artal, value.var = "N")
rownames(data_mat_wide) <- data_mat_wide[, 1]
data_mat_wide <- data_mat_wide[, -1]
data_mat_wide <- decostand(data_mat_wide, method = "hellinger")

nmds <- metaMDS(data_mat_wide)
stressplot(nmds)
gof <- goodness(nmds)
plot(nmds, display = "sites", type = "n")
points(nmds, display = "spec", cex = 2*gof/mean(gof))
text(nmds, display = "spec", cex = .7)

#
my_pca <- rda(data_mat_wide)
eigenvals <- my_pca$CA$eig
eigenvals # Fyrstu 2 útskýra bara 37%
plot(my_pca, type = "n")
points(my_pca, display = "sites")
text(my_pca, display = "species")






henda <- read.csv(here::here("listifyrirord.csv"))


jorundur <- df %>% 
  filter(!Flokkun %in% c("harpacticoida", "Campanulariidae")) %>% 
  mutate(
    Flokkun = case_when(
      for (i in 1:length(henda$New_Column)) {
        henda$New_Column[i]
        TRUE ~ Flokkun
      }
    )
  ) %>%
  drop_na()
  
  
#hafa hópa í ordination
#hafa top tíu tegundir sem sýna mesta breytileika
# Taka Capitata út og fleira
# byrja á því að plotta stöðvarnar og sjá svo hvort það sé hægt að skýra það út frá einhverjum umhverfisbreytum
# Senda á Jörund nýja töflu
# Taka kvaðratrót eða log af tegundunum
# frávik milli ára 
# gera hclust milli ára
# Athuga með gögn um súrefnisstyrk
#

Gera unconstrained, unimodal ordination, CA eða DCA.
 * Unimodal vegna þess að fyrsti DCA-ásinn eftir decorana() er >4
 * Unconstrained vegna þess að umhverfisbreytur eru notaðar til skýringa eftir á en ekki partur af tilraun.

DCA <- decorana (veg = log1p (veganjorundur))
ordiplot (DCA, display = 'sp', type = 'n')
orditorp (DCA, display = 'sp')





gledi <- structure(list(ar = c(2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 
                               2014L, 2014L, 2014L, 2014L, 2014L, 2014L, 2015L, 2015L, 2015L, 
                               2015L, 2015L, 2015L, 2016L, 2016L, 2016L, 2016L, 2016L, 2016L, 
                               2017L, 2017L, 2017L, 2017L, 2017L, 2017L), stod = structure(c(1L, 
                                                                                             2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 
                                                                                             6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L), levels = c("A7", 
                                                                                                                                                             "B5", "B8", "C4", "E3", "E4"), class = "factor"), tap = c(0.1815154141, 
                                                                                                                                                                                                                       0.174146595, 0.1785031086, 0.1427878082, 0.2154126214, 0.1351968124, 
                                                                                                                                                                                                                       0.117573295, 0.1191274479, 0.1294084997, 0.1130069238, 0.1010060354, 
                                                                                                                                                                                                                       0.1262329745, 0.1188653545, 0.1244070209, 0.1171794534, 0.1322903875, 
                                                                                                                                                                                                                       0.09040251879, 0.1039239106, 0.108551758, 0.1051986583, 0.1112451918, 
                                                                                                                                                                                                                       0.09691066013, 0.08857151425, 0.09206586826, 0.1442154807, 0.115848718, 
                                                                                                                                                                                                                       0.1163703415, 0.1022058277, 0.0796504565, 0.1015258137), sd = c(0.00544534379, 
                                                                                                                                                                                                                                                                                       0.002450552005, 0.01099042104, 0.01990496666, 0.1278627553, 0.01183349431, 
                                                                                                                                                                                                                                                                                       0.0009760592088, 0.0003483618316, 0.0006002430173, 0.0003497066178, 
                                                                                                                                                                                                                                                                                       0.0001190101187, 0.0004495699774, 0.001056751767, 0.0008385991238, 
                                                                                                                                                                                                                                                                                       0.002776269384, 0.003141179178, 0.002264629356, 0.0002854253291, 
                                                                                                                                                                                                                                                                                       0.001290991906, 0.001823914493, 0.0004200217485, 0.00021574591, 
                                                                                                                                                                                                                                                                                       0.001326007864, 0.002469933866, 0.01028086287, 0.0004952558501, 
                                                                                                                                                                                                                                                                                       0.001474450338, 0.0004351899832, 0.002435406591, 0.0008149949169
                                                                                                                                                                                                                       )), row.names = c(NA, -30L), class = "data.frame")

gledi <- gledi[!gledi$sd == max(gledi$sd),]
sapply(split(gledi,gledi$stod), function(x) mean(x$tap))


botngerd <- structure(list(Stod = structure(c(1L, 3L, 4L, 5L, 6L, 7L, 1L, 
                                              3L, 4L, 5L, 6L, 7L), levels = c("A7", "B0", "B5", "B8", "C4", 
                                                                              "E3", "E4", "U1", "U2"), class = "factor"), Dypi = structure(c(5L, 
                                                                                                                                             8L, 4L, 9L, 2L, 1L, 5L, 8L, 4L, 9L, 2L, 1L), levels = c("10.2", 
                                                                                                                                                                                                     "12", "14.3", "14.9", "15.1", "2.3", "2.9", "23.5", "33.1"), class = "factor"), 
                           Botngerd = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                  1L, 1L, 1L), levels = c("Ledja", "Mol"), class = "factor"), 
                           Sild = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L
                           ), Artal = c(1999L, 1999L, 1999L, 1999L, 1999L, 1999L, 2013L, 
                                        2013L, 2013L, 2013L, 2013L, 2013L)), row.names = c(1L, 3L, 
                                                                                           4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L, 14L), class = "data.frame")

rass <- botngerd[7:12,-5]









##2016
dataPath <- here::here("Kolgr2016")
datafiles <- list.files(path=dataPath, pattern = "csv", full.names = TRUE, recursive = T)
#datafiles <- datafiles[-19] 
classes <- c("character","integer", "factor", "character")

for (i in 1:2) {tables[i] <- lapply(datafiles[i], read.csv, colClasses=classes, na.strings=c("NA", ""))}


load_data <- function(dataPath, classes) { 
  tables <- lapply(datafiles, read.csv, colClasses=classes, na.strings=c("NA", ""))
  names(tables) <- substr(dirname( datafiles ),56,58)
  data.table::rbindlist(tables, idcol = "id" )
}

data <- load_data(dataPath, classes)

data$stod <- substr(data$id,1,2)
data$skipting <- gsub("¼|1/4|0.25",4,data$skipting) %>% 
  as.numeric() # laga misræmi í skráningu á súbbuninni
#data$Flokkun <- sapply(data$Flokkun, function(x) gsub("\\.|\\ sp|\\(p)|\\/.*","",x)) #Laga heitin. Eykur benthos::is_accepted(taxon = Flokkun) úr 432 í 522
#data$Flokkun <- str_to_sentence(data$Flokkun) #Stór stafur í byrjun heitis
data <- data[!is.na(data$N) & !is.na(data$Flokkun) & data$N!="NA",]  
gogn <- as.data.frame(data) %>% 
  ddply(.(Flokkun,id,N,skipting,stod),summarize, Artal=2016, Nu=sum(N*skipting)) %>% 
  select( Flokkun,id,N,Artal,skipting,stod,Nu)












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
  
  
<<<<<<< HEAD
  MedAmbi <- gogn %>%
=======
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
>>>>>>> dc1a0a0fd3705911a60dcba78ad7c842e50391b0
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
      Nu = total_abundance(count = alls),
      S = species_richness(taxon = Flokkun, count = alls),
      D = margalef(taxon = Flokkun, count = alls),
      SN = rygg(taxon = Flokkun, count = alls),
      SNa = rygg(taxon = Flokkun, count = alls, adjusted = TRUE),
      H = shannon(taxon = Flokkun, count = alls),
      AMBI=ambi(taxon = Flokkun, count = alls)
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
  
<<<<<<< HEAD
  PT$skipting <- gsub("¼|1/4|0.25",4,PT$skipting)
  PT <- PT[!is.na(N),]
  PT$alls <- PT$N*as.integer(PT$skipting)
  DT::datatable(PT)
=======
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
<<<<<<< HEAD

















load("./meistaraverkefni/meistaraverkefni/úrvinnsla/allt13.Rda") #sa
logi<-sapply(sa,as.logical)
fjoldteg99<-c(c(20,1,29,33,30,42,28),"","")
fjoldteg13<-rowSums(logi)
fjol<-rbind(fjoldteg99,fjoldteg13)
fjol<-apply(fjol,2,as.numeric)
rownames(fjol)<-c("1999","2013")
colnames(fjol)<-c("A7","B0","B5","B8","C4","E3","E4","U1","U2")


list.dirs("./meistaraverkefni/", pattern="csv") #sa




agnar<-read_csv("./meistaraverkefni/meistaraverkefni/taflaAgnar.csv" ) 
agnar<-filter(agnar,rowSums(agnar[,2:7], na.rm = TRUE)!= 0) %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(cols = !Flokkun, names_to = 'stod', values_to = 'N') %>% 
  mutate(Artal=1999, skipting=1, id=stod,Nu=N) %>% 
  select(Flokkun,id,N,Artal,skipting,stod,Nu)
  
  
  filter(rowSums() != 0)

agnar[rowSums(agnar)!=0,]





library(PDE)


f <- file.path("./skjol/lesefni/tafla.pdf")
area <- tabulizer::locate_areas(f)
tafla <- tabulizer::extract_tables(f)

  as.data.frame() %>%
  janitor::row_to_names(row_number = 1) 

# Taflan í greininni (mynd 2) er höfð á tveggja dálka formi, svo hún sé breiðari því þá er greinin læsilegri. Svona lagað skapar örlítil vandamál. Tveir "data frames" gerðir til að leysa það. 

one <- dfs[,1:5 ]
two <- dfs[,6:10 ]

# Þeir eru svo bundnir saman en dálkurinn T er færður inn handvirkt því mínusmerkið var eitthvað einkennilegt sem gerði mér ekki kleift að breyta þessu úr "character" yfir í "double" eða frá texta yfir í tölur.

dfs <- bind_rows(one, two) %>%
  slice(-c(1, 24, 46)) %>% 
  readr::type_convert() %>% 
  mutate(T=as.double(c("6.98","6.93","6.98","5.89","6.82","6.35","5.93","6.16","1.18","5.95","5.34","0.43","0.09","5.66","2.56","0.21","5.14","2.32","0.09","4.75","4.04","2.98","1.45","-0.22","4.05","2.32","-0.16","3.88","2.55","-0.46","4.28","2.29","-0.19","7.24","7.53","6.74","7.49","7.04","6.74","7.87","7.62","6.86","7.73"))) %>% 
  ddply(.(Current),summarize,T=mean(T))


Aggi<-read.csv2("./meistaraverkefni/meistaraverkefni/teglisti99allt.csv")
Aggi[is.na(Aggi)] <- 0
tAggi<-t(Aggi[-c(1,4,6,7,10:12),2:31]) 
Agnar<-matrix(unname(tAggi),nrow=30,byrow=TRUE,dimnames=list(row.names(tAggi),Aggi$X[-c(1,4,6,7,10:12)]))

library(tidyverse)
library(magick)
library(tesseract)


mynd <- file.path("./skjol/lesefni/tafla.html/page1.png")
raw_img <- image_read(f)
raw_img %>% 
  image_crop(geometry_area(1200, 1600, 110, 190)) %>%  
  image_quantize(colorspace = 'gray') %>%
  image_transparent("white", fuzz=22) %>% 
  image_background("white") %>%
  image_threshold() %>% 
  #image_ggplot()
  ocr()





scrape_fun <- function(url_in, crop_left, crop_top){
  raw_img <- image_read(url_in) %>% 
    image_quantize(colorspace = 'gray') %>%
    image_transparent("white", fuzz=22) %>% 
    image_background("white") %>%
    image_threshold() %>% 
    image_crop(geometry_area(0, 0, crop_left, crop_top)) 
  
  image_ocr(raw_img) %>% 
    str_c() %>% 
    str_split(pattern = "\n") %>% 
    unlist() %>%
    tibble(data = .) %>% 
    filter(str_length(data) >= 2) %>% 
    separate(
      data, 
      into = c("player", "position", "team", "sacks"), 
      sep = c(" \\(| - |\\) ")
    ) %>% 
    mutate(sacks = as.double(sacks)) %>% 
    mutate(sacks = if_else(sacks >= 20, sacks/10, sacks))
}




gogn <- geggjad %>% 
  filter(stod %in% c("A7", "B5", "B8","C4", "E3", "E4")) %>% 
  mutate(Artal = factor(Artal)) %>% 
  ddply(.(Artal,stod, Flokkun),summarise, N=sum(Nu)) %>% 
  #mutate(N = case_when(Artal != 1999 ~ round(N/(3*0.04)),
  #                      TRUE ~ round(N/0.0225))) %>% 
  pivot_wider(names_from = c(stod,Artal), values_from = N) %>% 
  arrange(Flokkun)

write_csv(gogn,"ArStodFjfermetri.csv", na="")





f <- file.path("./skjol/lesefni/tafla.pdf")



img <- image_read("./skjol/lesefni/tafla.jpg") %>%
  image_morphology(method = "Thinning", kernel = "Rectangle:30x1+0+0^<") #%>%
  #image_negate() #%>%
  #image_ocr()

img









# byrja á að deila með flatartaki
tegArtal <- ddply(KolgrTaxa,.(Artal,Flokkun),summarise, N=sum(N)) %>% pivot_wider(names_from = Artal, values_from = N)

df <- ddply(KolgrTaxa,.(Flokkun,Artal,stod), summarise, Fjm=sum(Nfm)) %>% 
  filter(stod != U1 & stod != U2 & stod != B0)
  pivot_wider(names_from = c(stod,Artal), values_from = Fjm)
  
  
  KolgrTaxa <- read_csv("KolgrTaxa.csv", na = "empty") %>% 
    mutate(Nfm= case_when(
      Artal != 1999 ~ round(Nu/(3*0.04)),
      TRUE ~ round(Nu/0.0225)
    ))
  
  df <- KolgrTaxa %>% 
    mutate(Artal = factor(Artal)) %>% 
    filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3") & Artal==1999) %>% 
    ddply(.(Phylum, Class, Flokkun, stod),summarise, Nfm=sum(Nfm)) %>% 
    pivot_wider(names_from = c(stod), values_from = Nfm) %>% 
    arrange(Phylum,Class)
  df <- df[, c('Phylum', 'Class', 'Flokkun', 'A7','B5','B8','C4','E3','E4')]  

   write.xlsx(df, file = "Fjoldi_a_fermetra.xlsx",
             sheetName = "1999", showNA = FALSE, append = FALSE)

for (i in 2013:2017) {
  df <- KolgrTaxa %>% 
    mutate(Artal = factor(Artal)) %>% 
    filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3") & Artal==i) %>% 
    ddply(.(Phylum, Class, Flokkun, stod),summarise, Nfm=sum(Nfm)) %>% 
    pivot_wider(names_from = c(stod), values_from = Nfm) %>% 
    arrange(Phylum,Class)
  df <- df[, c('Phylum','Class', 'Flokkun', 'A7','B5','B8','C4','E3','E4')]  
  write.xlsx(df, file = "Fjoldi_a_fermetra.xlsx",
             sheetName=as.character(i), showNA = F, append=TRUE)
}
  
=======
>>>>>>> dc1a0a0fd3705911a60dcba78ad7c842e50391b0
>>>>>>> 6f33eb9cf7b3dd42358b66164f73a93b800f33bb

   
   
   
   
   
   
   KolgrTaxa <- read_csv("KolgrTaxa.csv", na = "empty") 
   
   remove_list <- paste(c(
     "nýsestir",
     "ungviði",
     "ungv",
     "ungv.",
     "juv",
     "ath"
   ), collapse = '|') 
   
   remove_ind <- lapply(strsplit(remove_list , "\\|")[[1]] , \(x) grep(x , KolgrTaxa$gamalt , fixed = T)) |> 
     unlist() |> 
     unique()
   
   ekkiungvidi <- KolgrTaxa[-remove_ind,] 
   
   df <- ekkiungvidi %>% 
     mutate(Artal = factor(Artal)) %>% 
     filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
     ddply(.(Artal, stod,Flokkun),summarise, N=sum(Nfm)) %>% 
     arrange(N)
   
   DF <- ekkiungvidi %>% 
     filter(!Flokkun %in% c("Copepoda","Collembola", "Cyclopterus lumpus") &
              !Class %in% "Copepoda") %>% 
     mutate(
       Flokkun = case_when(
         Artal == 1999 & Flokkun == "Ampharete acutifrons" ~ "Ampharetinae",
         Artal == 1999 & Flokkun == "Bivalvia" ~ NA,
         Artal == 2013 & Flokkun == "Harmothoe" ~ "Harmothoe extenuata",
         Artal == 2013 & Flokkun == "Polynoidae" ~ "Harmothoe extenuata",
         Artal == 2014 & Flokkun == "Praxillella" ~ "Praxillella praetermissa",
         Artal == 2014 & Flokkun == "Syllidae" ~ "Syllis cornuta",
         Artal == 2015 & Flokkun == "Syllidae" ~ "Syllis cornuta",
         Artal == 2015 & Flokkun == "Mya" ~ "Mya arenaria",
         Artal == 2015 & Flokkun == "Mytilidae" ~ "Mytilus edulis",
         Artal == 2015 & Flokkun == "Bivalvia" ~ NA,
         Artal == 2016 & Flokkun == "Ampharetidae" ~ "Amphitrite cirrata",
         Artal == 2016 & Flokkun == "Aricidea" ~ "Aricidea suecica",
         Artal == 2016 & Flokkun == "Capitellidae" ~ "Capitella capitata",
         Artal == 2016 & Flokkun == "Cirratulidae" ~ "Cirratulus cirratus",
         Artal == 2016 & Flokkun == "Cossuridae" ~ "Cossura longocirrata",
         Artal == 2016 & Flokkun == "Nephtyidae" ~ "Nephtys",
         Artal == 2016 & Flokkun == "Pectinariidae" ~ "Pectinaria koreni",
         Artal == 2016 & Flokkun == "Phyllodocida" ~ "Phyllodoce maculata",
         Artal == 2016 & Flokkun == "Spio" ~ "Spio filicornis",
         Artal == 2016 & Flokkun == "Spionidae" ~ "Spio filicornis",
         Artal == 2016 & Flokkun == "Syllidae" ~ "Syllis",
         Artal == 2016 & Flokkun == "Cardiidae" ~ "Cardium",
         Artal == 2016 & Flokkun == "Cardiidae" ~ "Cardium",
         Artal == 2017 & Flokkun == "Mya" ~ "Mya arenaria",
         Artal == 2017 & Flokkun == "Maldanidae" ~ "Praxillella praetermissa",
         TRUE ~ Flokkun)) %>% 
     drop_na()
   
   dfvegan <- DF %>% mutate(Artal = factor(Artal)) %>% 
     filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
     ddply(.(Flokkun,stod),summarise,N=sum(Nfm)) %>%
     filter(Flokkun!="") %>% 
      mutate(Flokkun = factor(Flokkun)) %>%
      pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
      replace(is.na(.), 0) %>%
      column_to_rownames(var="stod") 
   ord <- decorana(labdsv::hellinger(dfvegan))
   plot(ord, type = "n", main = i)
   #points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
   text(ord, display = "spec", cex=0.7, col="blue")
   text(ord, display = "sites", cex=1, col="red")
   
   
   
   dfvegan <- DF %>% 
     filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>%
     add_column(dypi = 1) %>% 
     mutate(
       dypi = case_when(
         stod == "C4" ~ 16.6,
         stod == "A7" ~ 24.5,
         stod == "B5" ~ 16.6,
         stod == "B8" ~ 34.0,
         stod == "E4" ~ 11.2,
         TRUE ~ 11.0)) %>%
     tidyr::unite(rowname, Artal, stod) %>% 
     ddply(.(Flokkun,rowname,dypi),summarise,N=sum(N)) 
   
   
   dfvegan %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     column_to_rownames(var="rowname")
   
   
   
   
   

   
   
   
   
   
   ordiplot (ord, display = 'si', type = 'n')
   points (ord, col = botngerd$Dypi, pch =
             botngerd$Dypi )
   
   
   
   for (i in unique(jorundur$Artal)) {
     
    veganjorundur <- jorundur %>%
     filter(Artal == i) %>% 
     #select(-Artal) %>% 
      ddply(.(Flokkun,stod),summarise,N=sum(N)) %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
      replace(is.na(.), 0) %>%
      column_to_rownames(var="stod")
   
   
   #library(vegan)
   #data(dune)
 ord <- decorana(labdsv::hellinger(veganjorundur))
 #ord <- metaMDS(veganjorundur)
 plot(ord, type = "n", main = i)
 #points(ord, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
 text(ord, display = "spec", cex=0.7, col="blue")
 text(ord, display = "sites", cex=1, col="red")
 
 ordiellipse(ord, botngerd$Sild, col=1:4, kind = "ehull", lwd=3)
 ordiellipse(ord, botngerd$Sild, col=1:4, draw="polygon")
 ordispider(ord, botngerd$Sild, col=1:4, label = TRUE)
 #points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)
 #text(ord, display = "sites", cex=1, col="blue")

## Euclidean distance
#dist <- dist(veganjorundur , diag=TRUE)
#
## Hierarchical Clustering with hclust
#hc <- hclust(dist)
#
## Plot the result
#plot(hc, main = i)
    PCA <- rda(veganjorundur, scale = FALSE)
   plot(PCA, main=i)
   sitePCA <- PCA$CA$u # Site scores
   speciesPCA <- PCA$CA$v # Species scores
   
   # In a biplot of a PCA, species' scores are drawn as arrows 
   # that point in the direction of increasing values for that variable
   biplot(PCA, choices = c(1,2), type = c("text", "text"), xlim = c(-5,10), main=i) # biplot of axis 1 vs 2
   biplot(PCA, choices = c(1,3), type = c("text","text"), main=i) # biplot of axis 1 vs 3
   }
   
   
   
   veganjorundur %>%
     metaMDS(trace = F) %>%
     ordiplot(type = "none") %>%
     text("sites")
   
   
   
   PCA <- rda(veganjorundur, scale = FALSE)
   # Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
   # Here, all species are measured on the same scale 
   # So use scale = FALSE
   
   # Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
   barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
   # How much of the variance in our dataset is explained by the first principal component?
   
   # Calculate the percent of variance explained by first two axes
   sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
   # Also try to do it for the first three axes
   
   # Now, we`ll plot our results with the plot function
   plot(PCA)
   plot(PCA, display = "sites", type = "points")
   plot(PCA, display = "species", type = "text")   
   
   
   ###gera ord fyrir 199 á moti rest
   
   # 1999 á móti rest
   agnarallt <- jorundur %>% 
     filter(Artal == 1999) %>% 
     ddply(.(Flokkun),summarise,N=sum(N)) %>% 
     add_column(stod="1999")
   stodvar <- jorundur %>% 
     filter(Artal != 1999) %>% 
     ddply(.(stod,Flokkun),summarise,N=sum(N)) %>% 
     arrange(Flokkun, N, stod)
   stodvar <- rbind(stodvar,agnarallt) 
   
   rass <- stodvar %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     column_to_rownames(var="stod")
   
   ## Euclidean distance
   dist <- dist(rass , diag=TRUE)
   #
   ## Hierarchical Clustering with hclust
   hc <- hclust(dist)
   #
   ## Plot the result
   plot(hc,main = "1999 á móti 2013-2017")
   
   #lúppa fyrir öll ár
   for (i in 2013:2017) {
   stodvar <- jorundur %>% 
     filter(Artal != 1999 & Artal == i) %>% 
     ddply(.(stod,Flokkun),summarise,N=sum(N)) %>% 
     arrange(Flokkun, N, stod)
   stodvar <- rbind(stodvar,agnarallt) 
   
   rass <- stodvar %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     column_to_rownames(var="stod")
   
   ## Euclidean distance
   dist <- dist(rass , diag=TRUE)
   hc <- hclust(dist)
   plot(hc,main = paste("1999 á móti",i))
   
   }
   #
   
   
   
   
   
   
   
   
   
   
   
   #ungviði
   
   
   
   
   remove_list <- paste(c(
     "nýsestir",
     "ungviði",
     "ungv",
     "ungv.",
     "juv",
     ".",
     "ath"
   ), collapse = '|') # Röðin skiptir ekki máli til dæmis "." á undan "sp." eða öfugt. Það eru jafn mörg tilfelli af "sp." í allartalningar$Flokkun þó svo "." sé á undan
   
   remove_ind <- lapply(strsplit(remove_list , "\\|")[[1]] , \(x) grep(x , geggjad$gamalt , fixed = T)) |> 
     unlist() |> 
     unique()
   
   
   
   ungvidi <- geggjad[remove_ind,] 
   
   ungv <- ddply(ungvidi,.(Flokkun,Artal),summarise, N =sum(Nu))
   
   
   
   Fjölþátta greining eingöngu 2013-2017
   2. lumpa saman öllum stöðvum (sem við notuðum) Agnars í eina. 
   3. 1999-2017
   Bray-Curtis
   Multi dimensional scaling 
   (meginþáttagreiningu og fylgnigreiningu með reikniforritinu R (R Core Team, 2014;  Oksanen o. ., 2015))
   Passa upp á að 1999 sé sambærileg m2 þegar skoðað er þéttleikabreytingar á tíma
   Fjöldi teg á stög sé óháð flatarmáli
   
   # 1. Marine biology
   # 2. Marine biology research
   # 3. Marine pollution bullettin
   
   
   
   robot trex
   stich
   
   
   
   veganjorundur <- jorundur %>%
     tidyr::unite(rowname, Artal, stod) %>% 
     ddply(.(Flokkun,rowname),summarise,N=sum(N)) %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     column_to_rownames(var="rowname")
   
   ## Euclidean distance
   dist <- dist(veganjorundur , diag=TRUE)
   ## Hierarchical Clustering with hclust
   hc <- hclust(dist)
   ## Plot the result
   plot(hc)
   
   
   
   
   
   
   
   
   gogn <- jorundur %>% 
     ddply(.(Artal,stod,Flokkun),summarise, N=sum(N)) %>% 
     pivot_wider(names_from = c(stod,Artal), values_from = N)
   
   BBIlisti <- list()
   BBIastand <- list()
   nEQR <- list()
   for (i in unique(jorundur$Artal)) {
     my_BBI <- jorundur %>% filter(Artal %in% c(i)) %>%
       ddply(.(Artal,stod,Flokkun),summarise, N=sum(N)) %>% 
       select(-Artal) %>%
       pivot_wider(names_from = stod, values_from = N) %>% 
       BBI()
     # calculating nEQR values and ecological quality status
     BBIlisti[[i]] <- as.data.frame(cbind(my_BBI$BBI, Artal=i))
     BBIastand[[i]] <- my_BBI$BBIclass
     nEQR[[i]] <- as.data.frame(nEQR(my_BBI$BBI)[1])
   }
   
   rass <- do.call(rbind,nEQR)
   names(rass) <- c("nAMBI","nISI","nNSI","nNQI1","nShannon","nEQR")
   mm <- as.matrix(rass, ncol = 7)
   
   heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
             cellnote = mm, notecol = "black", notecex = .51,
             trace = "none", key = FALSE)
   
   heatmap.2(mm,dendrogram = "row")
   
   
   
   rass %>% 
     select(nNQI1, nAMBI, nShannon) %>% 
     as.matrix() %>% 
     heatmap.2(dendogram = "row")
   
   
   
   
   
   
   
   
   
   
   
   #Artal
   DF.A <- jorundur %>%
     #tidyr::unite(rowname, Artal, stod) %>% 
     ddply(.(Flokkun,Artal,stod),summarise,N=sum(N)) %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     select(-c(Artal,stod))
  
   DF.B <- jorundur %>%
     #tidyr::unite(rowname, Artal, stod) %>% 
     ddply(.(Flokkun,Artal,stod),summarise,N=sum(N)) %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     select(c(Artal))
   
   #stöð
   DF.A <- jorundur %>%
     #tidyr::unite(rowname, Artal, stod) %>% 
     ddply(.(Flokkun,Artal,stod),summarise,N=sum(N)) %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     select(-c(Artal,stod))
      
   DF.B <- jorundur %>%
     #tidyr::unite(rowname, Artal, stod) %>% 
     ddply(.(Flokkun,Artal,stod),summarise,N=sum(N)) %>% 
     pivot_wider(names_from = c(Flokkun), values_from = N) %>% 
     replace(is.na(.), 0) %>%
     select(c(stod)) %>% 
     mutate(stod = factor(stod)) 
   
   
   
   
   require("vegan")
   
   ## load the Dune data
   #data(dune, dune.env)
   
   ## PCA of the Dune data
   mod <- rda(DF.A, scale = TRUE)
   
   ## plot the PCA
   plot(mod, scaling = 3)
   
   ## build the plot up via vegan methods
   scl <- 3 ## scaling == 3
   colvec <- c("red2", "green4", "mediumblue", "orange", "black", "cyan")
   plot(mod, type = "n", scaling = scl)
   text(mod, display = "species", scaling = scl, cex = 0.8, col = "grey")
   with(DF.B, points(mod, display = "sites", col = colvec[Artal],
                         scaling = scl, pch = 21, bg = colvec[Artal]))
   with(DF.B, legend("topright", legend = levels(Artal), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))
   #with(DF.B, text(mod, display = "sites", scaling = scl, cex = 0.8, col = colvec[Artal]))
   
   
   
   
   
   
   
   A<-list()
   B<-c()
   for (i in 1:5) {
     A[i] <- list.files("./skjol/gps", full.names = T)
     #try(rgdal::ogrListLayers(A[i]))
     try(sf::st_read(A[i]))
   }
 rgdal::readOGR("./skjol/gps/Synataka_21_24_25_juni_2013.gdb")
   
 
 
 
 
 
 
 
 
 
 
 
 
 remove_list <- paste(c(
   "nýsestir",
   "ungviði",
   "ungv",
   "ungv.",
   "juv",
   ".",
   "ath"
 ), collapse = '|') # Röðin skiptir ekki máli til dæmis "." á undan "sp." eða öfugt. Það eru jafn mörg tilfelli af "sp." í allartalningar$Flokkun þó svo "." sé á undan
 
 remove_ind <- lapply(strsplit(remove_list , "\\|")[[1]] , \(x) grep(x , geggjad$gamalt , fixed = T)) |> 
   unlist() |> 
   unique()
 
 
 
 ungvidi <- geggjad[remove_ind,] 
 
 ungv <- ddply(ungvidi,.(Flokkun,Artal),summarise, N =sum(Nu))
 
 
 
 Fjölþátta greining eingöngu 2013-2017
 2. lumpa saman öllum stöðvum (sem við notuðum) Agnars í eina. 
 3. 1999-2017
 Bray-Curtis
 Multi dimensional scaling 
 (meginþáttagreiningu og fylgnigreiningu með reikniforritinu R (R Core Team, 2014;  Oksanen o. ., 2015))
 Passa upp á að 1999 sé sambærileg m2 þegar skoðað er þéttleikabreytingar á tíma
 Fjöldi teg á stöð sé óháð flatarmáli
 
 # 1. Marine biology
 # 2. Marine biology research
 # 3. Marine pollution bullettin
 
 
 A <- list()
 for (i in c("1999", "2013", "2014", "2015", "2016", "2017")) {
   df <- DF[DF$Artal==i,]
   #df <- jorundur[jorundur$Artal==i,]
   A[[i]] <- names(sort(sapply(split(df,df$Flokkun), function(x) sum(x$N)), decreasing = T)[1:10])
 }
 gagn <- table(unlist(A))
 gagn <- gagn[gagn>1]
 
 rass <- DF[DF$Flokkun %in% names(gagn),] %>% ddply(.(Artal, stod,Flokkun),summarise, N=sum(N), .drop=FALSE)%>% 
   filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
   mutate(Artal = factor(Artal))
 
 rass%>% 
   ggplot(aes(x = Artal, y = N)) +
   facet_wrap(~Flokkun, scales = "free") +
   geom_bar(aes(fill = stod), stat = "identity", color="black",position=position_dodge(preserve = "single"))  +
   xlab("") + 
   labs(caption = "(Botndýr í Kolgrafafirði 1999 og 2013-2017)") +
   theme_pubclean()
 
 BBIlisti <- list()
 BBIastand <- list()
 nEQR <- list()
 for (i in unique(rass$Artal)) {
   my_BBI <- rass %>% filter(Artal %in% c(i)) %>%
     select(-Artal) %>%
     pivot_wider(names_from = stod, values_from = N) %>% 
     BBI()
   # calculating nEQR values and ecological quality status
   BBIlisti[[i]] <- as.data.frame(cbind(my_BBI$BBI, Artal=i))
   BBIastand[[i]] <- my_BBI$BBIclass
   nEQR[[i]] <- as.data.frame(nEQR(my_BBI$BBI)[1])
 }
 
 ress <- do.call(rbind,nEQR)
 names(ress) <- c("nAMBI","nISI","nNSI","nNQI1","nShannon","nEQR")
 mm <- as.matrix(ress, ncol = 7)
 
 heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
           cellnote = mm, notecol = "black", notecex = .51,
           trace = "none", key = FALSE, srtCol=0,   adjCol = c(0.5,1))
 
 heatmap.2(mm,dendrogram = "row", srtCol=0,   adjCol = c(0.5,1))
 
 
 ress %>% 
   select(nNQI1, nAMBI, nShannon) %>% 
   as.matrix() %>% 
   heatmap.2(dendogram = "row", srtCol=0,   adjCol = c(0.5,1)) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 f <- file.path(list.files(here::here("set"),pattern = "pdf",full.names = T))
 tafla <- tabulizer::extract_tables(f, pages = 17:53)
 tabbla <- do.call(rbind,tafla)
 
 B<-list()
 for (i in names(gagn)) {
   B[[i]]<-tabbla[grepl(substr(i,1,6),tabbla[,4]),]
 }
 
 td <- do.call(rbind,B)
 Tdf <- td[c(3,32,44,50,74,86,90,93,103,110,113,117,120),]
 Tdf[,1] <- unname(Tdf[,1])
 
 
 
 Tdf <- structure(c("POSE", "POSE", "POER", "POER", "MOBI", "POSE", "POER", 
                    "ANOL", "POSE", "POSE", "CRAM", "POSE", "POSE", "POSE", "0248", 
                    "0252", "0214", "0220", "0456", "0248", "0186", "0000", "0270", 
                    "0310", "0810", "0304", "0272", "0310", "0450", "0500", "0300", 
                    "0590", "0520", "0950", "0687", "0001", "1140", "1507", "1490", 
                    "1680", "1745", "0310", "Capitella capitata", "Chaetozone setosa", 
                    "Eteone longa", "Harmothoe", "Macoma calcarea", "Mediomastus fragilis", 
                    "Microphthalmus aberrans", "Oligochaeta", "Ophelina acuminata", 
                    "Polydora", "Protomedeia fasciata", "Scalibregma inflatum", 
                    "Scoloplos armiger", "Spionidae", "SS", "SR", "SR", "SR", 
                    "SR", "SS", "SR", "SS", "SS", "SR", "SR", "SS", "SS", "SR", "D", 
                    "D", "M", "M", "D", "D", "M", "M", "M", "D", "D", "M", "M", "D", 
                    "F", "F", "F", "F", "F", "F", "F", "F", "F", "T", "T", "B", "F", 
                    "T", "Om", "Om", "Ca", "Ca", "Om", "Om", "He", "Om", "Om", "Om", 
                    "Om", "Om", "Om", "Om", "sed/pom/mic", "sed/pom/mic/dia", "mac", 
                    "mac", "sed/pom/mic", "sed/pom/mic", "dia", "pom/mic/dia", "sed/pom/mic", 
                    "sed/pom/mic/dia/phy", "pom/mic/dia/phy", "sed/pom/mic", "sed/pom/mic", 
                    "sed/pom/mic/dia/phy", "De", "De", "Pr/De", "Pr", "De/Su", "De", 
                    "Gr", "Dt", "De", "De/Su", "Su", "De", "De", "De/Su", "SS-De", 
                    "SR-De", "SR-Pr-mac", "SR-Pr-mac", "SR-De", "SS-De", "SR-He-mic", 
                    "SS-Om-mic", "SS-De", "SR-De", "SR-Su", "SS-De", "SS-De", "SR-De"
 ), dim = c(14L, 11L), dimnames = list(NULL, c("Major Group", 
                                               "Family code", "Species code", "Flokkun", "Food Source", 
                                               "Motility", "Habit", "Om/Ca/He", "Food size/type", "FeedMode", 
                                               "Feeding guild")))
 
 
 Tdf <- as.data.frame(Tdf[-14,])
 rass <- DF %>% filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3"))
 
 Dt <- merge(rass, 
             Tdf,
             all.x = T,
             by = "Flokkun")
 
 Dt %>% 
   mutate(Artal = factor(Artal)) %>% 
   ggplot(aes(x = Artal, y = N)) +
   facet_wrap(~Dt$`FeedMode`, scales = "free") +
   geom_bar(aes(fill = stod), stat = "identity", color="black",position="dodge")  +
   xlab("") + 
   labs(caption = "(Botndýr í Kolgrafafirði 1999 og 2013-2017)")
 
 # ANOL Ph. Annelida, Oligochaeta (oligochaetes)
 # MOBI Ph. Mollusca, Cl. Bivalvia (clams, scallops, mussels, oysters, etc.) 
 # POER Ph. Annelida, Polychaeta Errantia 349 
 # POSE Ph. Annelida, Polychaeta Sedentaria 
 
 
 
 
 
 
 
 
 library(BBI)
 BBIlisti <- list()
 BBIastand <- list()
 nEQR <- list()
 for (i in unique(jorundur$Artal)) {
   my_BBI <- jorundur %>% filter(Artal %in% c(i)) %>%
     ddply(.(Artal,stod,Flokkun),summarise, N=sum(N)) %>% 
     select(-Artal) %>%
     pivot_wider(names_from = stod, values_from = N) %>% 
     BBI()
   # calculating nEQR values and ecological quality status
   BBIlisti[[i]] <- as.data.frame(cbind(my_BBI$BBI, Artal=i))
   BBIastand[[i]] <- my_BBI$BBIclass
   nEQR[[i]] <- as.data.frame(nEQR(my_BBI$BBI)[1])
 }
 
 rass <- do.call(rbind,nEQR)
 names(rass) <- c("nAMBI","nISI","nNSI","nNQI1","nShannon","nEQR")
 
 
 rass %>%
   scale() %>%                           # Scale the data
   dist() %>%                            # Compute distance matrix
   hclust(method = "ward.D2") %>%        # Hierarchical clustering
   fviz_dend(cex = 0.5, k = 6, palette = "jco") 
 
 
 
 
 
 
 
 
 
 
 
 
 
Nofn1 <- names(gagn)
df2 <- as.data.frame(td)
Nofn2 <-  df2$V4

df2[Nofn1 %in% Nofn2,]








library(dplyr)
library(knitr)

x <- Nofn2

ignores <- c("university", "college", "u", "of", "institute", "inst")

x_refin <- x %>% 
  refinr::key_collision_merge(ignore_strings = ignores) %>% 
  refinr::n_gram_merge(ignore_strings = ignores)

# Create df for comparing the original values to the edited values.
# This is especially useful for larger input vectors.
inspect_results <- data_frame(original_values = x, edited_values = x_refin) %>% 
  mutate(equal = original_values == edited_values)

# Display only the values that were edited by refinr.
knitr::kable(
  inspect_results[!inspect_results$equal, c("original_values", "edited_values")]
)
#> |original_values                         |edited_values                    |
#> |:---------------------------------------|:--------------------------------|
#> |Clemsson University                     |CLEMSON                          |
#> |university-of-clemson                   |CLEMSON                          |
#> |Clem son, U.                            |CLEMSON                          |
#> |college, clemson u                      |CLEMSON                          |
#> |Technology, Massachusetts' Institute of |Massachusetts Inst of Technology |
#> |UNIVERSITY:  mit                        |M.I.T.                           |
 
 


f <- file.path(list.files(here::here("skjol/lesefni/feeding"),pattern = "pdf",full.names = T))
tafla <- tabulizer::extract_tables(f)
tabbla <- do.call(rbind,tafla)











remove_list <- paste(c(
  "nýsestir",
  "ungviði",
  "ungv",
  "ungv.",
  "juv",
  ".",
  "ath"
), collapse = '|') # Röðin skiptir ekki máli til dæmis "." á undan "sp." eða öfugt. Það eru jafn mörg tilfelli af "sp." í allartalningar$Flokkun þó svo "." sé á undan

remove_ind <- lapply(strsplit(remove_list , "\\|")[[1]] , \(x) grep(x , geggjad$gamalt , fixed = T)) |> 
  unlist() |> 
  unique()



ungvidi <- geggjad[remove_ind,] 

ungv <- ddply(ungvidi,.(Flokkun,Artal),summarise, N =sum(Nu))



Fjölþátta greining eingöngu 2013-2017
2. lumpa saman öllum stöðvum (sem við notuðum) Agnars í eina. 
3. 1999-2017
Bray-Curtis
Multi dimensional scaling 
(meginþáttagreiningu og fylgnigreiningu með reikniforritinu R (R Core Team, 2014;  Oksanen o. ., 2015))
Passa upp á að 1999 sé sambærileg m2 þegar skoðað er þéttleikabreytingar á tíma
Fjöldi teg á stöð sé óháð flatarmáli

# 1. Marine biology
# 2. Marine biology research
# 3. Marine pollution bullettin


A <- list()
for (i in c("1999", "2013", "2014", "2015", "2016", "2017")) {
  df <- DF[DF$Artal==i,]
  #df <- jorundur[jorundur$Artal==i,]
  A[[i]] <- names(sort(sapply(split(df,df$Flokkun), function(x) sum(x$N)), decreasing = T)[1:10])
}
gagn <- table(unlist(A))
gagn <- gagn[gagn>1]

rass <- DF[DF$Flokkun %in% names(gagn),] %>% ddply(.(Artal, stod,Flokkun),summarise, N=sum(N), .drop=FALSE)%>% 
  filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3")) %>% 
  mutate(Artal = factor(Artal))

rass%>% 
  ggplot(aes(x = Artal, y = N)) +
  facet_wrap(~Flokkun, scales = "free") +
  geom_bar(aes(fill = stod), stat = "identity", color="black",position=position_dodge(preserve = "single"))  +
  xlab("") + 
  labs(caption = "(Botndýr í Kolgrafafirði 1999 og 2013-2017)") +
  theme_pubclean()

BBIlisti <- list()
BBIastand <- list()
nEQR <- list()
for (i in unique(rass$Artal)) {
  my_BBI <- rass %>% filter(Artal %in% c(i)) %>%
    select(-Artal) %>%
    pivot_wider(names_from = stod, values_from = N) %>% 
    BBI()
  # calculating nEQR values and ecological quality status
  BBIlisti[[i]] <- as.data.frame(cbind(my_BBI$BBI, Artal=i))
  BBIastand[[i]] <- my_BBI$BBIclass
  nEQR[[i]] <- as.data.frame(nEQR(my_BBI$BBI)[1])
}

ress <- do.call(rbind,nEQR)
names(ress) <- c("nAMBI","nISI","nNSI","nNQI1","nShannon","nEQR")
mm <- as.matrix(ress, ncol = 7)

heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = mm, notecol = "black", notecex = .51,
          trace = "none", key = FALSE, srtCol=0,   adjCol = c(0.5,1))

heatmap.2(mm,dendrogram = "row", srtCol=0,   adjCol = c(0.5,1))


ress %>% 
  select(nNQI1, nAMBI, nShannon) %>% 
  as.matrix() %>% 
  heatmap.2(dendogram = "row", srtCol=0,   adjCol = c(0.5,1)) 













f <- file.path(list.files(here::here("set"),pattern = "pdf",full.names = T))
tafla <- tabulizer::extract_tables(f, pages = 17:53)
tabbla <- do.call(rbind,tafla)

B<-list()
for (i in names(gagn)) {
  B[[i]]<-tabbla[grepl(substr(i,1,6),tabbla[,4]),]
}

td <- do.call(rbind,B)
td_nytt <- unname(td[,1])
Tdf <- td[c(3,32,44,50,74,86,90,93,103,110,113,117,120),]
Tdf[,1] <- unname(Tdf[,1])



Tdf <- structure(c("POSE", "POSE", "POER", "POER", "MOBI", "POSE", "POER", 
                   "ANOL", "POSE", "POSE", "CRAM", "POSE", "POSE", "POSE", "0248", 
                   "0252", "0214", "0220", "0456", "0248", "0186", "0000", "0270", 
                   "0310", "0810", "0304", "0272", "0310", "0450", "0500", "0300", 
                   "0590", "0520", "0950", "0687", "0001", "1140", "1507", "1490", 
                   "1680", "1745", "0310", "Capitella capitata", "Chaetozone setosa", 
                   "Eteone longa", "Harmothoe", "Macoma calcarea", "Mediomastus fragilis", 
                   "Microphthalmus aberrans", "Oligochaeta", "Ophelina acuminata", 
                   "Polydora", "Protomedeia fasciata", "Scalibregma inflatum", 
                   "Scoloplos armiger", "Spionidae", "SS", "SR", "SR", "SR", 
                   "SR", "SS", "SR", "SS", "SS", "SR", "SR", "SS", "SS", "SR", "D", 
                   "D", "M", "M", "D", "D", "M", "M", "M", "D", "D", "M", "M", "D", 
                   "F", "F", "F", "F", "F", "F", "F", "F", "F", "T", "T", "B", "F", 
                   "T", "Om", "Om", "Ca", "Ca", "Om", "Om", "He", "Om", "Om", "Om", 
                   "Om", "Om", "Om", "Om", "sed/pom/mic", "sed/pom/mic/dia", "mac", 
                   "mac", "sed/pom/mic", "sed/pom/mic", "dia", "pom/mic/dia", "sed/pom/mic", 
                   "sed/pom/mic/dia/phy", "pom/mic/dia/phy", "sed/pom/mic", "sed/pom/mic", 
                   "sed/pom/mic/dia/phy", "De", "De", "Pr/De", "Pr", "De/Su", "De", 
                   "Gr", "Dt", "De", "De/Su", "Su", "De", "De", "De/Su", "SS-De", 
                   "SR-De", "SR-Pr-mac", "SR-Pr-mac", "SR-De", "SS-De", "SR-He-mic", 
                   "SS-Om-mic", "SS-De", "SR-De", "SR-Su", "SS-De", "SS-De", "SR-De"
), dim = c(14L, 11L), dimnames = list(NULL, c("Major Group", 
                                              "Family code", "Species code", "Flokkun", "Food Source", 
                                              "Motility", "Habit", "Om/Ca/He", "Food size/type", "FeedMode", 
                                              "Feeding guild")))


Tdf <- as.data.frame(Tdf[-14,])

Dt <- merge(rass, 
            Tdf,
            all.x = T,
            by = "Flokkun")

Dt %>% 
  ggplot(aes(x = Artal, y = N)) +
  facet_wrap(~Dt$`Major Group`, scales = "free") +
  geom_bar(aes(fill = stod), stat = "identity", color="black",position="dodge")  +
  xlab("") + 
  labs(caption = "(Botndýr í Kolgrafafirði 1999 og 2013-2017)")

# ANOL Ph. Annelida, Oligochaeta (oligochaetes)
# MOBI Ph. Mollusca, Cl. Bivalvia (clams, scallops, mussels, oysters, etc.) 
# POER Ph. Annelida, Polychaeta Errantia 349 
# POSE Ph. Annelida, Polychaeta Sedentaria 


data1_agg <- ddply(jorundur,.(Artal, stod, Flokkun), summarise, N = sum(N))


f <- file.path(list.files(here::here("set"),pattern = "pdf",full.names = T))
tafla <- tabulizer::extract_tables(f, pages = 17:48)
tabbla <- do.call(rbind,tafla)

# B<-list()
# for (i in names(gagn)) {
#   B[[i]]<-tabbla[grepl(substr(i,1,6),tabbla[,4]),]
# }
# 
# 
# 
# library(dplyr)
# table_df <- bind_rows(lapply(B, as.data.frame))
# 
# colnames(table_df) <- c("Major Group", "Family code", "Species code", "Taxon name", "Food Source", 
#                         "Motility", "Habit", "Om/Ca/He", "Food size/type", "FeedMode", 
#                         "Feeding guild")


tabbla_df <- as.data.frame(tabbla)
colnames(tabbla_df) <- c("Major Group", "Family code", "Species code", "Taxon name", "Food Source", 
                         "Motility", "Habit", "Om/Ca/He", "Food size/type", "FeedMode", 
                         "Feeding guild")
table_df <- subset(tabbla_df, !grepl("^Major", `Major Group`))


find_closest_match <- function(x, y) {
  string_distances <- stringdist::stringdistmatrix(tolower(x), tolower(y), method = "jw")
  y[apply(string_distances, 1, which.min)]
}

data1_agg$matched_taxon_name <- find_closest_match(data1_agg$Flokkun, table_df$`Taxon name`)

# nákvæm pörun:
find_exact_match <- function(x, y) {
  sapply(x, function(Flokkun) {
    matched <- y[Flokkun == y]
    if (length(matched) > 0) {
      return(matched[1])
    } else {
      return(NA_character_)
    }
  }, simplify = "character")
}


table_df$`Cleaned Taxon name` <- gsub("\\s*(sp\\.|spp\\.|indet\\.)", "", table_df$`Taxon name`)

data1_agg$exact_taxon_name <- find_exact_match(data1_agg$Flokkun, table_df$`Cleaned Taxon name`)
unmatched_rows <- is.na(data1_agg$exact_taxon_name)
data1_agg$matched_taxon_name[unmatched_rows] <- find_closest_match(data1_agg$Flokkun[unmatched_rows], table_df$`Cleaned Taxon name`)
data1_agg$final_taxon_name <- ifelse(is.na(data1_agg$exact_taxon_name), data1_agg$matched_taxon_name, data1_agg$exact_taxon_name)
merged_data <- merge(data1_agg, table_df, by.x = "final_taxon_name", by.y = "Taxon name")






grouped_data <- ddply(data_2013, .(`Major Group`, stod), summarise, total_N = sum(N, na.rm = TRUE), .drop = F)


# Load the tidyverse package
library(tidyverse)

library(plyr)

# Group data by year, major group, and station
grouped_data <- ddply(merged_data, .(Artal, `Major Group`, stod), summarise, total_N = sum(N, na.rm = TRUE))

# Calculate station totals for each year
station_totals <- ddply(merged_data, .(Artal, stod), summarise, station_total_N = sum(N))

# Add station totals to the grouped data
grouped_data_with_totals <- join(grouped_data, station_totals, by = c("Artal", "stod"))

# Calculate proportions
grouped_data_with_totals$proportion <- grouped_data_with_totals$total_N / grouped_data_with_totals$station_total_N

# Spread the data into a wide format
wide_data <- reshape2::dcast(grouped_data_with_totals, Artal + stod ~ `Major Group`, value.var = "proportion", fill = 0)

# Print the wide data
print(wide_data)


for (i in c("1999",2013:2017)) {
  wide_data_B <- wide_data[wide_data$Artal ==i,]
  wide_dataB <- t(wide_data_B[,-c(1:2)])
  #seti2 <- prop.table(as.matrix(seti2), 2)
  colnames(wide_dataB) <- wide_data_B$stod
  
  barplot(wide_dataB,main = unique(wide_data_B$Artal))
  require(graphics)
  #legend("topright",horiz = F, rownames(wide_dataB),fill = gray.colors(12))
}















library(ggplot2)
library(reshape2)

# Define selected columns for plotting
selected_cols <- c("Major Group", "Family code", "Species code", 
                   "Food Source", "Motility", "Habit", 
                   "Om/Ca/He", "Food size/type", "FeedMode", 
                   "Feeding guild")

# Loop through selected columns
for (col in selected_cols) {
  LabX<-col
  # Create a data frame for the current column
  data_col <- merged_data[, c("Artal", "stod", col, "N")]
  
  # Remove rows with missing values
  data_col <- na.omit(data_col)
  colnames(data_col)[3] <- "col"
  # Aggregate N values by Artal, stod, and the current column
  agg_data <- aggregate(N ~ Artal + stod + col, data_col, sum)
  
  # Compute proportions for each station
  prop_data <- ddply(agg_data, .(Artal, stod), transform, prop_N = N / sum(N))
  
  # Reshape data to long format
  melted_data <- melt(prop_data, id.vars = c("Artal", "stod", "col"))
  
  # Create stacked bar plot for each year and station
  for (year in unique(merged_data$Artal)) {
    for (station in unique(merged_data$stod)) {
      
      # Filter data for the current year and station
      plot_data <- subset(melted_data, Artal == year & stod == station)
      plot_data <- plot_data[plot_data$variable!="N",]
      # Create stacked bar plot
      print(ggplot(plot_data, aes(x = col, y = value, fill = variable)) +
              geom_bar(stat = "identity", position = "stack") +
              scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
              xlab(LabX) +
              ylab("Proportion") +
              ggtitle(paste("Station ", station, " - Year ", year, sep = "")) +
              theme_bw())
    }
  }
}





df <- data.frame(find_name = c("major_group", "food_source", "motility", "habit", "om_ca_he", "food_size_type", "feed_mode", "feeding_guild"),
                 text = c(
                   'major_group: Ph. Mollusca, Cl. Gastropoda (snails) "MOGA",\n Ph. Annelida, Polychaeta Sedentaria "POSE", Ph. Arthropoda, SubPh. Chelicerata, Cl. Arachnida (mites) "CHAC", Ph. Arthropoda, SubPh. Crustacea, O. Amphipoda (amphipods) "CRAM", Ph. Annelida, Polychaeta Errantia "POER", Ph. Mollusca, Cl. Bivalvia (clams, scallops, mussels, oysters, etc.) "MOBI", Ph. Cnidaria, Hydrozoa (hydroids) "CNHY", Ph. Echinodermata, Cl. Asteroidea (seastars) "ECAS", Ph. Arthropoda, SubPh. Crustacea, Cl. Ostracoda (seed shrimp) "CROS", Ph. Priapulida (penis worms) "PRIA", Ph. Arthropoda, SubPh. Crustacea, O. Isopoda "CRIS", Ph. Annelida, Oligochaeta (oligochaetes) "ANOL"',
                   'food_source: (epibenthic (EP), surface (SR), subsurface (SS);',
                   'om_ca_he: (carnivorous (Ca), herbivorous (He) omnivorous (Om);',
                   'food_size_type: (sediment (sed), particulate organic matter (pom), benthic microfauna (e.g., diatoms and other single-celled organisms, mic), benthic meiofauna (organisms retained on a <500 μm sieve, mei), benthic macrofauna (organisms retained on a >500 μm sieve, including macroalgae, mac), phytoplankton (phy), zooplankton (zoo) terrestrial material (e.g., wood, terr).',
                   'feed_mode: Deposit feeder (ingests sediment; De), Detritus feeder (ingests particular matter only, without sediment; Dt), Suspension/Filter feeder (strains particles from the water, Su), Predator (eats live animals only; Pr), Scavenger (carrion only; Sc), Suctorial parasite (Sp), Chemosynthetic (with symbiotic bacteria, Ch), Lignivorous (eats wood, Li), Grazer (feeds by scraping, either on algae or sessile animals, Gr), and Browsing (feeds by tearing or gathering particular items, Br).',
                   'motility: Indicates if an animal is completely sessile (S), is able to move, but movement isn’t necessary for feeding (discretely motile, D), or moves actively, and movement is required for feeding (motile, M) (after Fauchald and Jumars, 1979).',
                   'habit: An animal may be free-living (may live on surface or actively burrow, F), Commensal (living with but not harming host, C), Tubiculous (T), Burrow-dwelling (sedentary, living in burrow, B), Encrusting (requiring a large point of attachment, e.g., compound ascidians or encrusting bryozoans, R), Attached (to hard substrate, requiring just one point of attachment, e.g., solitary ascidians or calcareous sponges, A), Parasitic (feeding directly on host, X), Anchored in the mud (sedentary, e.g., sea pens, or burrowing anemones, U) and Planktonic (spending the majority of its life cycle in the water column, P).',
                   'feeding_guild: (1) food source (epibenthic (EP), surface (SR), subsurface (SS); (2) diet type: (carnivorous (Ca), herbivorous (He) omnivorous (Om); (3) food type/size: (sediment (sed), particulate organic matter (pom), benthic microfauna (e.g., diatoms and other single-celled organisms, mic), benthic meiofauna (organisms retained on a <500 μm sieve, mei), benthic macrofauna (organisms retained on a >500 μm sieve, including macroalgae, mac), phytoplankton (phy), zooplankton (zoo) terrestrial material (e.g wood, terr), or symbiotic chemoautotrophic bacteria (sym), fish (fis) and (4) feeding mode Deposit feeder (ingests sediment; De), Detritus feeder (ingests particular matter only, without sediment; Dt), Suspension/Filter feeder (strains particles from the water, Su), Predator (eats live animals only; Pr), Scavenger (carrion only; Sc), Suctorial parasite (Sp), Chemosynthetic (hosting chemoautotrophic symbiotic bacteria, Ch), Lignivorous (eats wood, Li), Grazer (feeds by scraping, either on algae or sessile animals, Gr), and Browsing (feeds by tearing or gathering particular items, Br).'))
















"Acanthocardia echinata"  table_df[table_df$`Cleaned Taxon name` %in% "Cardiidae",]
"Abra nitida" table_df[table_df$`Cleaned Taxon name` %in% "Semelidae",]
"Arctica islandica" table_df[table_df$`Cleaned Taxon name` %in% "Bivalvia",]
"Aricidea suecica"  table_df[table_df$`Cleaned Taxon name` %in% "Aricidea",]
"Caprella septentrionalis"  table_df[table_df$`Cleaned Taxon name` %in% "Caprella",]
"Acanthocardia tuberculata"  table_df[table_df$`Cleaned Taxon name` %in% "Cardiidae",]
"Crassicorophium bonellii"  table_df[table_df$`Cleaned Taxon name` %in% "Corophium",]
"Parvicardium minimum"  table_df[table_df$`Cleaned Taxon name` %in% "Cardiidae",]
"Nereimyra punctata"  table_df[table_df$`Cleaned Taxon name` %in% "Hesionidae",]
"Eunoe nodosa"  table_df[table_df$`Cleaned Taxon name` %in% "Eunoe",]
"Glycera capitata"  table_df[table_df$`Cleaned Taxon name` %in% "Glycera",]
"Cistenides granulata"  table_df[table_df$`Cleaned Taxon name` %in% "Pectinaria",]
"Kurtiella bidentata"  table_df[table_df$`Cleaned Taxon name` %in% "Lasaeidae",]
"Margarites groenlandicus"  table_df[table_df$`Cleaned Taxon name` %in% "Margarites",]
"Schistomeringos nigridentata"  table_df[table_df$`Cleaned Taxon name` %in% "Dorvilleidae",]
"Cistenides hyperborea"  table_df[table_df$`Cleaned Taxon name` %in% "Pectinaria",]
"Tunicata" NA
"Parvicardium pinnulatum"  table_df[table_df$`Cleaned Taxon name` %in% "Cardiidae",]
"Dexamine thea"  table_df[table_df$`Cleaned Taxon name` %in% "Amphipoda",]
"Abra prismatica"  table_df[table_df$`Cleaned Taxon name` %in% "Semelidae",]
"Abra prismatica"  table_df[table_df$`Cleaned Taxon name` %in% "Sternaspis nr. fossor",]

































find_taxon_matchB <- function(x, taxon_levels) {
  matched_taxon <- rep(NA_character_, length(x))
  found_match <- rep(FALSE, length(x))
  
  for (level in taxon_levels) {
    if (all(found_match)) break
    
    level_taxon <- find_exact_match(x[!found_match], table_df[[level]])
    level_matched <- !is.na(level_taxon)
    matched_taxon[!found_match][level_matched] <- level_taxon[level_matched]
    found_match[!found_match] <- level_matched
  }
  
  return(matched_taxon)
}


taxon_levels <- c("Species", "Genus", "Family", "Superfamily", "Order", "Superorder", "Subterclass", "Infraclass", "Subclass", "Class", "Phylum", "Kingdom")
KolgrTaxa$exact_taxon_name <- find_taxon_match(KolgrTaxa$Flokkun, taxon_levels)

unmatched_rows <- is.na(KolgrTaxa$exact_taxon_name)
KolgrTaxa$final_taxon_name <- KolgrTaxa$exact_taxon_name
merged_data <- merge(KolgrTaxa, table_df, by.x = "final_taxon_name", by.y = "Taxon name", all.x = TRUE)










find_taxon_match <- function(x, kolgr_taxa, y) {
  matched_taxon <- rep(NA_character_, length(x))
  taxon_levels <- c("Genus", "Family", "Superfamily", "Order", "Superorder", "Subterclass", "Infraclass", "Subclass", "Class", "Phylum", "Kingdom")
  
  for (i in 1:length(x)) {
    found_match <- FALSE
    
    taxon_name <- find_exact_match(x[i], y$`Cleaned Taxon name`)
    if (!is.na(taxon_name)) {
      matched_taxon[i] <- taxon_name
      found_match <- TRUE
    }
    
    if (!found_match) {
      for (level in taxon_levels) {
        taxon_name <- find_exact_match(kolgr_taxa[[level]][i], y$`Cleaned Taxon name`)
        if (!is.na(taxon_name)) {
          matched_taxon[i] <- taxon_name
          break
        }
      }
    }
  }
  
  return(matched_taxon)
}

KolgrTaxa$matched_taxon_name <- find_taxon_match(KolgrTaxa$Flokkun, KolgrTaxa, table_df)
unmatched_rows <-  KolgrTaxa$matched_taxon_name == ""

find_closest_match <- function(x, y) {
  string_distances <- stringdist::stringdistmatrix(tolower(x), tolower(y), method = "jw")
  y[apply(string_distances, 1, which.min)]
}

KolgrTaxa$matched_taxon_name[unmatched_rows] <- find_closest_match(KolgrTaxa$Flokkun[unmatched_rows], table_df$`Cleaned Taxon name`)
merged_data <- merge(KolgrTaxa, table_df, by.x = "matched_taxon_name", by.y = "Taxon name")









#Code to be submitted







column_mapping <- data.frame(
  find_name = c("major_group", "food_source", "motility", "habit", "om_ca_he", "food_size_type", "feed_mode"),
  merged_data_col = c("Major.Group", "Food.Source", "Motility", "Habit", "Om.Ca.He", "Food.size.type", "FeedMode")
)

text_with_mapping <- merge(text, column_mapping, by = "find_name")
choices = text_with_mapping$find_name

