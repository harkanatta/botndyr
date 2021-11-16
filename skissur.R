library(DT)
library(tidyverse)

stodvar <- read.csv("skjol/stodvar.csv",header = T,encoding = "UTF-8",row.names = NULL)
Names <- make.names(stodvar[, 1], unique=TRUE) # Make a vector of names from the first column
stodvar <- stodvar[, -1] #delete the first column

stodvar <- stodvar %>% 
  mutate_all(~na_if(., '#VALUE!')) %>% replace(is.na(.), 0) %>% #replace "#VALUE!" with zeros
  mutate_if(is.character,as.integer)

rownames(stodvar) <- Names#[as.numeric(rownames(stodvar))] 

stodvar <- stodvar %>% 
  filter_all(any_vars(. != 0)) #remove rows with only zeros

#Names <- make.names(rownames(stodvar)) 
#rownames(stodvar) <- Names#[as.numeric(rownames(stodvar))]

DT::datatable(stodvar,rownames = T, caption = "2017")




stodvar <- read.csv("skjol/stod2015.csv",header = T,encoding = "UTF-8",row.names = NULL)
Names <- make.names(stodvar[, 1], unique=TRUE)
stodvar <- stodvar[, -1]
stodvar <- stodvar %>% filter_all(any_vars(. != 0))
str(stodvar)

rownames(stodvar) <- Names[as.numeric(rownames(stodvar))] #bull
DT::datatable(stodvar,rownames = T, caption = "2015")


















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






