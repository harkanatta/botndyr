#Glæðitap í Kolgrafafirði

library(plyr)
library(googlesheets)
library(RColorBrewer)
library(plotrix)

#skjal <- gs_url("https://docs.google.com/spreadsheets/d/1ZUSAZFnTKmTrpMDmfsBLH-QP1n8b65cH4uL3VuJ8Oqs/edit?usp=sharing")
#gledi <- gs_read_csv(skjal)
#gledi <- gledi[gledi$stod!="B7" & gledi$stod!="U1" & gledi$stod!="U3" & gledi$stod!="B8",]
gledi <- read.csv("./set/glaedi.csv", check.names = F)
gledi <- as.data.frame(gledi[,-1])
gledi$stod <- as.factor(gledi$stod)

xgildi <- tapply(gledi$stod,gledi$ar,function(x) return(x))
ygildi <- tapply(gledi$tap ,gledi$ar,function(x) return(x))
stadalfravik <- tapply(gledi$sd,gledi$ar,function(x) return(x))
litir <- brewer.pal(5,"Set1")

png('~/GleditapMedStadalfravikum.png',16,7,"cm",pointsize=8,res=900)
plotCI(as.numeric(gledi$stod),gledi$tap, ui=gledi$tap+gledi$sd, li=gledi$tap-gledi$sd,col=litir,xaxt='n',yaxt='n',pch=NA,xlab = 'Stöð',ylab='Glæðitap (%)')
abline(h=seq(0.05,0.30,0.30/6), col = 'lightgray', lty = 3)
mapply(points,xgildi,ygildi,col=1, bg=litir ,pch=21:25,type="p")
legend("topleft", legend=unique(gledi$ar),pch=c(21:25), col = 1, pt.bg=litir , horiz = T)
axis(1,at=1:length(unique(gledi$stod)),labels=levels(gledi$stod))
axis(2,at=seq(0.05,0.35,0.35/7),labels = c(5,10,15,20,25,30,35),las=2)
dev.off()

#ggplot útgáfa
library(ggplot2)

gledi_plot <- ggplot(data = gledi, aes(x = ar, y = tap, color = stod, shape = stod)) +
  geom_point(size = 3, alpha = 0.8) + # Adjusting point size and transparency
  geom_errorbar(aes(ymin = tap - sd, ymax = tap + sd), width = 0.2) + # Adding error bars
  labs(title = "Glæðitap - Kolgrafafirði", x = "", y = "", color = "Stöð", shape = "Stöð") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) # Adding percentage formatting to the y-axis

print(gledi_plot)



#Gögnin lesin inn, skeytt saman og staðalfrávik reiknað
library(magrittr)
glaedi <- read.csv("./set/gleditap.csv", check.names = F)
glaedi[37:38,"stod"] <- c("B8","B8")
glaedi[37:38,"ar"] <- c("2016","2016")
glaedi2013 <- read.csv("./set/gledi2013.csv", check.names = F)
#glaedi[,3:5] <- glaedi[,3:5]*0.001
glaedi <- glaedi[glaedi$fyrir!=0,]
glaedi <- rbind(glaedi2013,glaedi)
glaedi <- glaedi[order(glaedi$ar),]
glaedi <- glaedi %>% 
   filter(stod %in% c("C4", "A7", "B5", "B8", "E4", "E3"))  %>% 
  mutate(gltap = (deigla+fyrir-eftir)/(fyrir)) %>% 
  ddply(.(ar,stod),summarize,tap=sum(gltap)/2, sd=sd(gltap))





botngerd <- structure(list(
  Stod = structure(c(1L, 2L, 3L, 4L, 5L, 6L), levels = c("A7", "B5", "B8", "C4", "E3", "E4"), class = "factor"),
  Dypi = c(16.6, 24.5, 16.6, 34, 11.2, 11), 
  Sild = c(0L, 1L, 0L, 0L, 1L, 1L),
  Organic = c(0.13414426, 0.12774569, 0.13054132, 0.11744032, 0.1150086, 0.11178908)),
  row.names = c(NA, -6L), class = "data.frame")
#botngerd <- botngerd[,-1]


#athuga með dýpi og glæðitap
breytur <- read.csv2("./set/urvinnsla_breytur.csv",dec = ".", encoding = "latin1")
breytur <- breytur[c(8,10:14),]
Tap <- ddply(glaedi,.(stod),summarise,tap=mean(tap))
breytur <- cbind(breytur,Tap[c(1,3:7),2])
colnames(breytur)[5] <- "Tap"
breytur <- breytur[,-1]
plot(breytur$Dýpi,breytur$Tap,type='n')
text(breytur$Dýpi,breytur$Tap,breytur$Stöð)
