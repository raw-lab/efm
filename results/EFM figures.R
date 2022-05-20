####GREEN LAKE AND SALT LAKE WATER####

#library
library(ggplot2)
library(dplyr)
library(reshape)
library(tidyverse)
library(wesanderson)
names(wes_palettes)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)

#Set working directory 
getwd()
setwd("/home/qmurphy2/R")

####GREEN LAKE AND SALT LAKE WATER####

#Natural water with CV error bars (updated 5_20_22)
natdf <- data.frame(concentration = c(6300823.212, 10696658.97), Location = c("Green Lake", "Salt Lake"), cv=c(0.2715, 0.2510))
natdf
pn <- ggplot(natdf, aes(x= Location, y= concentration, fill = Location, width= 0.8))
pn <- pn + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
pn <- pn + geom_errorbar(aes(x= "Green Lake", ymin= 4590440.724, ymax= 8011205.699), width=0.3, colour="grey", alpha=0.9, size=1.3)
pn <- pn + geom_errorbar(aes(x= "Salt Lake", ymin= 8010460.146, ymax= 13382857.8), width=0.3, colour= "grey", alpha=0.9, size=1.3)
pn <- pn + labs(x= "Location", y= "Concentration (VLP/mL)")
pn <- pn + theme_classic()
pn <- pn + theme(text = element_text(size=18))
pn


####GREEN LAKE AND SALT LAKE EPS####

#create data frame 
epsdf <- data.frame(concentration = c(3343554487, 1135649922, 2829560863), Location = c("Green Lake (4C)", "Green Lake (-80C)", "Salt Lake (-80C)"), cv= c(0.234, 0.596, 0.3108))
epsdf

#EPS plot with CV error bars (updated 5_20_22)
ep <- ggplot(epsdf, aes(x=Location, y=concentration, fill = Location, width = 0.8))
ep <- ep + geom_bar(stat="identity")
ep <- ep + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
ep <- ep + labs(x= "Location", y= "Concentration (VLP/g)")
ep <- ep + geom_errorbar(aes(x= "Green Lake (4C)", ymin= 2561184863, ymax= 4125924111), width=0.3, colour="grey", alpha=0.9, size=1.3)
ep <- ep + geom_errorbar(aes(x= "Green Lake (-80C)", ymin= 460396379.2, ymax= 1810903465), width=0.3, colour="grey", alpha=0.9, size=1.3)
ep <- ep + geom_errorbar(aes(x= "Salt Lake (-80C)", ymin= 1950116331, ymax= 3709005394), width=0.3, colour= "grey", alpha=0.9, size=1.3)
ep <- ep + theme_classic()
ep <- ep + theme(text = element_text(size=18))
ep


####GREEN LAKE AND SALT LAKE MAT####


#create data frame 
matdf <- data.frame(concentration = c(1542009902, 706920248.5, 793786479.5, 1459443129), Location = c("Green Lake (4C)", "Green Lake (4C + benzonase)", "Green Lake (-80C)", "Salt Lake (-80C)"), cv= c(0.3009, 0.1083, 0.211 ,0.3397))
matdf

#Mat plot with CV error bars (updated 5_20_22)
mp <- ggplot(matdf, aes(x=Location, y=concentration, fill = Location, width = 0.8))
mp <- mp + geom_bar(stat="identity")
mp <- mp + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
mp <- mp + labs(x= "Location", y= "Concentration (VLP/g)")
mp <- mp + geom_errorbar(aes(x= "Green Lake (4C)", ymin= 1078013469, ymax= 2006006336), width=0.3, colour="grey", alpha=0.9, size=1.3)
mp <- mp + geom_errorbar(aes(x= "Green Lake (4C + benzonase)", ymin= 630313232.7, ymax= 783527264.3), width=0.3, colour="grey", alpha=0.9, size=1.3)
mp <- mp + geom_errorbar(aes(x= "Green Lake (-80C)", ymin= 626279050.2, ymax= 961293908.9), width=0.3, colour="grey", alpha=0.9, size=1.3)
mp <- mp + geom_errorbar(aes(x= "Salt Lake (-80C)", ymin= 963615125.6, ymax= 1955271133), width=0.3, colour= "grey", alpha=0.9, size=1.3)
mp <- mp + theme_classic()
mp <- mp + theme(text = element_text(size=18))
mp



####Preservation plot (FGL)

#create data frame 
presdf <- data.frame(concentration = c(297041337.4, 733402271.7, 1572390309, 2121898760), Location = c("Mat (2% glut.)", "Mat (2% glut. + 25% glycerol)", "EPS (2% glut.)", "EPS (2% glut. + 25% glycerol)"), cv= c(0.447, 0.1362, 0.3276 ,0.2797))
presdf

#Preservation plot with CV error bars (updated 5_20_22)
pp <- ggplot(presdf, aes(x=Location, y=concentration, fill = Location, width = 0.8))
pp <- pp + geom_bar(stat="identity")
pp <- pp + geom_bar(stat="identity") + scale_fill_brewer(palette = "Paired")
pp <- pp + labs(x= "Sample and Preservation Method", y= "Concentration (VLP/g)")
pp <- pp + geom_errorbar(aes(x= "Mat (2% glut.)", ymin= 164126259.1, ymax= 429956415.8), width=0.3, colour="grey", alpha=0.9, size=1.3)
pp <- pp + geom_errorbar(aes(x= "Mat (2% glut. + 25% glycerol)", ymin= 633506324.7, ymax= 833298218.7), width=0.3, colour="grey", alpha=0.9, size=1.3)
pp <- pp + geom_errorbar(aes(x= "EPS (2% glut.)", ymin= 1057186446, ymax= 2087594172), width=0.3, colour="grey", alpha=0.9, size=1.3)
pp <- pp + geom_errorbar(aes(x= "EPS (2% glut. + 25% glycerol)", ymin= 1528223681, ymax= 2715573839), width=0.3, colour= "grey", alpha=0.9, size=1.3)
pp <- pp + theme_classic()
pp <- pp + theme(text = element_text(size=18))
pp




####Benzonase/EDTA EPS plot (FGL)

#create data frame 
bedf <- data.frame(concentration = c(4716861709, 5022186337, 4708975991, 7811122197, 8550222405, 10875305501), Location = c("Probe + EDTA", "Probe + benz.", "Probe + EDTA + benz.", "Vortex + EDTA", "Vortex + benz.", "Vortex + EDTA + benz."), cv= c(0.2798, 0.2107, 0.2425 ,0.2750, 0.2843, 0.1766))
bedf

#Preservation plot with CV error bars (updated 5_20_22)
bep <- ggplot(bedf, aes(x=Location, y=concentration, fill = Location, width = 0.8))
bep <- bep + geom_bar(stat="identity")
bep <- bep + geom_bar(stat="identity") + scale_fill_brewer(palette = "BrBG")
bep <- bep + labs(x= "Treatment", y= "Concentration (VLP/g)")
bep <- bep + geom_errorbar(aes(x= "Probe + EDTA", ymin= 3397188371, ymax= 6036535047), width=0.3, colour="grey", alpha=0.9, size=1.3)
bep <- bep + geom_errorbar(aes(x= "Probe + benz.", ymin= 3963955841, ymax= 6080416832), width=0.3, colour="grey", alpha=0.9, size=1.3)
bep <- bep + geom_errorbar(aes(x= "Probe + EDTA + benz.", ymin= 3567092954, ymax= 5850859029), width=0.3, colour="grey", alpha=0.9, size=1.3)
bep <- bep + geom_errorbar(aes(x= "Vortex + EDTA", ymin= 5662751918, ymax= 9959492477), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bep <- bep + geom_errorbar(aes(x= "Vortex + benz.", ymin= 6119301270, ymax= 10981143541), width=0.3, colour="grey", alpha=0.9, size=1.3)
bep <- bep + geom_errorbar(aes(x= "Vortex + EDTA + benz.", ymin= 8954656012, ymax= 12795954989), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bep <- bep + theme_classic()
bep <- bep + theme(text = element_text(size=18))
bep



















####Beads 
bdf <- data.frame(concentration= c(88614518.79, 137150271.8, 314838928.3, 632435569.8), Dilution =c("1:50", "1:10", "1:5", "No Dilution"))
bdf

#bead plot 
bp <- ggplot(bdf, aes(fct_reorder(Dilution, concentration), concentration, fill = Dilution)) + labs(x = "Dilution", y= "Concentration (beads/mL")
bp <- bp + geom_bar(stat="identity") + scale_fill_manual(limits = c("1:50", "1:10", "1:5", "No Dilution"), values=wes_palette("IsleofDogs2", n=4, type = "continuous")) 
bp <- bp + labs(x= "Dilution", y= "Concentration (beads/mL)")
bp <- bp + geom_errorbar(aes(x= "1:50", ymin= 68584858.84, ymax= 108644178.7), width=0.3, colour="grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "1:10", ymin= 113021191, ymax= 161279352.6), width=0.3, colour="grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "1:5", ymin= 254976930.5, ymax= 374700926), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "No Dilution", ymin= 506274716.6, ymax= 758596423), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bp <- bp + theme_classic()
bp <- bp + theme(text = element_text(size=18))
bp


###Beads (just no dilution)
ndbdf <- data.frame(concentration= c(1971710894), Dilution= c("Control Beads"), sd= c(157721104.7), cv= c(0.07999200347))
ndbdf

#No dilution bead plot 
ndbp <- ggplot(ndbdf, aes(fct_reorder(Dilution, concentration), concentration, fill = Dilution)) + labs(x = "Dilution", y= "Concentration (beads/mL)")
ndbp <- ndbp + geom_bar(stat="identity") + scale_fill_manual(limits = c("Control Beads"), values=wes_palette("Zissou1", n=4, type = "continuous")) 
ndbp <- ndbp + labs(x= "Control Beads", y= "Concentration (beads/mL)")
ndbp <- ndbp + geom_errorbar(aes(x= "Control Beads", ymin= 1813989789, ymax= 2129431999), width=0.3, colour= "grey", alpha=0.9, size=1.3)
ndbp <- ndbp + theme_classic()
ndbp <- ndbp + theme(text = element_text(size=18))
ndbp






#tilt x-axis labels 
bp <- bp + theme(axis.text.x = element_text(angle=45, hjust=0.9))






####Extra unused in paper####

#data upload
water <- read.table(file = 'avgwater.tsv', sep = '\t', header = TRUE)
str(water)

#create data frame
waterdf <- data.frame(concentration = c(1519806408, 6000000, 856889447.4, 5713333.333), location = c("Green Lake concentrated", "Green Lake natural", "Salt Lake concentrated", "Salt Lake natural"), sd=c(1098136178, 311092822.7))

#view data frame 
waterdf 
waterdf <- as.numeric(waterdf$location) 

####GREEN LAKE AND SALT LAKE WATER PLOTS####
#plot with both concentrated and natural 
p <- ggplot(water, aes(x=location, y=concentration, fill = location, width = 0.6))
p <- p + geom_bar(stat="identity")
p <- p + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
p <- p + labs(x= "Location", y= "Concentration (VLP/mL)")
p
##issues with scale

#plot with only concentrated (water)
condf <- data.frame(concentration = c(1519806408, 856889447.4), location = c("Green Lake concentrated", "Salt Lake concentrated"), sd=c(1098136178, 311092822.7))
condf
pc <- ggplot(condf, aes(x= location, y= concentration, fill = location, width= 0.6))
pc <- pc + geom_bar(stat="identity")
pc <- pc + labs(x= "Location", y= "Concentration (VLP/mL)")
pc

###Beads with 1:50, 1:10, 1:5, 2:5 and no dilution###
bp <- ggplot(bdf, aes(x=Dilution, y=concentration, fill = Dilution, width = 0.8))
bp <- bp + geom_bar(stat="identity")
bp <- bp + geom_bar(stat="identity") + scale_fill_brewer(palette = "Paired")
bp <- bp + labs(x= "Dilution", y= "Concentration (beads/mL)")
bp <- bp + geom_errorbar(aes(x= "1:50", ymin= 68584858.84, ymax= 108644178.7), width=0.3, colour="grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "1:10", ymin= 113021191, ymax= 161279352.6), width=0.3, colour="grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "1:5", ymin= 254976930.5, ymax= 374700926), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "2:5", ymin= 564220788.8, ymax= 713519679.3), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bp <- bp + geom_errorbar(aes(x= "No Dilution", ymin= 506274716.6, ymax= 758596423), width=0.3, colour= "grey", alpha=0.9, size=1.3)
bp <- bp + theme_bw()
bp <- bp + theme(text = element_text(size=18))
bp
