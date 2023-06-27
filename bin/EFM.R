####GREEN LAKE AND SALT LAKE WATER####

#library
library(ggplot2)
library(dplyr)
library(reshape)
library(tidyverse)
library(wesanderson)
names(wes_palettes)
library(RColorBrewer)
#display.brewer.all(colorblindFriendly = T)

####GREEN LAKE AND SALT LAKE WATER####

fglw <- 995057.8963
fglw_cv <- 0.3879462248

gslw <- 1434441.777
gslw_cv <- 0.1915141408

#Natural water with CV error bars
waterdf <- data.frame(concentration = c(fglw, gslw), Location = c("Green Lake", "Salt Lake"), cv=c(fglw_cv, gslw_cv))

pn <- ggplot(waterdf, aes(x= Location, y= concentration, fill = Location, width= 0.8))+ggtitle("Water")
pn <- pn + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
pn <- pn + geom_errorbar(aes(x= "Green Lake", ymin=fglw-(fglw*fglw_cv), ymax= fglw+(fglw*fglw_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
pn <- pn + geom_errorbar(aes(x= "Salt Lake", ymin=gslw-(gslw*gslw_cv), ymax=gslw+(gslw*gslw_cv)), width=0.1, colour= "grey", alpha=0.9, size=0.5)
pn <- pn + labs(x= "Location", y= "Concentration (VLP/mL)")
pn <- pn + theme_classic()
pn <- pn + theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
pn


####GREEN LAKE AND SALT LAKE EPS####

fgleps <- 12452341.84
fgleps_cv <- 0.3526190621

gsleps <- 13068009.14
gsleps_cv <- 0.3440039759

#create data frame 
epsdf <- data.frame(concentration = c(fgleps, gsleps), Location = c("Green Lake", "Salt Lake"), cv= c(fglweps_cv, gsleps_cv))

#EPS plot with CV error bars 
ep <- ggplot(epsdf, aes(x=Location, y=concentration, fill = Location, width = 0.8))+ggtitle("EPS")
ep <- ep + geom_bar(stat="identity")
ep <- ep + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
ep <- ep + labs(x= "Location", y= "Concentration (VLP/g)")
ep <- ep + geom_errorbar(aes(x= "Green Lake", ymin= fgleps-(fgleps*fgleps_cv), ymax= fgleps+(fgleps*fgleps_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
ep <- ep + geom_errorbar(aes(x= "Salt Lake", ymin=gsleps-(gsleps*gsleps_cv), ymax=gsleps+(gsleps*gsleps_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
ep <- ep + theme_classic()
ep <- ep + theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
ep


####GREEN LAKE AND SALT LAKE MAT####

fglm <- 8021540.087
fglm_cv <- 0.2132320356

gslm <- 8715091.921
gslm_cv <- 0.2532708777

#create data frame 
matdf <- data.frame(concentration = c(fglm, gslm), Location = c("Green Lake", "Salt Lake"), cv= c(fglm_cv, gslm_cv))

#Mat plot with CV error bars
mp <- ggplot(matdf, aes(x=Location, y=concentration, fill = Location, width = 0.8, main="Mat"))+ggtitle("Mat")
mp <- mp + geom_bar(stat="identity")
mp <- mp + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
mp <- mp + labs(x= "Location", y= "Concentration (VLP/g)")
mp <- mp + geom_errorbar(aes(x= "Green Lake", ymin=fglm-(fglm*fglm_cv), ymax=fglm+(fglm*fglm_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
mp <- mp + geom_errorbar(aes(x= "Salt Lake", ymin=gslm-(gslm*gslm_cv), ymax=gslm+(gslm*gslm_cv)), width=0.1, colour= "grey", alpha=0.9, size=0.5)
mp <- mp + theme_classic()
mp <- mp + theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
mp



####ALL####

#create data frame of all
alldf <- data.frame(concentration = c(fglw, gslw, fgleps, gsleps, fglm, gslm), Sample = c("Green Lake Water", "Salt Lake Water", "Green Lake EPS", "Salt Lake EPS", "Green Lake Mat", "Salt Lake Mat"), cv= c(fglw_cv, gslw_cv, fgleps_cv, gsleps_cv, fglm_cv, gslm_cv))

#plot of everything
ap <- ggplot(alldf, aes(x=factor(Sample, level=c("Green Lake Water", "Salt Lake Water", "Green Lake EPS", "Salt Lake EPS", "Green Lake Mat", "Salt Lake Mat")), y=concentration, fill = Sample, width = 0.8))+
  ggtitle("All")
ap <- ap + geom_bar(stat="identity")
ap <- ap + geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2")
ap <- ap + labs(x= "Sample", y= "Concentration (VLP/mL or VLP/g)")
ap <- ap + geom_errorbar(aes(x= "Green Lake Water", ymin=fglw, ymax= fglw+(fglw*fglw_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
ap <- ap + geom_errorbar(aes(x= "Salt Lake Water", ymin=gslw, ymax=gslw+(gslw*gslw_cv)), width=0.1, colour= "grey", alpha=0.9, size=0.5)
ap <- ap + geom_errorbar(aes(x= "Green Lake EPS", ymin= fgleps, ymax= fgleps+(fgleps*fgleps_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
ap <- ap + geom_errorbar(aes(x= "Salt Lake EPS", ymin=gsleps, ymax=gsleps+(gsleps*gsleps_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
ap <- ap + geom_errorbar(aes(x= "Green Lake Mat", ymin=fglm, ymax=fglm+(fglm*fglm_cv)), width=0.1, colour="grey", alpha=0.9, size=0.5)
ap <- ap + geom_errorbar(aes(x= "Salt Lake Mat", ymin=gslm, ymax=gslm+(gslm*gslm_cv)), width=0.1, colour= "grey", alpha=0.9, size=0.5)
ap <- ap + theme_classic()
ap <- ap + theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("FGL Water", "GSL Water", "FGL EPS", "GSL EPS", "FGL Mat", "GSL Mat"))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_fill_discrete(breaks=c("Green Lake Water", "Salt Lake Water", "Green Lake EPS", "Salt Lake EPS", "Green Lake Mat", "Salt Lake Mat"))
ap



