library(dplyr)
library(devtools)
devtools::install_github("gaospecial/ggVennDiagram")
library("ggVennDiagram")
library("UpSetR")
landuse <- read.csv("landuse/assessments.csv")
climate <- read.csv("climate/assessments.csv")
invasion <- read.csv("invasion/assessments.csv")
overexploitation <- read.csv("overexploitation/assessments.csv")
pollution <- read.csv("pollution/assessments.csv")
#We want to combine all the data together.
climate$landuse<-0
climate$invasion<-0
climate$overexploitation<-0
climate$pollution<-0
climate$climate<- 1
invasion$landuse<-0
invasion$invasion<-1
invasion$overexploitation<-0
invasion$pollution<-0
invasion$climate<- 0
landuse$landuse<-1
landuse$invasion<-0
landuse$overexploitation<-0
landuse$pollution<-0
landuse$climate<- 0
pollution$landuse<-0
pollution$invasion<-0
pollution$overexploitation<-0
pollution$pollution<-1
pollution$climate<- 0
overexploitation$landuse<-0
overexploitation$invasion<-0
overexploitation$overexploitation<-1
overexploitation$pollution<-0
overexploitation$climate<- 0
threats.dat <- rbind(landuse,climate,invasion,overexploitation,pollution)
#Some species have multiple threats so they appear multiple times in the dataframe
#See how many unique species there are
length(unique(threats.dat$scientificName))
#Check which rows are duplicated
threats.dup<-which(duplicated(threats.dat$scientificName))
#Within these some are still repeated due to having more than 2 threats?
threats.redundant<-distinct(threats.dat[threats.dup,1:23])
#Merge duplicated rows
for(i in 1:nrow(threats.redundant)){
  #For every row of the redundant dataframe say in which rows of the original threats.dat
  #can the repeated species be found
  row.number<- which(threats.dat$scientificName==threats.redundant$scientificName[i])
  row.selected<- threats.dat[row.number,]
  # MAkes a single row with all teh info found in collums 1:23
  # and then adds the collums 24:28 which contain the 0 and 1 for the extinction cause as a sum so if
  # for one of teh repeats landuse was 0 but for the other landuse was 1 it adds it making it one, does that for all threats
  new.row <- data.frame(c(row.selected[1,1:23],colSums(row.selected[,24:28])))
  #Eliminates all repeated rows
  threats.dat <- threats.dat[-row.number,]
  #Adds the newly made row
  threats.dat  <- rbind(threats.dat,new.row)
}
#Visualize which reason is more common for the extinction of species
boxplot(c(sum(threats.dat$landuse),sum(threats.dat$pollution),sum(threats.dat$overexploitation),sum(threats.dat$invasion),sum(threats.dat$climate))~c("landuse","pollution","overexploitation","invasion","climate"))
x <- list(landuse=landuse$scientificName,
          overexploitation=overexploitation$scientificName,
          invasion=invasion$scientificName,
          pollution=pollution$scientificName,
          climate=climate$scientificName)
#Visualize the intersections of data
ggVennDiagram(x,label_alpha=0)
upset(data=threats.dat[c(3,24:28)],nsets=5)
#Look at the taxonomic groups
landusetaxonomy <- read.csv("landuse/taxonomy.csv")
climatetaxonomy <- read.csv("climate/taxonomy.csv")
invasiontaxonomy <- read.csv("invasion/taxonomy.csv")
overexploitationtaxonomy <- read.csv("overexploitation/taxonomy.csv")
pollutiontaxonomy <- read.csv("pollution/taxonomy.csv")
all.tax <- rbind(landusetaxonomy,climatetaxonomy,invasiontaxonomy,overexploitationtaxonomy,pollutiontaxonomy)
all.tax <- distinct(all.tax)
all.tax <- all.tax[order(all.tax$scientificName),]
threats.dat <- threats.dat[order(threats.dat$scientificName),]
threats.dat.mammals <- threats.dat[which(all.tax$className=="MAMMALIA"),]
threats.dat.birds <- threats.dat[which(all.tax$className=="AVES"),]
threats.dat.fish <- threats.dat[which(all.tax$className=="ACTINOPTERYGII"),]
threats.dat.amphibian <- threats.dat[which(all.tax$className=="AMPHIBIA"),]
threats.dat.gastropode <- threats.dat[which(all.tax$className=="GASTROPODA"),]
threats.dat.flower <- threats.dat[which(all.tax$className=="MAGNOLIOPSIDA"),]
threats.dat.reptile <- threats.dat[which(all.tax$className=="REPTILIA"),]

upset(data=threats.dat[c(3,24:28)],nsets=5)

upset(data=threats.dat.mammals[c(3,24:28)],nsets=5)

upset(data=threats.dat.birds[c(3,24:28)],nsets=5)
upset(data=threats.dat.fish[c(3,24:28)],nsets=5)
upset(data=threats.dat.amphibian[c(3,24:28)],nsets=5)
upset(data=threats.dat.gastropode[c(3,24:28)],nsets=5)
upset(data=threats.dat.flower[c(3,24:28)],nsets=5)
upset(data=threats.dat.reptile[c(3,24:28)],nsets=5)