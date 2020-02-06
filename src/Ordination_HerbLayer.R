file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "11"

library(vegan)
#-------------------------------------

herbals_eigene <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
herbals_andere <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals_ANDERE.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
#-------------------------------------

# 1 restructure herbals andere

herbals_andere$plot_alt <- NULL
herbals_andere$Note <- NULL
names(herbals_andere)[2] <- "species"
names(herbals_andere)[3] <- "lat"
names(herbals_andere)[4] <- "coverage"

################################################
#r weniger 5%		0,01
#+ weniger 5%		0,1
#1 weniger 5%		1,0 + 2,0 + 3,0
#2 5-25%			  5,0 + 10,0 + 15,0 + 20,0 
#3 25-50%		    25,0 + 35,0 + 40,0 
#4 50-75%       50,0
#5 75-100%
################################################

#rename the coverage
herbals_andere$coverage_correct <- NA

for(i in 1:nrow(herbals_andere)){
  if(herbals_andere$coverage[i] == 0.01){
    herbals_andere$coverage_correct[i] <- "r"
  }else if(herbals_andere$coverage[i] == 0.1){
    herbals_andere$coverage_correct[i] <- "+"
  }else if(herbals_andere$coverage[i] < 5 &  herbals_andere$coverage[i] > 0.1){
    herbals_andere$coverage_correct[i] <- "1"
  }else if(herbals_andere$coverage[i] < 25 &  herbals_andere$coverage[i] > 3){
    herbals_andere$coverage_correct[i] <- "2"
  }else if(herbals_andere$coverage[i] < 50 &  herbals_andere$coverage[i] > 20){
    herbals_andere$coverage_correct[i] <- "3"
  }else if(herbals_andere$coverage[i] == 50){
    herbals_andere$coverage_correct[i] <- "4"
  }
}

names(herbals_andere)[5] <- names(herbals_andere)[4]
herbals_andere[,4] <- NULL

#-------------------------------------

# 2 restructure herbals_eigene

herbals_eigene$comment <- NULL
herbals_eigene$foto <- NULL
herbals_eigene <- herbals_eigene[herbals_eigene$processing_information != "ignorieren",]
herbals_eigene$processing_information <- NULL
names(herbals_eigene)[1] <- names(herbals_andere)[1]

#change plot ID
for(i in 1:nrow(herbals_eigene)){
  herbals_eigene$plotID[i] <- paste0("fs-05", herbals_eigene$plotID[i])
}

herbals_eigene <- herbals_eigene[!is.na(herbals_eigene$lat),]
#-------------------------------------

# 3 combine the two datasets

herbals <- rbind(herbals_andere, herbals_eigene)
rm(herbals_andere, herbals_eigene)

#write.csv(herbals, paste0(file_base, paste0("processed/herbals_vers", currentVersion, ".csv")), row.names = FALSE)
herbals <- read.csv(paste0(file_base, "processed/herbals_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)
#-------------------------------------

# 4 reshape the dataframe

#assing a metrical scale level
for(i in 1:nrow(herbals)){
  if(herbals$coverage[i] == "r"){
    herbals$coverage[i] <- 0.01
  }else if(herbals$coverage[i] == "+"){
    herbals$coverage[i] <- 0.1
  }else if(herbals$coverage[i] == "1"){
    herbals$coverage[i] <- 1
  }else if(herbals$coverage[i] == "2"){
    herbals$coverage[i] <- 15
  }else if(herbals$coverage[i] == "3"){
    herbals$coverage[i] <- 37.5
  }else if(herbals$coverage[i] == "4"){
    herbals$coverage[i] <- 62.5
  }else{
    herbals$coverage[i] <- 87.5
  }
}
herbals$coverage <- as.numeric(herbals$coverage)

allSpecies <- unique(herbals$lat)
processHerb <- matrix(nrow = length(unique(herbals$plotID)), ncol = length(allSpecies))
colnames(processHerb) <- allSpecies
rownames(processHerb) <- unique(herbals$plotID)

#iterate over the plot locations
for(i in 1:nrow(processHerb)){
  temp_herb <- herbals[herbals$plotID == unique(herbals$plotID)[i],]
  #iterate over the species
  for(j in 1: ncol(processHerb)){
    count <- temp_herb$lat == colnames(processHerb)[j]
    processHerb[i,j] <- sum(count, na.rm = TRUE)
  }
}
#to df
processHerb <- as.data.frame(processHerb)

#write.csv(processHerb, paste0(file_base, paste0("processed/herbals_vers", currentVersion, ".csv")), row.names = FALSE)
processHerb <- read.csv(paste0(file_base, "processed/herbals_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)

#-------------------------------------

# 5 start with the ordination

#first try
ca <- cca(processHerb)
plot(ca,display="sites")

ca <- decorana(processHerb)
plot(ca,display="sites")

#value distribution
for(i in 1:ncol(processHerb)){print(unique(processHerb[,i]))}
#only the distribution values 0, 1 and 2 are present. No species can be found more often. 

#->> values are not suited for analysis

#second try with half of the dataset
processHerb_half <- processHerb[,1:(0.5*ncol(processHerb))]
processHerb_half <- processHerb[apply(processHerb_half, 1, sum) != 0]

ca <- cca(processHerb_half)
plot(ca,display="sites")

ca <- decorana(processHerb_half)
plot(ca,display="sites")


#->> values are still not suited for analysis, though the distribution after decorana appears improved 


