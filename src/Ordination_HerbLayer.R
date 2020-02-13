file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "11"

library(vegan)
#-------------------------------------
#load the data
#herbal layer
herbals_eigene <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
herbals_andere <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals_ANDERE.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)

#general
general <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_general.csv")), sep = ";", stringsAsFactors = FALSE)
general <- general[1:8,]
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

#-------------------------------------

# 5  assign the coverage of the three layers (tree, shrub, herbal)

layerInfo <- cbind(general[2], general[10:12])

rm(general)

gap <- data.frame(x = rep(NA, nrow(processHerb)-(nrow(layerInfo))))

gap <- cbind(gap, gap, gap, gap)
names(gap) <- names(layerInfo)

layerInfo <- rbind(gap, layerInfo)
rm(gap)

processHerb <- cbind(processHerb, layerInfo[2:4])

rm(layerInfo)
#-------------------------------------

# 6 add the counts

#iterate over the plot locations
for(i in 1:nrow(processHerb)){
  temp_herb <- herbals[herbals$plotID == unique(herbals$plotID)[i],]
  #iterate over the species
  for(j in 1:(ncol(processHerb)-3)){
    count <- temp_herb$lat == colnames(processHerb)[j]
    processHerb[i,j] <- sum(count, na.rm = TRUE)
  }
}
#to df
processHerb <- as.data.frame(processHerb)

#write.csv(processHerb, paste0(file_base, paste0("processed/herbals_vers", currentVersion, ".csv")), row.names = FALSE)
processHerb <- read.csv(paste0(file_base, "processed/herbals_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)

#-------------------------------------

# 7 start with the ordination

processHerb <- processHerb[(44-7):44,]

#first try
ca <- cca(processHerb)
ca
plot(ca,display="sites")

dca <- decorana(processHerb)
dca
plot(dca,display="sites")

#value distribution
for(i in 1:ncol(processHerb)){print(unique(processHerb[,i]))}
#only the coverage distribution shows more variability 
#without the coverage, the data doesn't contain suitable data



