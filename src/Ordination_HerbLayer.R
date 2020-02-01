file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "11"

library(vegan)
library(reshape2)
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

#2 restructure herbals_eigene

herbals_eigene$comment <- NULL
herbals_eigene$foto <- NULL
herbals_eigene <- herbals_eigene[herbals_eigene$processing_information != "ignorieren",]
herbals_eigene$processing_information <- NULL
names(herbals_eigene)[1] <- names(herbals_andere)[1]

#change plot ID
for(i in 1:nrow(herbals_eigene)){
  herbals_eigene$plotID[i] <- paste0("fs-05", herbals_eigene$plotID[i])
}

#-------------------------------------

#3 combine the two datasets

herbals <- rbind(herbals_andere, herbals_eigene)
rm(herbals_andere, herbals_eigene)

write.csv(herbals, paste0(file_base, paste0("processed/herbals_vers", currentVersion, ".csv")), row.names = FALSE)
herbals <- read.csv(paste0(file_base, "processed/herbals_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)
#-------------------------------------

#4 start with the ordination



test <- herbals[herbals$plotID == unique(herbals$plotID)[1],]
test1 <- test
test1$lat <- NULL
test1$plotID <- NULL
test1$ID <- 1:nrow(test1)
test1 <- cbind(test1[3], test1[1:2])

reshape2::acast(test1, ID ~ species)

#restructure the df
test <- herbals
test$ID <- 1:nrow(test)
test <- cbind(test[5], test[1:4])
melttest <- melt(test, id.vars = "ID")

reshape2::acast(melttest, melttest$variable ~ melttest$value)



names <- unique(herbals$species)

herbals[,5:114] <- NA
names(herbals)[5:114] <- names

for(i in 1:nrow(herbals)){
  for(j in 5:114){
    if(herbals$species[i] == names(herbals)[j]){
      herbals[i,j] <- herbals$coverage[i]
    }else{
      herbals[i,j] <- 0
    }
  }
}


for(i in 1:unique(herbals$plotID)){
  temp_df <- herbals[herbals$plotID == unique(herbals$plotID)[i],]
  
}



test <- herbals[herbals$plotID == "fs-058",]

neuerDF <- unique(herbals$plotID)
neuerDF <- data.frame(plotID = neuerDF, Art1 = NA)
neuerDF[,3:111] <- NA
names(neuerDF)[2:111] <- names

for(i in 1:length(unique(herbals$plotID))){
  for(j in 2:ncol(neuerDF)){
    if(herbals$)
  }
}

unique(test$Waldehrenpreis)
