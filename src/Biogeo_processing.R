#Script for processing bio-geography data from the core plots and calculation of 
# biodiversity metrics

####Before using this script:##################

#1 the file base has to be changed to the personal directory
#2 the loaded .csv files in the "org" folder were extracted from the current version of the "Strukturplots_Tabelle"
#  every single excel sheet was extracted (just a copy) in a new excel file and was saved as .csv using the respective name
#3 the name of the current version has to be filled in (with a leading zero for one-digit numbers -> 05 instead of 5)

###############################################

workingDir <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#workingDir <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "11"

library(stringr)

#-------------------------------------------------------------------------------
#1 READ THE DATA
#-------------------------------------------------------------------------------
#1.1 Meta data (general)
#------------------------
#general <- read.csv(paste0(workingDir, paste0("org/Vers", currentVersion, "_general.csv")), sep = ";", stringsAsFactors = FALSE)
#general <- general[1:8,]
#trees
#load the processed table at 2.1
# previousTrees <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_treesPreviousSemesters.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE)
# trees <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_trees.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
# treePlot <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_treesPlot.csv"), sep = ";", dec = ".", stringsAsFactors = FALSE)
# #assign trees of current semester to their plot ID
# #remove unneccessary cols
# trees$statusID <- NULL
# trees$plot <- NA
# 
# for(i in 1:nrow(trees)){
#   trees$plot[i] <- treePlot$plotID[treePlot$treeID == trees$treeID[i]]
# }
# rm(treePlot)
# 
# #assign ID column
# trees$ID <- seq(from = 1077, to = as.numeric(1077+nrow(trees)-1), by = 1)
# trees <- cbind(trees[10], trees[1:9])
# 
# #bind previous and current semester data together
# trees <- rbind(previousTrees, trees)
# 
# rm(previousTrees)

#1.2 Young trees
#----------------
#youngTrees <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_youngTrees.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
#youngTrees <- youngTrees[1:15,]

#1.3 Herbals
#------------
#herbals <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_herbals.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)

#1.4 Deathwoood
#---------------
#load the processed table at 2.2
# deathwood <- read.csv(paste0(workingDir, paste0("org/Vers", currentVersion, "_deathwood.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)
# #temporary adjustment------------
# plotnumberAdjust <- unique(deathwood$plot)
# 
# newPlotnumber <- NA
# for(i in 1:length(plotnumberAdjust)){
#   newPlotnumber[i] <- paste0("fs-05", i)
# }
# 
# for(i in 1:nrow(deathwood)){
#   temp <- deathwood$plot[i]
#   deathwood$plot[i] <- newPlotnumber[as.numeric(str_sub(newPlotnumber, 6,6)) == temp] 
# }
#----------------------------------

#-------------------------------------------------------------------------------
# 2 COMBINE THE TREE TABLE FROM THE CURRENT YEAR WITH THE LAST YEARS
#-------------------------------------------------------------------------------
#use the script CONVERT_SQL2TABLE.R
#store the result in Biogeography_Dataprocessing/org/VersXX_trees_previousPlots.csv

#combine the two tables
trees_currentPlots <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_trees_currentPlots.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
trees_currentPlots <- trees_currentPlots[1:93,] 
trees_previousPlots <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_trees_previousPlots.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE)

trees_currentPlots$treeID <- NA

for(i in 1:nrow(trees_currentPlots)){
  #assign new plot ID
  trees_currentPlots$plot[i] <- paste0("fs-05", trees_currentPlots$plot[i])
  
  #assign new tree ID
  if(str_length(trees_currentPlots$numberPlate[i]) == 4){
    trees_currentPlots$treeID[i] <- paste0(trees_currentPlots$colorPlate[i], "0", trees_currentPlots$numberPlate[i])
  }else if(str_length(trees_currentPlots$numberPlate[i]) == 3){
    trees_currentPlots$treeID[i] <- paste0(trees_currentPlots$colorPlate[i], "00", trees_currentPlots$numberPlate[i])
  }else{
    trees_currentPlots$treeID[i] <- paste0(trees_currentPlots$colorPlate[i], "000", trees_currentPlots$numberPlate[i])
  }
  
  #assign the tree species
  if(trees_currentPlots$species[i] == "Hainbuche"){
    trees_currentPlots$species[i] <- "HBU"
  }else if(trees_currentPlots$species[i] == "Stieleiche"){
    trees_currentPlots$species[i] <- "EIS"
  }else if(trees_currentPlots$species[i] == "Traubeneiche"){
    trees_currentPlots$species[i] <- "EIT"
  }else if(trees_currentPlots$species[i] == "Rotbuche"){
    trees_currentPlots$species[i] <- "BUR"
  }else if(trees_currentPlots$species[i] == "Schwarzerle"){
    trees_currentPlots$species[i] <- "ERS"
  }else if(trees_currentPlots$species[i] == "Laerche"){
    trees_currentPlots$species[i] <- "LAE"
  }else if(trees_currentPlots$species[i] == "Douglasie"){
    trees_currentPlots$species[i] <- "DGL"
  }
}
trees_previousPlots <- trees_previousPlots[trees_previousPlots$deathwood == 0,]

trees_previousPlots$easting <- NULL
trees_previousPlots$northing <- NULL
trees_previousPlots$remarks <- NULL
trees_previousPlots$deathwood <- NULL


trees_currentPlots$creatorID <- "FMLRS"
trees_currentPlots$ID <- seq(from = 1077, to = as.numeric(1076+nrow(trees_currentPlots)), by = 1)
trees_currentPlots <- cbind(trees_currentPlots[2], trees_currentPlots[15], trees_currentPlots[16], trees_currentPlots[7], trees_currentPlots[13], trees_currentPlots[14], trees_currentPlots[1])
names(trees_currentPlots) <- names(trees_previousPlots)

#combine the tables
trees <- rbind(trees_previousPlots, trees_currentPlots)

rm(trees_currentPlots, trees_previousPlots)
#write.csv(trees, paste0(workingDir, paste0("processed/treesAll_vers", currentVersion, ".csv")), row.names = FALSE)

#-------------------------------------------------------------------------------
# 3 COMBINE THE DEATHWOOD TABLE FROM THE CURRENT YEAR WITH THE LAST YEARS
#-------------------------------------------------------------------------------
#use the script CONVERT_SQL2TABLE.R
#store the result in Biogeography_Dataprocessing/org/VersXX_trees_previousPlots.csv

#combine the two tables
deathwood_current <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_deathwood.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
deathwood_previous <- read.csv(paste0(workingDir, "org/Vers", currentVersion, "_trees_previousPlots.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE)
deathwood_previous <- deathwood_previous[deathwood_previous$deathwood == 1,]

deathwood_current$treeID <- NA

for(i in 1:nrow(deathwood_current)){
  #assign new plot ID
  deathwood_current$plot[i] <- paste0("fs-05", deathwood_current$plot[i])
}

deathwood_previous <- cbind(deathwood_previous[10], deathwood_previous[7]) 
deathwood_previous$class <- 1
deathwood_previous <- cbind(deathwood_previous[1], deathwood_previous[3], deathwood_previous[2])
deathwood_current <- cbind(deathwood_current[1], deathwood_current[2], deathwood_current[3])

names(deathwood_previous) <- names(deathwood_current)
#combine the tables
deathwood <- rbind(deathwood_previous, deathwood_current)

rm(deathwood_previous, deathwood_current)
#write.csv(deathwood, paste0(workingDir, paste0("processed/deathwoodAll_vers", currentVersion, ".csv")), row.names = FALSE)

#-------------------------------------------------------------------------------
# 4 ASSIGN THE HIGHT LEVEL
#-------------------------------------------------------------------------------
# 4.1 Assign the height level to each living tree (5m levels)
#------------------------------------------------------------
levels <- seq(from = 5, to = 45, by = 5)
trees <- read.csv(paste0(workingDir, paste0("processed/treesAll_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)


#derivation
# trees$totalHeight[1] > levels
# levels[trees$totalHeight[1] > levels]
# length(levels[trees$totalHeight[1] > levels])
# length(levels[trees$totalHeight[1] > levels]) + 1


for(i in 1:nrow(trees)){
  if(!is.na(trees$height[i])){
    trees$level[i] <- length(levels[trees$height[i] > levels]) + 1
  }else{
    trees$level[i] <- NA
  }
}

#write.csv(trees, paste0(workingDir, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
trees <- read.csv(paste0(workingDir, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)

# 4.2 Assign the height level to each dead tree (5m levels)
#-----------------------------------------------------------
#only the category 1 (standing tree) and 3 (lying tree) should be assigned to a height level
#-> other categories = NA
#standing trees could have a level > 1 -> biased by transversal lying trees which are defined as level 1

#smaller deathwood parts are not included in the form

deathwood <- read.csv(paste0(workingDir, "processed/deathwoodAll_vers", currentVersion, ".csv"), sep = ",", dec = ",", stringsAsFactors = FALSE)

for(i in 1:nrow(deathwood)){
  if(deathwood$class[i] == 2 | deathwood$class[i] > 3){
    deathwood$level[i] <- NA
  }else if(deathwood$class[i] == 3 & !is.na(deathwood$length[i])){
    deathwood$level[i] <- 1
  }else if(deathwood$class[i] == 1 & !is.na(deathwood$length[i])){
    deathwood$level[i] <- length(levels[deathwood$length[i] > levels]) + 1
  }else{
    deathwood$level[i] <- NA
  }
}

#write.csv(deathwood, paste0(workingDir, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
deathwood <- read.csv(paste0(workingDir, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)

#-------------------------------------------------------------------------------
# 5 STATISTICAL ANALYSIS
#-------------------------------------------------------------------------------
#Preliminary considerations
#--------------------------
#necessary parameters:
# tree species diversity    [v]
# tree level diversity      [v]
# tree condition diversity  [v]
# overall diversity         [v]

# tree species evenness     [v]
# tree level evenness       [v]
# tree condition evenness   [v]
# overall evenness          [v]

#shannon entropy

#as H = - E(from i= 1 to s)p(i)*log2(p(i)
#H ... Entropy
#i ... tree of the species x
#E ... sum from i = 1 to s
#s ... end value -> number of occuring tree species in the plot
#p(i) ... propability of i -> calculated by the percentage of the 
#         tree species in proportion to number of all trees
#
#exmpl. 10 trees total, 7 beech trees, 3 oak trees
#p(i = beech) = 7/10
#p(i = oak) = 3/10


#evenness

#as E = H/Hmax
#H...entropy as calculated above
#hmax... calculated by log(n)
# -> log(n) = equal distribution of the species number to the number of species
#
#exampl. 10 trees total, 7 beech trees, 3 oak trees
#Hmax = 0,5 for p(beech) and 0,5 for p(oak)

# 5.1 Tree species entropy
#---------------------------

#load tree species entropy function
source(paste0(workingDir, "src/treespeciesEntropy_fun.R"))

#run the function and load the output
treespecies_entropy(treeTable = trees, outputFolder = paste0(workingDir, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"))
totalTreespeciesEntropy <- read.csv(paste0(workingDir, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)

# 5.2 Tree level entropy
#-----------------------
#using living and dead trees

source(paste0(workingDir, "src/treelevelEntropy_fun.R"))

treelevel_entropy(treeTable = trees, deathwoodTable = deathwood, outputFolder = paste0(workingDir, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")))
totalTreeLevelEntropy <- read.csv(paste0(workingDir, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)

# 5.3 tree condition entropy
#----------------------------
#only deathwood of the class 1 and 3 is assigned as condition 2 (dead)

source(paste0(workingDir, "src/treeconditionEntropy_fun.R"))


treecondition_entropy(treeTable = trees, deathwoodTable = deathwood, outputFolder = paste0(workingDir, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")))
totalTreeConditionEntropy <- read.csv(paste0(workingDir, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)

# 5.4 Overall entropy
#---------------------
 
source(paste0(workingDir, "src/overallEntropy_fun.R"))
overall_entropy(species = read.csv(paste0(workingDir, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                level = read.csv(paste0(workingDir, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE),
                condition = read.csv(paste0(workingDir, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE),
                outputFolder = paste0(workingDir, paste0("entropy/overallEntropy_vers", currentVersion,".csv")))

overallEntropy <- read.csv(paste0(workingDir, paste0("entropy/overallEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)

# 5.5 tree species evenness
#--------------------------
#load tree species evenness function
source(paste0(workingDir, "src/treespeciesEvenness_fun.R"))

#run the function and load the output
treespecies_evenness(treeTable = trees, 
                     entropy = read.csv(paste0(workingDir, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                     outputFolder = paste0(workingDir, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"))

totalTreespeciesEvenness <- read.csv(paste0(workingDir, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)

# 5.6 tree level evenness
#------------------------

#load tree level evenness function
source(paste0(workingDir, "src/treelevelEvenness_fun.R"))

#run the function and load the output
treelevel_evenness(treeTable = trees,
                   deathwoodTable = deathwood,
                   entropy = read.csv(paste0(workingDir, "entropy/treeLevelEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                   outputFolder = paste0(workingDir, "evenness/treeLevelEvenness_vers", currentVersion, ".csv"))

totalTreelevelEvenness <- read.csv(paste0(workingDir, "evenness/treeLevelEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)

# 5.7 tree condition evenness
#-----------------------------
#load tree condition evenness function
source(paste0(workingDir, "src/treeconditionEvenness_fun.R"))

#run the function and load the output
treecondition_evenness(treeTable = trees,
                       deathwoodTable = deathwood,
                       entropy = read.csv(paste0(workingDir, "entropy/treeConditionEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                       outputFolder = paste0(workingDir, "evenness/treeConditionEvenness_vers", currentVersion, ".csv"))

totalTreeconditionEvenness <- read.csv(paste0(workingDir, "evenness/treeConditionEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)

# 5.8 overall evenness
#----------------------
#load overall evenness function
source(paste0(workingDir, "src/overallEvenness_fun.R"))

#run the function and load the output
overall_evenness(species = read.csv(paste0(workingDir, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                 level = read.csv(paste0(workingDir, "evenness/treeLevelEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                 condition = read.csv(paste0(workingDir, "evenness/treeConditionEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                 outputFolder = paste0(workingDir, "evenness/overallEvenness_vers", currentVersion, ".csv"))

overallEvenness <- read.csv(paste0(workingDir, "evenness/overallEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
