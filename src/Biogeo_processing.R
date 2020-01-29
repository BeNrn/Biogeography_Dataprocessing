####Before using this script:##################

#1 the file base has to be changed to the personal directory
#2 the loaded .csv files in the "org" folder are extracted from the current version of the "Strukturplots_Tabelle"
#  every single excel sheet was extracted (just a copy) in a new excel file and was saved as .csv using the respective name
#3 the name of the current version has to be filled in  (with a leading 0 for one-digit numbers -> 05 instead of 5)

###############################################

file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeo_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "00"
library(stringr)
#--------------------------------------------------------------------
# 1 READ THE DATA

#general
general <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_general.csv")), sep = ";", stringsAsFactors = FALSE)

#trees
#load the processed table at 2.1
# previousTrees <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_treesPreviousSemesters.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE)
# trees <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_trees.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
# treePlot <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_treesPlot.csv"), sep = ";", dec = ".", stringsAsFactors = FALSE)
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

#young trees
youngTrees <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_youngTrees.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
youngTrees <- youngTrees[1:14,]

#herbals
herbals <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)

#deathwood
#load the processed table at 2.2
# deathwood <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_deathwood.csv")), sep = ";", dec = ",", stringsAsFactors = FALSE)
# #temporarily adjustment------------
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

#--------------------------------------------------------------------

# 2 ASSIGN THE HIGHT LEVEL

# 2.1 assign the height level to each living tree (5m levels)

levels <- seq(from = 5, to = 45, by = 5)

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
rm(levels)

# write.csv(trees, paste0(file_base, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
trees <- read.csv(paste0(file_base, paste0("processed/treesWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)
#--------------------------------------------------------------------

# 2.2 assign the height level to each dead tree (5m levels)

#only the category 1 (standing tree) and 3 (lying tree) should be assigned to a height level
#-> other categories = NA
#standing trees could have a level > 1 -> biased by transversal lying trees which are defined as level 1

#smaller deathwood parts are not included in the form

levels <- seq(from = 5, to = 45, by = 5)

for(i in 1:nrow(deathwood)){
  if(deathwood$class[i] == 2 | deathwood$class[i] > 3){
    deathwood$level[i] <- NA
  }else if(deathwood$class[i] == 3){
    deathwood$level[i] <- 1
  }else{
    deathwood$level[i] <- length(levels[deathwood$length[i] > levels]) + 1
  }
}

# write.csv(deathwood, paste0(file_base, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), row.names = FALSE)
deathwood <- read.csv(paste0(file_base, paste0("processed/deathwoodWithLevels_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)

#--------------------------------------------------------------------

# 3 STATISTICAL ANALYSIS

#needed parameters:
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
#--------------------------------------------------------------------

# 3.1 tree species entropy

#load tree species entropy function
source(paste0(file_base, "src/treespeciesEntropy_fun.R"))

#run the function and load the output
treespecies_entropy(treeTable = trees, outputFolder = paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"))
totalTreespeciesEntropy <- read.csv(paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.2 tree level entropy
#using living and dead trees

source(paste0(file_base, "src/treelevelEntropy_fun.R"))

treelevel_entropy(treeTable = trees, deathwoodTable = deathwood, outputFolder = paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")))
totalTreeLevelEntropy <- read.csv(paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.3 tree condition entropy
#only deathwood of the class 1 and 3 is assigned as condition 2 (dead)

source(paste0(file_base, "src/treeconditionEntropy_fun.R"))


treecondition_entropy(treeTable = trees, deathwoodTable = deathwood, outputFolder = paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")))
totalTreeConditionEntropy <- read.csv(paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.4 overall entropy
 
source(paste0(file_base, "src/overallEntropy_fun.R"))
overall_entropy(species = read.csv(paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                level = read.csv(paste0(file_base, paste0("entropy/treeLevelEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE),
                condition = read.csv(paste0(file_base, paste0("entropy/treeConditionEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE),
                outputFolder = paste0(file_base, paste0("entropy/overallEntropy_vers", currentVersion,".csv")))

overallEntropy <- read.csv(paste0(file_base, paste0("entropy/overallEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.5 tree species evenness

#load tree species evenness function
source(paste0(file_base, "src/treespeciesEvenness_fun.R"))

#run the function and load the output
treespecies_evenness(treeTable = trees, 
                     entropy = read.csv(paste0(file_base, "entropy/treeSpeciesEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                     outputFolder = paste0(file_base, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"))

totalTreespeciesEvenness <- read.csv(paste0(file_base, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.6 tree level evenness

#load tree level evenness function
source(paste0(file_base, "src/treelevelEvenness_fun.R"))

#run the function and load the output
treelevel_evenness(treeTable = trees,
                   deathwoodTable = deathwood,
                   entropy = read.csv(paste0(file_base, "entropy/treeLevelEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                   outputFolder = paste0(file_base, "evenness/treeLevelEvenness_vers", currentVersion, ".csv"))

totalTreelevelEvenness <- read.csv(paste0(file_base, "evenness/treeLevelEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.7 tree condition evenness

#load tree condition evenness function
source(paste0(file_base, "src/treeconditionEvenness_fun.R"))

#run the function and load the output
treecondition_evenness(treeTable = trees,
                       deathwoodTable = deathwood,
                       entropy = read.csv(paste0(file_base, "entropy/treeConditionEntropy_vers", currentVersion, ".csv"), stringsAsFactors = FALSE), 
                       outputFolder = paste0(file_base, "evenness/treeConditionEvenness_vers", currentVersion, ".csv"))

totalTreeconditionEvenness <- read.csv(paste0(file_base, "evenness/treeConditionEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
#------------------------------------------------------------------------------

# 3.8 overall evenness

#load overall evenness function
source(paste0(file_base, "src/overallEvenness_fun.R"))

#run the function and load the output
overall_evenness(species = read.csv(paste0(file_base, "evenness/treeSpeciesEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                 level = read.csv(paste0(file_base, "evenness/treeLevelEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                 condition = read.csv(paste0(file_base, "evenness/treeConditionEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE),
                 outputFolder = paste0(file_base, "evenness/overallEvenness_vers", currentVersion, ".csv"))

overallEvenness <- read.csv(paste0(file_base, "evenness/overallEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
