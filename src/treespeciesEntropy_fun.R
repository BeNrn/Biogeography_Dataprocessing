######################################################
# #example for one plot

#trees from org/VersX_Trees.csv

# trees_plot1 <- trees[trees$plot == 1,]
# species <- unique(trees_plot1$species)
# 
# count <- c()
# 
# for(i in 1:length(species)){
#   count[i] <- nrow(trees_plot1[trees_plot1$species == species[i],])
# }
# 
# entropy <- data.frame(species = species, count = count, probability = NA, entropy = NA)
# 
# for(i in 1:nrow(entropy)){
#   entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#   entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
# }
# 
# #the entropy of the tree species for plot 1
# H1_treespecies_plot1 <- -sum(entropy$entropy)

#function without loop over all plots
# # # # # # # # # # # # # #  # # # #

# treespecies_entropy <- function(plotNumber, treeTable){
#   # @description Calculates the tree species entropy using a dataframe column of the species in one plot  
#   # 
#   # @param plotNumber The number of the plot as numeric argument
#   # @param treeTable A dataframe containing at least the plotnumber (header = plot) and the tree species (header = species)
#   # @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
#   
#   #load the tree species of one plot
#   treesInPlot <- treeTable[treeTable$plot == plotNumber,]
#   #extract the unique species
#   species <- unique(treesInPlot$species)
#   #extract the number of the unique individuals 
#   count <- c()
#   for(i in 1:length(species)){
#     count[i] <- nrow(treesInPlot[treesInPlot$species == species[i],])
#   }
#   #generating a dataframe containing the results for each individual species
#   entropy <- data.frame(species = species, count = count, probability = NA, entropy = NA)
#   #calculating the probability and the entropy
#   for(i in 1:nrow(entropy)){
#     entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#     entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
#   }
#   #calculating the overall tree species entropy for the plot
#   overallTreespEntropy <- -sum(entropy$entropy)
#   return(overallTreespEntropy)
# }

######################################################

treespecies_entropy <- function(treeTable, outputFolder){
  #' @description Loops over all unique plot numbers. Calculates for each the tree species entropy using a dataframe column of the species.  
  #' 
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot) and the tree species (header = specID)
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  
  #anstatt plot == i unique(ites element)
  
  #create an empty data frame
  totalTreespeciesEntropy <- data.frame(plotNumber = NA, entropy = NA)
  #for every plot
  for(i in 1:length(unique(treeTable$plot))){
    
    #for one plot with the number i
    ######################################
    #load the tree species of one plot
    treesInPlot <- treeTable[treeTable$plot == unique(treeTable$plot)[i],]
    #extract the unique species
    species <- unique(treesInPlot$specID)
    #extract the number of the unique individuals 
    count <- c()
    for(j in 1:length(species)){
      count[j] <- nrow(treesInPlot[treesInPlot$specID == species[j],])
    }
    #generating a dataframe containing the results for each individual species
    entropy <- data.frame(species = species, count = count, probability = NA, entropy = NA)
    #calculating the probability and the entropy
    for(k in 1:nrow(entropy)){
      entropy$probability[k] <- entropy$count[k] / sum(entropy$count)
      entropy$entropy[k] <- entropy$probability[k] * log(entropy$probability[k], base = 2)
    }
    #calculating the overall tree species entropy for the plot
    overallTreespEntropy <- -sum(entropy$entropy)
    ######################################
    
    if(i == 1){
      totalTreespeciesEntropy$plotNumber <- unique(treeTable$plot)[i]
      totalTreespeciesEntropy$entropy <- overallTreespEntropy
    }else{
      temp_plotNumber <- unique(treeTable$plot)[i]
      temp_entropy <- overallTreespEntropy
      totalTreespeciesEntropy[i,] <- c(temp_plotNumber, temp_entropy)
    }
  }
  
  #save the data frame
  write.csv(totalTreespeciesEntropy, outputFolder, row.names = FALSE)
}