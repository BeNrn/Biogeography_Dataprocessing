######################################################
# #example for one plot

#trees from org/VersX_Trees.csv

# condition <- c("livingTrees", "deathwood")
# 
# livingTrees_plot1 <- nrow(trees[trees$plot == 1,])
# 
# deathwood_plot1 <- deathwood[deathwood$plot == 1,]
# deathwood_plot1 <- nrow(deathwood_plot1[deathwood_plot1$class == 1 | deathwood_plot1$class == 3,])
# 
# count <- c(livingTrees_plot1, deathwood_plot1)
# 
# entropy <- data.frame(condition = condition, count = count, probability = NA, entropy = NA)
# 
# for(i in 1:nrow(entropy)){
#   entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#   entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
#   if(is.na(entropy$entropy[i])){
#     entropy$entropy[i] <- 0
#   }
# }
# 
# #the entropy of the tree species for plot 1
# H1_treespecies_plot1 <- -sum(entropy$entropy)
######################################################

treecondition_entropy <- function(treeTable, deathwoodTable, outputFolder){
  #' @description Calculates the tree species entropy using a dataframe column of the species in one plot  
  #' 
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot) and the tree species (header = specID)
  #' @param deathwoodTable A dataframe containing at least the plotnumber (header = plot) and the class (header = class). Only trees of the class 1 and three were used
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  totalTreeConditionEntropy <- data.frame(plotNumber = NA, entropy = NA)
  
  for(i in 1:length(unique(treeTable$plot))){
    
    #for one plot with the number i
    ######################################
    #get the number of living trees in one plot
    livingTrees <- nrow(treeTable[treeTable$plot == unique(treeTable$plot)[i],])
    #get the number of dead trees
    deadTrees <- deathwoodTable[deathwoodTable$plot == unique(treeTable$plot)[i],]
    deadTrees <- nrow(deadTrees[deadTrees$class == 1 | deadTrees$class == 3,])
    #fill the count argument
    count <- c(livingTrees, deadTrees)
    #define the condition
    condition <- c("livingTrees", "deadTrees")
    
    #generating a dataframe containing the results for the two conditions
    entropy <- data.frame(condition = condition, count = count, probability = NA, entropy = NA)
    #calculating the probability and the entropy
    for(j in 1:nrow(entropy)){
      entropy$probability[j] <- entropy$count[j] / sum(entropy$count)
      entropy$entropy[j] <- entropy$probability[j] * log(entropy$probability[j], base = 2)
      #to avoid NaN values entropy is turned to 0
      if(is.na(entropy$entropy[j])){
        entropy$entropy[j] <- 0
      }
    }
    #calculating the overall tree species entropy for the plot
    overallTreecondEntropy <- -sum(entropy$entropy)
    ######################################
    
    if(i == 1){
      totalTreeConditionEntropy$plotNumber <- unique(trees$plot)[i]
      totalTreeConditionEntropy$entropy <- overallTreecondEntropy
    }else{
      temp_plotNumber <- unique(trees$plot)[i]
      temp_entropy <- overallTreecondEntropy
      totalTreeConditionEntropy[i,] <- c(temp_plotNumber, temp_entropy)
    }
  }
  
  #save the data frame
  write.csv(totalTreeConditionEntropy, outputFolder, row.names = FALSE)
}