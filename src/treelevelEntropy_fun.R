######################################################
# #example for one plot

#trees from org/VersX_Trees.csv
# 
# trees_plot1 <- trees[trees$plot == 1,]
# level <- unique(trees_plot1$level)
# 
# count <- c()
# 
# for(i in 1:length(level)){
#   count[i] <- nrow(trees_plot1[trees_plot1$level == level[i],])
# }
# 
# entropy <- data.frame(level = level, count = count, probability = NA, entropy = NA)
# 
# for(i in 1:nrow(entropy)){
#   entropy$probability[i] <- entropy$count[i] / sum(entropy$count)
#   entropy$entropy[i] <- entropy$probability[i] * log(entropy$probability[i], base = 2)
# }
# 
# #the entropy of the tree level for plot 1
# entropy_plot1 <- -sum(entropy$entropy)
######################################################

treelevel_entropy <- function(treeTable, deathwoodTable, outputFolder){
  #' @description Calculates the tree level entropy using a dataframe column of the level characteristics in one plot  
  #' 
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot) and the tree level (header = level).
  #' @param deathwoodTable A dataframe containing at least the plotnumber (header = plot) and the tree level (header = level)
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  totalTreeLevelEntropy <- data.frame(plotNumber = NA, entropy = NA)
  
  for(i in 1:length(unique(treeTable$plot))){
  
    #for one plot with the number i
    ######################################
    #load the tree levels of one plot
    treesInPlot <- treeTable[treeTable$plot == unique(treeTable$plot)[i],]
    treesInPlot <- treesInPlot["level"]
    deadTreesInPlot <- deathwoodTable[deathwoodTable$plot == unique(treeTable$plot)[i],]
    deadTreesInPlot <- deadTreesInPlot["level"]
    allTreesInPLot <- rbind(treesInPlot, deadTreesInPlot)
    #remove NA values
    allTreesInPLot <- allTreesInPLot[!is.na(allTreesInPLot)]
    #the if statement is fomulated because of plots only containing NA values
    #in this special case, the overall entropy is defined as NA
    if(length(allTreesInPLot) == 0){
      overallTreelvEntropy <- NA
      
    }else{
      #extract the unique levels
      level <- unique(allTreesInPLot)
      #extract the number of the unique levels 
      count <- c()
      for(k in 1:length(level)){
        count[k] <- length(allTreesInPLot[allTreesInPLot == level[k]])
      }
      #generating a dataframe containing the results for each individual level
      entropy <- data.frame(level = level, count = count, probability = NA, entropy = NA)
      #calculating the probability and the entropy
      for(j in 1:nrow(entropy)){
        entropy$probability[j] <- entropy$count[j] / sum(entropy$count)
        entropy$entropy[j] <- entropy$probability[j] * log(entropy$probability[j], base = 2)
      }
      #calculating the overall tree level entropy for the plot
      overallTreelvEntropy <- -sum(entropy$entropy)
    }
    ######################################
    
    if(i == 1){
      totalTreeLevelEntropy$plotNumber <- unique(treeTable$plot)[i]
      totalTreeLevelEntropy$entropy <- overallTreelvEntropy
    }else{
      temp_plotNumber <- unique(trees$plot)[i]
      temp_entropy <- overallTreelvEntropy
      totalTreeLevelEntropy[i,] <- c(temp_plotNumber, temp_entropy)
    }
  }
  #save the data frame
  write.csv(totalTreeLevelEntropy, outputFolder, row.names = FALSE)
}