treelevel_evenness <- function(treeTable, deathwoodTable, entropy, outputFolder){
  #' @description Loops over all plot numbers. Calculates for each the tree species evenness using shannon entropy and the maximum entropy. The latter is calculated by expecting the same distribution of all occuring species (all with the same probability).
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot).
  #' @param deathwoodTable A dataframe containing at least the plotnumber (header = plot) and the tree level (header = level)
  #' @param entropy The entropy of the plot as a dataframe with at least one column (header = plotNumber)
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Generates a dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  
  totalTreelevelEvenness <- data.frame(plotNumber = NA, evenness = NA)
  #for every plot
  for(i in 1:length(unique(treeTable$plot))){
    
    #for one plot with the number i
    ######################################
    #load the entropy
    entropyInPlot <- entropy$entropy[entropy$plotNumber == unique(treeTable$plot)[i]]
    #load the tree species of one plot
    treesInPlot <- treeTable[treeTable$plot == unique(treeTable$plot)[i],]
    treesInPlot <- treesInPlot["level"]
    deadTreesInPlot <- deathwoodTable[deathwoodTable$plot == unique(treeTable$plot)[i],]
    deadTreesInPlot <- deadTreesInPlot["level"]
    #bind them together
    allTreesInPLot <- rbind(treesInPlot, deadTreesInPlot)
    #remove NAs
    allTreesInPLot <- allTreesInPLot[!is.na(allTreesInPLot)]
    
    #for maximum entropy:
    #calculate the number of elements
    n <- length(unique(allTreesInPLot))
    #calculate the probability of every species (all have the same probability)
    prob <- 1/n
    #calculate the logarithm for one species
    entropy_val <- prob * log(prob, base = 2)
    #calculate the max entropy by multiplication of n with the entropy values
    max_entropy <- n * entropy_val * -1
    #evenness
    evenness <- entropyInPlot/max_entropy
    ######################################
    
    if(i == 1){
      totalTreelevelEvenness$plotNumber <- unique(treeTable$plot)[i]
      totalTreelevelEvenness$evenness <- evenness
    }else{
      temp_plotNumber <- unique(treeTable$plot)[i]
      temp_evenness <- evenness
      totalTreelevelEvenness[i,] <- c(temp_plotNumber, temp_evenness)
    }
  }
  
  #save the data frame
  write.csv(totalTreelevelEvenness, outputFolder, row.names = FALSE)  
}
