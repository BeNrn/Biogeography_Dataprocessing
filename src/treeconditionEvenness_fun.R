treecondition_evenness <- function(treeTable, deathwoodTable, entropy, outputFolder){
  #' @description Loops over all plot numbers. Calculates for each the tree species evenness using shannon entropy and the maximum entropy. The latter is calculated by expecting the same distribution of all occuring species (all with the same probability).
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot).
  #' @param deathwoodTable A dataframe containing at least the plotnumber (header = plot) and the class (header = class). Only trees of the class 1 and three were used.
  #' @param entropy The entropy of the plot as a dataframe with at least one column (header = plotNumber)
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Generates a dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  
  totalTreeconditionEvenness <- data.frame(plotNumber = NA, evenness = NA)
  #for every plot
  for(i in 1:length(unique(treeTable$plot))){
    
    #for one plot with the number i
    ######################################
    #load the entropy
    entropyInPlot <- entropy$entropy[entropy$plotNumber == unique(treeTable$plot)[i]]
    #load the tree species of one plot
    treesInPlot <- treeTable[treeTable$plot == unique(treeTable$plot)[i],]
    deadTreesInPlot <- deathwoodTable[deathwoodTable$plot == unique(treeTable$plot)[i],]
    #for maximum entropy:
    #calculate the number of conditions
    if(nrow(treesInPlot)!= 0 & nrow(deadTreesInPlot) != 0){
      n <- 2
    }else{
      n <- 1 #n = 0 per definition ausgeschlossen
    }
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
      totalTreeconditionEvenness$plotNumber <- unique(treeTable$plot)[i]
      totalTreeconditionEvenness$evenness <- evenness
    }else{
      temp_plotNumber <- unique(treeTable$plot)[i]
      temp_evenness <- evenness
      totalTreeconditionEvenness[i,] <- c(temp_plotNumber, temp_evenness)
    }
  }
  
  #save the data frame
  write.csv(totalTreeconditionEvenness, outputFolder, row.names = FALSE)  
}
