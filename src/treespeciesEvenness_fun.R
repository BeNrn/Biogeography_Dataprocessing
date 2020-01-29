######################################################
# #example for one plot
# entropy_plot1 <- read.csv(paste0(file_base, paste0("entropy/treeSpeciesEntropy_vers", currentVersion, ".csv")), stringsAsFactors = FALSE)
# entropy_plot1 <- entropy_plot1$entropy[entropy_plot1$plotNumber == 3]
# trees_plot1 <- trees[trees$plot == 3,]
# n <- length(unique(trees_plot1$species))
# probability = 1/n
# entropy_val <- probability*log(probability, base = 2)
# max_entropy <- n * entropy_val * -1
# 
# evenness <- entropy_plot1/max_entropy
######################################################

treespecies_evenness <- function(treeTable, entropy, outputFolder){
  #' @description Loops over all plot numbers. Calculates for each the tree species evenness using shannon entropy and the maximum entropy. The latter is calculated by expecting the same distribution of all occuring species (all with the same probability).
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot).
  #' @param entropy The entropy of the plot as a dataframe with at least one column (header = plotNumber)
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Generates a dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  
  
  totalTreespeciesEvenness <- data.frame(plotNumber = NA, evenness = NA)
  #for every plot
  for(i in 1:length(unique(treeTable$plot))){
    
    #for one plot with the number i
    ######################################
    #load the entropy
    entropyInPlot <- entropy$entropy[entropy$plotNumber == unique(treeTable$plot)[i]]
    #load the tree species of one plot
    treesInPlot <- treeTable[treeTable$plot == unique(treeTable$plot)[i],]
    #for maximum entropy:
    #calculate the number of elements
    n <- length(unique(treesInPlot$specID))
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
      totalTreespeciesEvenness$plotNumber <- unique(treeTable$plot)[i]
      totalTreespeciesEvenness$evenness <- evenness
    }else{
      temp_plotNumber <- unique(treeTable$plot)[i]
      temp_evenness <- evenness
      totalTreespeciesEvenness[i,] <- c(temp_plotNumber, temp_evenness)
    }
  }

  #save the data frame
  write.csv(totalTreespeciesEvenness, outputFolder, row.names = FALSE)  
}
