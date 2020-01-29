overall_entropy <- function(species, level, condition, outputFolder){
  #' @description Loops over all entropy dataframesand calculates for each plot the overall entropy using the specific entropy values of this plot.
  #' 
  #' @param treeTable A dataframe containing at least the plotnumber (header = plot) and the tree species (header = specID)
  #' @param species A dataframe containing the species entropy
  #' @param level A dataframe containing the level entropy.
  #' @param condition A dataframe containing the condition entropy.
  #' @param outputFolder The folder where the dataframe should be saved.
  #' 
  #' @return Dataframe, which is written to the outputFolder.
  #' 
  #' @references Lingenfelder, M. & J. Weber (2001): Analyse der Strukturdiversität in Bannwäldern. - in: AFZ-Der Wald. 13. S. 695 - 697.
  #' 
  overallEntropy <- data.frame(plotNumber = NA, speciesEntropy = NA,
                               levelEntropy = NA, conditionEntropy = NA, overallEntropy = NA)
  #for every plot
  for(i in 1:length(unique(species$plot))){
    spec <- species$entropy[species$plotNumber == unique(species$plotNumber)[i]]
    lev <- level$entropy[level$plotNumber == unique(species$plotNumber)[i]]
    cond <- condition$entropy[condition$plotNumber == unique(species$plotNumber)[i]]
    overallE <- spec+lev+cond
    
    if(i == 1){
      overallEntropy$plotNumber <- unique(species$plotNumber)[i]
      overallEntropy$speciesEntropy <- spec
      overallEntropy$levelEntropy <- lev
      overallEntropy$conditionEntropy <- cond
      overallEntropy$overallEntropy <- overallE
    }else{
      temp_plotNumber <- unique(species$plotNumber)[i]
      temp_spec <- spec
      temp_lev <- lev
      temp_cond <- cond
      temp_entropy <- overallE

      overallEntropy[i,] <- c(temp_plotNumber, temp_spec, temp_lev, temp_cond, temp_entropy)
    }
  }
  
  #save the data frame
  write.csv(overallEntropy, outputFolder, row.names = FALSE)
}