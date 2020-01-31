file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "11"

library(formattable) #https://cran.r-project.org/web/packages/formattable/vignettes/formattable-data-frame.html
#---------------------------------------------------------------------------------
# 1 overall entropy

overallEntropy <- read.csv(paste0(file_base, paste0("entropy/overallEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)

#rounding
for(i in 2: ncol(overallEntropy)){
  overallEntropy[i] <- round(overallEntropy[i], digits = 2)
}

#heat map
formattable(overallEntropy[60:67,],
            align = c("c","c","c","c","c"),
            list("plotNumber" = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 area(col = speciesEntropy:overallEntropy) ~ color_tile("white", "#188918")))
#############################################################
# 2 overall evenness

overallEvenness <- read.csv(paste0(file_base, "evenness/overallEvenness_vers", currentVersion, ".csv"), stringsAsFactors = FALSE)
overallEvenness[is.na(overallEvenness)] <- NA

#rounding
for(i in 2: ncol(overallEvenness)){
  overallEvenness[i] <- round(overallEvenness[i], digits = 2)
}

#heat map
formattable(overallEvenness[60:67,],
            align = c("c","c","c","c","c"),
            list("plotNumber" = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 area(col = speciesEvenness:overallEvenness) ~ color_tile("white", "#188918")))
