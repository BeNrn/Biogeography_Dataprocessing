file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
currentVersion <- "11"

library(formattable) #https://www.littlemissdata.com/blog/prettytables
library(DT) #https://rstudio.github.io/DT/010-style.html

#---------------------------------------------------------------------------------
overallEntropy <- read.csv(paste0(file_base, paste0("entropy/overallEntropy_vers", currentVersion,".csv")), stringsAsFactors = FALSE)

#rounding
for(i in 2: ncol(overallEntropy)){
  overallEntropy[i] <- round(overallEntropy[i], digits = 2)
}

#heat map
names <- names(overallEntropy)

formattable(overallEntropy[60:67,],
            align = c("c","c","c","c","c"),
            list("plotNumber" = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 "speciesEntropy" = color_tile("white", "#2a542d"),
                 "levelEntropy" = color_tile("white", "#2a542d"),
                 "conditionEntropy" = color_tile("white", "#2a542d"),
                 "overallEntropy" = color_tile("white", "#2a542d")))

#DT
overallEntropy <- overallEntropy[60:67,]
color_intensity <- seq(max(overallEntropy[2:5]), min(overallEntropy[2:5]), length.out = 10)
max_colorIntensity<- max(color_intensity)
for(i in 1:length(color_intensity)){
  color_intensity[i] <- color_intensity[i]/max_colorIntensity
  print(rgb(0,color_intensity[i],0))
}
rgb(1,)


