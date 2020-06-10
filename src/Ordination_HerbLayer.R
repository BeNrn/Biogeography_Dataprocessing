#file_base <- "~/Studium/02_Master/07_Biogeographie/R/Biogeography_Dataprocessing/"
#file_base <- "F:/MODULE/07_Biogeographie/R/Biogeo_Dataprocessing/"
file_base <- "D:/Hausabeit_Biogeo/Biogeography_Dataprocessing-master/"

currentVersion <- "11"

library(vegan)
library(ggplot2)
library(stringr)
#-------------------------------------
#load the data
#herbal layer
herbals_eigene <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
herbals_andere <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_herbals_ANDERE.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)

#general
general <- read.csv(paste0(file_base, paste0("org/Vers", currentVersion, "_general.csv")), sep = ";", stringsAsFactors = FALSE)
general <- general[1:8,]
#-------------------------------------

# 1 restructure herbals andere

herbals_andere$plot_alt <- NULL
herbals_andere$Note <- NULL
names(herbals_andere)[2] <- "species"
names(herbals_andere)[3] <- "lat"
names(herbals_andere)[4] <- "coverage"

################################################
#r weniger 5%		0,01
#+ weniger 5%		0,1
#1 weniger 5%		1,0 + 2,0 + 3,0
#2 5-25%			  5,0 + 10,0 + 15,0 + 20,0 
#3 25-50%		    25,0 + 35,0 + 40,0 
#4 50-75%       50,0
#5 75-100%
################################################

#rename the coverage
herbals_andere$coverage_correct <- NA

for(i in 1:nrow(herbals_andere)){
  if(herbals_andere$coverage[i] == 0.01){
    herbals_andere$coverage_correct[i] <- "r"
  }else if(herbals_andere$coverage[i] == 0.1){
    herbals_andere$coverage_correct[i] <- "+"
  }else if(herbals_andere$coverage[i] < 5 &  herbals_andere$coverage[i] > 0.1){
    herbals_andere$coverage_correct[i] <- "1"
  }else if(herbals_andere$coverage[i] < 25 &  herbals_andere$coverage[i] > 3){
    herbals_andere$coverage_correct[i] <- "2"
  }else if(herbals_andere$coverage[i] < 50 &  herbals_andere$coverage[i] > 20){
    herbals_andere$coverage_correct[i] <- "3"
  }else if(herbals_andere$coverage[i] == 50){
    herbals_andere$coverage_correct[i] <- "4"
  }
}

names(herbals_andere)[5] <- names(herbals_andere)[4]
herbals_andere[,4] <- NULL

#-------------------------------------

# 2 restructure herbals_eigene

herbals_eigene$comment <- NULL
herbals_eigene$foto <- NULL
herbals_eigene <- herbals_eigene[herbals_eigene$processing_information != "ignorieren",]
herbals_eigene$processing_information <- NULL
names(herbals_eigene)[1] <- names(herbals_andere)[1]

#change plot ID
for(i in 1:nrow(herbals_eigene)){
  herbals_eigene$plotID[i] <- paste0("fs-05", herbals_eigene$plotID[i])
}

herbals_eigene <- herbals_eigene[!is.na(herbals_eigene$lat),]
#-------------------------------------

# 3 combine the two datasets

herbals <- rbind(herbals_andere, herbals_eigene)
rm(herbals_andere, herbals_eigene)

#write.csv(herbals, paste0(file_base, paste0("processed/herbals_vers", currentVersion, ".csv")), row.names = FALSE)
herbals <- read.csv(paste0(file_base, "processed/herbals_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)
#-------------------------------------

# 4 reshape the dataframe

#assing a metrical scale level
for(i in 1:nrow(herbals)){
  if(herbals$coverage[i] == "r"){
    herbals$coverage[i] <- 0.01
  }else if(herbals$coverage[i] == "+"){
    herbals$coverage[i] <- 0.1
  }else if(herbals$coverage[i] == "1"){
    herbals$coverage[i] <- 1
  }else if(herbals$coverage[i] == "2"){
    herbals$coverage[i] <- 15
  }else if(herbals$coverage[i] == "3"){
    herbals$coverage[i] <- 37.5
  }else if(herbals$coverage[i] == "4"){
    herbals$coverage[i] <- 62.5
  }else{
    herbals$coverage[i] <- 87.5
  }
}
herbals$coverage <- as.numeric(herbals$coverage)

allSpecies <- unique(herbals$lat)
processHerb <- matrix(nrow = length(unique(herbals$plotID)), ncol = length(allSpecies))
colnames(processHerb) <- allSpecies
rownames(processHerb) <- unique(herbals$plotID)

#-------------------------------------

# 5  assign the coverage of the three layers (tree, shrub, herbal)

layerInfo <- cbind(general[2], general[10:12])

rm(general)

#creating NAs for the missing data
gap <- data.frame(x = rep(NA, nrow(processHerb)-(nrow(layerInfo))))

gap <- cbind(gap, gap, gap, gap)
names(gap) <- names(layerInfo)

layerInfo <- rbind(gap, layerInfo)
rm(gap)

processHerb <- cbind(processHerb, layerInfo[2:4])

rm(layerInfo)
#-------------------------------------

# 6 add the counts

#iterate over the plot locations
for(i in 1:nrow(processHerb)){
  temp_herb <- herbals[herbals$plotID == unique(herbals$plotID)[i],]
  #iterate over the species
  for(j in 1:(ncol(processHerb)-3)){
    count <- temp_herb$lat == colnames(processHerb)[j]
    processHerb[i,j] <- sum(count, na.rm = TRUE)
  }
}
#to df
processHerb <- as.data.frame(processHerb)

#write.csv(processHerb, paste0(file_base, paste0("processed/restructuredHerb_vers", currentVersion, ".csv")), row.names = FALSE)
processHerb <- read.csv(paste0(file_base, "processed/restructuredHerb_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)

#-------------------------------------

# 7 start with the ordination

processHerb <- processHerb[(44-7):44,]


######################################################
#Add the trees
trees <- read.csv(paste0(file_base, "org/Vers", currentVersion, "_trees_currentPlots.csv"), sep = ";", dec = ",", stringsAsFactors = FALSE)
trees <- trees[1:93,]

trees <- cbind(trees[,1],trees[,7])

trees_count <- data.frame(plot = seq(1,8), NA, NA, NA, NA, NA, NA, NA)
names(trees_count)[2:8] <- unique(trees[,2])

for(i in seq(1,8)){
  for(j in seq(1, length(unique(trees[,2])))){
    #iterate over plots
    tmp <- (trees[trees[,1] == i])
    #iterate over species
    trees_count[i,j+1] <- length(tmp[tmp == unique(trees[,2])[j]])
  }
}

#actually add the trees
processHerb <- cbind(processHerb, trees_count[,2:8])

#write.csv(processHerb, paste0(file_base, paste0("processed/restructuredHerb_FINAL_vers", currentVersion, ".csv")), row.names = FALSE)
finalHerb <- read.csv(paste0(file_base, "processed/restructuredHerb_FINAL_vers", currentVersion, ".csv"), sep = ",", stringsAsFactors = FALSE)
######################################################

#remove cols with only zeros
finalHerb <- finalHerb[,74:ncol(finalHerb)]

#indirekte Gradientenanalyse (S.64, Multivariate Statistik in der Ökologie, Wesche, Leyer)
ca_indir <- cca(finalHerb[,1:41])
ca_indir

plot(ca_indir,display = "sites", xlim = c(-2,2))


#plot the species, colored after their location
df <- data.frame(species = names(finalHerb[,1:41]), CA1 = scores(ca_indir)$species[,1], CA2 = scores(ca_indir)$species[,2])
df[4:11] <- rep(NA, 8)
names(df)[4:11] <- c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6", "plot7", "plot8")


for(i in 1: nrow(df)){
  df$plot1[i] <- finalHerb[1,i]
  df$plot2[i] <- finalHerb[2,i]
  df$plot3[i] <- finalHerb[3,i]
  df$plot4[i] <- finalHerb[4,i]
  df$plot5[i] <- finalHerb[5,i]
  df$plot6[i] <- finalHerb[6,i]
  df$plot7[i] <- finalHerb[7,i]
  df$plot8[i] <- finalHerb[8,i]
}

#merge the species and their coordinates from the first to the last plot plotwise together

#header
#species plot 1
#species plot 2
#...

for(i in seq(1:8)){
  if(i == 1){
    plotDF <- df[df[,i+3] != 0,][,1:3]
    plotDF <- cbind(plotDF, rep(i, nrow(plotDF)))
    names(plotDF)[4] <- "plot" 
  }else{
    tempDF <- df[df[,i+3] != 0,][,1:3] 
    tempDF <- cbind(tempDF, rep(i, nrow(tempDF)))
    names(tempDF)[4] <- "plot"
    plotDF <- rbind(plotDF, tempDF)
  }
}
plotDF$plot <- as.character(plotDF$plot)

#plot the result with the plot number as group argument
ggplot(data = plotDF, mapping = aes(CA1, CA2))+
  geom_point(aes(color = plot), size = 3)+
  geom_text(aes(label = species, color = plot), hjust = -0.1, vjust = 0.5, size = 4, fontface = "bold")+
  scale_color_discrete(l = 50, h = c(00, 170))+
  xlab("CA1 Achse")+
  ylab("CA2 Achse")+
  theme(axis.text.x = element_text(color="black", size=12), 
        axis.text.y = element_text(color="black", size=12),
        axis.title.x = element_text(color = "black", size = 13),
        axis.title.y = element_text(color = "black", size = 13))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "#5F6670")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "#5F6670")+
  coord_cartesian(xlim = c(-3, 3), ylim = c(-2, 2))





plot(df$x[df$plot1 != 0], df$y[df$plot1 != 0], col = "red", pch = 19, xlim = c(-4, 4), ylim = c(-1.5, 1.5), cex = 2)
points(df$x[df$plot2 != 0], df$y[df$plot2 != 0], col = "green", pch = 19, cex = 1.3)
points(df$x[df$plot3 != 0], df$y[df$plot3 != 0], col = "yellow", pch = 19)
points(df$x[df$plot4 != 0], df$y[df$plot4 != 0], col = "blue", pch = 1)
points(df$x[df$plot5 != 0], df$y[df$plot5 != 0], col = "black", pch = 3)
points(df$x[df$plot6 != 0], df$y[df$plot6 != 0], col = "purple", pch = 3)
points(df$x[df$plot7 != 0], df$y[df$plot7 != 0], col = "brown", pch = 2)
points(df$x[df$plot8 != 0], df$y[df$plot8 != 0], col = "darkblue", pch = 2, cex = 2)



scores(ca_indir)$species
plot(scores(ca_indir)$species[,1], scores(ca_indir)$species[,2])


#find the datapoints of the plot
scores(ca_indir)$sites

plot(scores(ca_indir)$sites[,1], scores(ca_indir)$sites[,2], xlim = c(-4, 4), ylim = c(-2, 2))

df <- data.frame(scores(ca_indir)$sites)
ggplot(data = df, mapping = aes(CA1, CA2))+
  geom_point(color = "darkgreen", size = 2)+
  geom_text(aes(label = c("Plot 1", "Plot 2", "Plot 3", "Plot 4", "Plot 5", "Plot 6", "Plot 7", "Plot 8")), hjust = -0.2, vjust = 0.5, size = 4)+
  xlab("CA1 Achse")+
  ylab("CA2 Achse")+
  theme(axis.text.x = element_text(color="black", size=12), 
        axis.text.y = element_text(color="black", size=12),
        axis.title.x = element_text(color = "black", size = 13),
        axis.title.y = element_text(color = "black", size = 13))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "#5F6670")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "#5F6670")+
  coord_cartesian(xlim = c(-3, 3), ylim = c(-2, 2))
  


col_wildcard <- finalHerb[,51]

df <- data.frame(x = scores(ca_indir)$sites[,1], y = scores(ca_indir)$sites[,2], col = sample(rainbow(length(unique(col_wildcard))), size = length(col_wildcard), replace = T))
plot(x = df$x, y = df$y, col = df$col, pch = 19)


rgb(red, green, blue, alpha, maxColorValue = 1)

df  <- data.frame(id = 1:N, x1 = rnorm(N), x2 = rnorm(N), x3 = sample(rainbow(5),N,replace=T) )
plot(x=df$x1, y=df$x2, col=df$x3, pch=19)

#first try
ca <- cca(finalHerb)
ca
plot(ca,display="sites")

dca <- decorana(processHerb)
dca
plot(dca,display="sites")

#value distribution
for(i in 1:ncol(processHerb)){print(unique(processHerb[,i]))}
#only the coverage distribution shows more variability 
#without the coverage, the data doesn't contain suitable data



