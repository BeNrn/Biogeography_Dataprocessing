file_base <- "~/Studium/02_Master/07_Biogeographie/"
currentVersion <- "11"

library(RSQLite)
#----------------------------------------------------------
#using the SQLite Database
sqlite    <- dbDriver("SQLite")
db <- dbConnect(sqlite, paste0(file_base, "Daten/Datenbank/MOFGeoDB.sqlite"))

dbListTables(db)

#load the trees
trees <- dbReadTable(db, "tree")

#remove unneccessary cols
trees$statusID <- NULL
trees$geometry <- NULL
trees$plot <- NA


#loading the treePlot table (contain the connection tree to plot number)
treePlot <- dbReadTable(db, "treePlot")

#assign the plot ID to the trees
for(i in 1:nrow(trees)){
  if(length(treePlot$plotID[treePlot$treeID == trees$treeID[i]]) != 0){
    trees$plot[i] <- treePlot$plotID[treePlot$treeID == trees$treeID[i]]
  }else{
    trees$plot[i] <- NA
  }
}
# -> 509 NAs
#remove missing values
trees <- trees[!is.na(trees$plot),]

names(trees)[1] <- "ID"

#write the df
write.csv(trees, paste0(file_base, paste0("R/Biogeography_Dataprocessing/org/Vers", currentVersion, "_trees_previousPlots.csv")), row.names = FALSE)

dbDisconnect(sqlite, paste0(file_base, "Daten/Datenbank/MOFGeoDB.sqlite"))
