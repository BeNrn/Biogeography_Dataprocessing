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

#estimate deathwood
trees$deathwood <- 0
for(i in 1:nrow(trees)){
  if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot nur Stamm"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. liegt schief"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. Specht mittel. "){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. rinde. "){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. rinde. gegen�ber c7."){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. rind3. ."){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. Rinde . ."){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. rinde. nahe erster Qb."){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "oben offen. keine Krone. tot"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "viele Baumpilze. jedoch unter 25%. Totholz"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. viel Rinde ab und lose. stammartiger Ast steckt abgebrochen neben Stamm im Boden. Rindensch�rfung auf jeder H�he"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "wurde Februar 2019 gef�llt und bleibt liegen"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. abgebrochen auf 7m. Stamm 20m daneben. Krone fehlt"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "tot. abgeknickt"){
    trees$deathwood[i] <- 1
  }else if(!is.na(trees$remarks[i]) & trees$remarks[i] == "Totholz"){
    trees$deathwood[i] <- 1
  }
}



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
