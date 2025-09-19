########################################################
# Clean database from TSP - fifth campaign - data.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2024
########################################################

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to QcTSP5!")
  packageStartupMessage("The QcTSP5 package provides a clean version of the TSP of the third campaign of Quebec provincial inventory.")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
}

.onDetach <- function(libpath) {
}


.loadPackageData <- function(filename) {
  return(readRDS(system.file(paste0("extdata/",filename,".Rds"), package = "QcTSP5")))
}

#'
#' Restore Quebec TSP Data in the Global Environment.
#'
#' @description This function call creates four data.frame objects that contain
#' the tree measurements.
#'
#' @details
#'
#' The resulting list encompasses five data.frame objects are: \cr
#' \itemize{
#' \item plots: the list of plots \cr
#' \item sites: some site information recorded in those plots \cr
#' \item photoInterpretedStands: some site information recorded through photo-interpretation \cr
#' \item trees: the tallied trees \cr
#' \item studyTrees: the study trees (a subsample of trees) \cr
#' }
#'
#' @export
restoreQcTSP5Data <- function() {
  assign("QcTSP5Data", .loadPackageData("QcTSP5"), envir = .GlobalEnv)
}


#'
#' Extract plot list for Artemis simulation
#' @param QcTSP5Data the database that is retrieved through the restoreQcTSP5Data function
#' @param plots a vector of integers standing for the plot id to be considered
#' @version a character string identifying the version of the model to be used either "Artemis2009" or "Artemis2014"
#' @return a data.frame object formatted for Capsis Web API
#'
#' @export
extractArtemisFormatForMetaModelling <- function(QcTSP5Data, plots,version="Artemis2009") {
  plotList <- unique(plots) ### make sure there is no duplicate
  plotInfo <- QcTSP5Data$plots[which(QcTSP5Data$plots$ID_PE %in% plotList), c("ID_PE", "LATITUDE", "LONGITUDE", "DATE_SOND")]
  siteInfo <- QcTSP5Data$sites[which(QcTSP5Data$sites$ID_PE %in% plotList), c("ID_PE", "ALTITUDE", "SDOMAINE", "GUIDE_ECO", "TYPE_ECO", "CL_DRAI")]
  standInfo <- QcTSP5Data$photoInterpretedStands[which(QcTSP5Data$photoInterpretedStands$ID_PE %in% plotList), c("ID_PE", "CL_AGE", "TYPE_ECO")]
  colnames(standInfo)[2] <- "CL_AGE_PHOTO"
  colnames(standInfo)[3] <- "TYPE_ECO_PHOTO"
  treeInfo <- QcTSP5Data$trees[which(QcTSP5Data$trees$ID_PE %in% plotList), c("ID_PE", "ETAT","ESSENCE", "CL_DHP", "TIGE_HA","HAUT_ARBRE")]
  saplings <- QcTSP5Data$saplings
  saplings$HAUT_ARBRE <- NA
  saplingInfo <- saplings[which(saplings$ID_PE %in% plotList), c("ID_PE", "ESSENCE", "CL_DHP", "HAUT_ARBRE","TIGE_HA")]
  plotInfo <- merge(plotInfo, standInfo, by = "ID_PE",all.x=TRUE)
  plotInfo <- merge(plotInfo, siteInfo, by="ID_PE")
  output_tree <- merge(plotInfo,
                  treeInfo,
                  by = "ID_PE")
  output_saplings <- merge(plotInfo,
                          saplingInfo,
                          by = "ID_PE")
  output_saplings$ETAT<-10                #######Rajoute un etat 10 pour les gaules, elles sont toutes vivantes
  output <- rbind(output_tree, output_saplings)
  outputPlots <- unique(output$ID_PE)

  missingPlots <- setdiff(plotList, outputPlots)
  if (length(missingPlots) > 0) {
    message("These plots have no saplings and no trees: ", paste(missingPlots, collapse = ", "))
    message("We will add a fake sapling to make sure they are properly imported in Artemis-2009.")
    fakeSaplings <- NULL
    for (mPlot in missingPlots) {
      fakeSaplings <- rbind(fakeSaplings, data.frame(ID_PE = mPlot, ETAT=10, ESSENCE = "SAB", CL_DHP = as.integer(2), HAUT_ARBRE = NA, TIGE_HA = as.integer(25)))
    }
    output_MissingSaplings <- merge(plotInfo,
                                   fakeSaplings,
                                   by = "ID_PE")
    output <- rbind(output, output_MissingSaplings)
  }

  outputPlots <- unique(output$ID_PE)
  missingPlots <- setdiff(plotList, outputPlots)
  if (length(missingPlots) > 0) {
    message("Apparently, there are still some plots with no saplings and no trees: ", paste(missingPlots, collapse = ", "))
  }

  output <- output[order(output$ID_PE, -output$CL_DHP),]
  output$ANNEE_SOND <- as.integer(format(output$DATE_SOND, "%Y"))
  output$TREEFREQ <- output$TIGE_HA / 25
  #output$TREESTATUS <- 10
  output$TREEHEIGHT <- output$HAUT_ARBRE * .1

   output$TYPE_ECO_PHOTO<-ifelse(is.na(output$TYPE_ECO_PHOTO)==TRUE,output$TYPE_ECO,output$TYPE_ECO_PHOTO)
   output$CL_AGE_PHOTO<-ifelse(is.na(output$CL_AGE_PHOTO)==TRUE,output$CL_AGE,output$CL_AGE_PHOTO)
   output <- output[,c("ID_PE", "LATITUDE", "LONGITUDE", "ALTITUDE", "SDOMAINE", "GUIDE_ECO", "TYPE_ECO", "CL_DRAI", "ETAT",
                      "ESSENCE", "CL_DHP", "TREEFREQ", "TREEHEIGHT", "ANNEE_SOND", "TYPE_ECO_PHOTO", "CL_AGE_PHOTO")]

   if (version=="Artemis2009"){
   colnames(output) <- c("PLOT", "LATITUDE", "LONGITUDE", "ALTITUDE", "SUBDOMAIN", "ECOREGION", "TYPEECO", "DRAINAGE_CLASS",
                         "TREESTATUS", "SPECIES",  "TREEDHPCM", "TREEFREQ", "TREEHEIGHT", "ANNEE_SOND", "STANDTYPEECO", "STANDAGE")
   }

   if (version=="Artemis2014"){
     colnames(output) <- c("PlacetteID", "Latitude", "Longitude", "Altitude", "Sdom_Bio", "Reg_Eco", "Type_Eco", "Cl_Drai","Etat",
                           "Espece", "DHPcm", "Nombre", "Hauteur", "Annee", "Type_Eco_Photo", "Age")
     output$Veg_Pot<-substr(output$Type_Eco,1,3)

   }

  return(output)
}


######################Natura
#Besoins Strate et SousDomaine bioclimatique sous la forme "6OUEST" dans fichiers Arbre
#BesoinsStrate Placette Essence DHP Hauteur Age et Étage dans fichier Arbres Etude

#'
#' Extract plot list for Natura simulation
#' @param QcTSP5Data the database that is retrieved through the restoreQcTSP3Data function
#' @param stratumPlots a dataframe with a column "stratum" as a grouping variable as a character field and a column "plots" for plots number as an integer
#' @param version a character string identifying the version of the model to be used either "Natura2014" or "RNatura2014"
#' @return a list of two dataframes where the first is a tree list and the second is a list of study trees for model Natura on Capsis Web API
#'
#' @export

extractNaturaFormatForMetaModelling <- function(QcTSP5Data, stratumPlots, version="Natura2014") {
  plotList <- unique(stratumPlots$plots) ### make sure there is no duplicate
  plotInfo <- QcTSP5Data$plots[which(QcTSP5Data$plots$ID_PE %in% plotList), c("ID_PE", "LATITUDE", "LONGITUDE", "DATE_SOND")]
  siteInfo <- QcTSP5Data$sites[which(QcTSP5Data$sites$ID_PE %in% plotList), c("ID_PE", "ALTITUDE", "SDOMAINE", "GUIDE_ECO", "TYPE_ECO", "CL_DRAI")]
  standInfo <- QcTSP5Data$photoInterpretedStands[which(QcTSP5Data$photoInterpretedStands$ID_PE %in% plotList), c("ID_PE", "CL_AGE", "TYPE_ECO")]
  colnames(standInfo)[2] <- "CL_AGE_PHOTO"
  colnames(standInfo)[3] <- "TYPE_ECO_PHOTO"
  treeInfo <- QcTSP5Data$trees[which(QcTSP5Data$trees$ID_PE %in% plotList), c("ID_PE", "ETAT","ESSENCE", "CL_DHP", "TIGE_HA","HAUT_ARBRE")]
  plotInfo <- merge(plotInfo, standInfo, by = "ID_PE", all.x=TRUE)
  plotInfo <- merge(plotInfo, siteInfo, by="ID_PE")
  names(stratumPlots)<-c("stratum","ID_PE")
  output <- merge(stratumPlots,
                          plotInfo,
                              by = "ID_PE")
  output<-merge(output,
                  treeInfo,
                      by= "ID_PE")

  studyTreesInfo<-QcTSP5Data$studyTrees[which(QcTSP5Data$studyTrees$ID_PE %in% plotList), c("ID_PE", "ESSENCE", "ETAGE_ARB", "DHP","HAUT_ARBRE","AGE" )]
  studyTreesInfo$DHP<-studyTreesInfo$DHP/10
  studyTreesInfo$HAUT_ARBRE<-studyTreesInfo$HAUT_ARBRE/10
  studyTreesInfo<-studyTreesInfo[which(studyTreesInfo$ETAGE_ARB %in% c("C","D") & is.na(studyTreesInfo$AGE)==FALSE),]#######Sélection des dominants et codominants seulement
  studyTrees<-merge(stratumPlots,
                          studyTreesInfo,
                                by = "ID_PE")

  studyTreesID<-unique(studyTrees$ID_PE)
  outputPlots <- unique(output$ID_PE[which(output$ID_PE %in% studyTreesID)])###Au moins un arbre étude dominant ou codominant avec âge doit être présent pour simulation avec Natura
  missingPlots <- setdiff(plotList, outputPlots)

  if (length(missingPlots) > 0) {
    message("These plots have no  trees: ", paste(missingPlots, collapse = ", "))
    message("They will not be simulated with Natura ")
      }

  output <- output [,c(2,1,3:17)]
  output <- output[order(output$stratum, output$ID_PE, -output$CL_DHP),]
  output$ANNEE_SOND <- as.integer(format(output$DATE_SOND, "%Y"))
  output$TREEFREQ <- output$TIGE_HA / 25
  #output$TREESTATUS <- 10
  output$TREEHEIGHT <- output$HAUT_ARBRE/10

  output$TYPE_ECO_PHOTO<-ifelse(is.na(output$TYPE_ECO_PHOTO)==TRUE,output$TYPE_ECO,output$TYPE_ECO_PHOTO)
  output$CL_AGE_PHOTO<-ifelse(is.na(output$CL_AGE_PHOTO)==TRUE,output$CL_AGE,output$CL_AGE_PHOTO)

  if (version=="Natura2014"){
  output <- output[,c("stratum","ID_PE", "LATITUDE", "LONGITUDE", "ALTITUDE", "SDOMAINE", "GUIDE_ECO", "TYPE_ECO", "CL_DRAI", "ETAT",
                      "ESSENCE", "CL_DHP", "TREEFREQ", "TREEHEIGHT", "ANNEE_SOND", "TYPE_ECO_PHOTO", "CL_AGE_PHOTO")]


  colnames(output) <- c("STRATUM","PLOT", "LATITUDE", "LONGITUDE", "ALTITUDE", "SUBDOMAIN", "ECOREGION", "TYPEECO", "DRAINAGE_CLASS",
                        "TREESTATUS",  "SPECIES", "TREEDHPCM", "TREEFREQ", "TREEHEIGHT", "ANNEE_SOND", "STANDTYPEECO", "STANDAGE")
  }

  if (version=="RNatura2014"){
    output <- output[,c("stratum","ID_PE", "LATITUDE", "LONGITUDE", "ALTITUDE", "SDOMAINE", "GUIDE_ECO","TIGE_HA","TYPE_ECO","ETAT",
                        "ESSENCE",  "CL_DHP",  "TREEHEIGHT")]

    colnames(output) <- c("strate","placette", "latitude", "longitude", "Altitude", "sdom_bio", "reg_eco", "tige_ha","type_eco",
                          "etat","essence",  "dhpcm", "hauteur_pred")
  }

  studyTrees <- studyTrees [,c(2,1,3:7)]

  if (version=="Natura2014"){
  colnames(studyTrees) <- c("STRATUM","PLOT", "SPECIES", "TREECLASS", "TREEDHPCM",  "TREEHEIGHT", "TREEAGE")
  }

  if (version=="RNatura2014"){
    colnames(studyTrees) <- c("strate","placette", "essence", "etage", "dhpcm",  "hauteur", "age")
  }

  outputNat<-list(output,studyTrees)

  return(outputNat)
}


