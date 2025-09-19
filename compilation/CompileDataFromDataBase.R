#'
#' Script to produce the data.frame objects
#' that contain the information from
#' the original database.
#' @author Hugues Power- September 2025
#' @importFrom sf st_read st_drop_geometry

rm(list=ls())
options(scipen=999)

source("./compilation/utilityFunctions.R")

if (!require("sf")) {
  install.packages("sf")
  require("sf")
}


.db <- file.path(getwd(), "compilation", "PET5.gpkg") # database path

plots <-st_drop_geometry(st_read(.db, "placette")) #### 40 246 observations
colnames(plots)<-c(toupper(colnames(plots)))

sites <- st_drop_geometry(st_read(.db, "station_pe")) #### 40 246 observations
colnames(sites)<-c(toupper(colnames(sites)))

photoInterpretedStands <- st_drop_geometry(st_read(.db, "pee_ori_sond"))#### 40246 observations
colnames(photoInterpretedStands)<-c(toupper(colnames(photoInterpretedStands )))

trees <- st_drop_geometry(st_read(.db, "dendro_arbres")) #### 1 876 579 observations
colnames(trees)<-c(toupper(colnames(trees)))

studyTrees <- st_drop_geometry(st_read(.db, "dendro_arbres_etudes")) #### 183 499 observations
colnames(studyTrees)<-c(toupper(colnames(studyTrees)))

saplings <- st_drop_geometry(st_read(.db, "dendro_gaules")) #### 166 718 observations
colnames(saplings)<-c(toupper(colnames(saplings)))

sites$CL_DRAI<-as.integer(sites$CL_DRAI)
trees$ETAT<-as.integer(trees$ETAT)
trees$CL_DHP<-as.integer(trees$CL_DHP)
saplings$CL_DHP<-as.integer(saplings$CL_DHP)


sort(unique(sites$GUIDE_ECO))
GUIDE_ECO <- c("1a", "2a", "2b", "2c", "3ab",  "3c", "3d",
               "4a", "4bc", "4de", "4f", "4gh",
               "5a", "5bcd", "5ef", "5g", "5hi", "5jk",
               "6ab", "6cdefg", "6hi", "6j", "6kl", "6mn", "6opqr")
SDOMAINE <- c("1", "2OUEST", "2EST", "2EST", "3OUEST", "3EST", "3EST",
               "4OUEST", "4OUEST", "4EST", "4EST", "4EST",
               "5OUEST", "5OUEST", "5EST", "5EST", "5EST", "5EST",
               "6OUEST", "6OUEST", "6EST", "6EST", "6EST", "6EST", "6EST")
matchREGECO_SDOMAINE <- data.frame(GUIDE_ECO, SDOMAINE)
sites <- merge(sites, matchREGECO_SDOMAINE, by = "GUIDE_ECO")
plots <- plots[,colnames(plots)[which(colnames(plots) != "SHAPE")]]

TSP5 <- list()
TSP5$plots <- plots[,c("ID_PE", "LATITUDE", "LONGITUDE", "DATE_SOND")]
TSP5$sites <- sites[,c("ID_PE", "ALTITUDE", "SDOMAINE", "GUIDE_ECO", "TYPE_ECO", "CL_DRAI")]
TSP5$photoInterpretedStands <- photoInterpretedStands[,c("ID_PE", "CL_AGE", "TYPE_ECO")]
TSP5$trees <- trees[,c("ID_PE", "ETAT","ESSENCE", "CL_DHP", "TIGE_HA","HAUT_ARBRE")]
TSP5$studyTrees <- studyTrees[,c("ID_PE", "ESSENCE", "ETAGE_ARB", "DHP","HAUT_ARBRE","AGE" )]
TSP5$saplings <- saplings[, c("ID_PE", "ESSENCE", "CL_DHP", "TIGE_HA")]

saveRDS(TSP5, file = file.path(getwd(),"inst","extdata", "QcTSP5.Rds"), compress = "xz")
