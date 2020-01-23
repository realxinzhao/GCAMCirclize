
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(lattice)
library(scales)
library(circlize)
library(RColorBrewer)

###font in graph
fontfamily = "serif"


nonyear <- c(1975, 1990, 2005)
`%notin%` = Negate(`%in%`) #create not in

noncrop <- c("Forest","FodderGrass", "Pasture", "FodderHerb", "biomass",
             "biomassoil", "corn for ethanol",  "sugar for ethanol")

Regmap <- read.csv(paste0(dirname(getwd()),"/info/Regmapping.csv"), header=TRUE, sep=",",comment.char = "#")
#Filenames = list.files(pattern = "*.csv")



C_1 <- c(
  "Forest",
  "Grassland",
  "OtherArableLand",
  "Pasture",
  "ProtectedGrassland",
  "ProtectedShrubland",
  "ProtectedUnmanagedForest",
  "ProtectedUnmanagedPasture",
  "RockIceDesert",
  "Shrubland",
  "Tundra",
  "UnmanagedForest",
  "UnmanagedPasture",
  "UrbanLand")

#11 crops
C_2 <- c(
  "Corn",
  "FiberCrop",
  "FodderGrass",
  "FodderHerb",
  "MiscCrop",
  "OilCrop",
  "OtherGrain",
  "PalmFruit",
  "Rice",
  "SugarCrop",
  "Wheat")
#3 corps; 14 corps in total
C_3 <- c(
  "biomass_grass",
  "biomass_tree",
  "Root_Tuber")
#note that only 14 crops + forest + pasture = 16 land use sectors have a yield/productivity!
Land_noncrop <- c(
  "Forest",
  "Grassland",
  "OtherArableLand",
  "Pasture",
  "Shrubland",
  "UnmanagedForest",
  "UnmanagedPasture")

Land_fixed <- c(
  "ProtectedGrassland",
  "ProtectedShrubland",
  "ProtectedUnmanagedForest",
  "ProtectedUnmanagedPasture",
  "RockIceDesert",
  "Tundra",
  "UrbanLand",
  "ProtectedForestFire",
  "ProtectedGrasslandFires",
  "GrasslandFires",
  "ForestFire")

Land_prod_yield <- c(C_2, C_3, "Forest", "Pasture")

