# CREATE BOARD FADQ ----

# 1.0 LIBRARY ----
library(pins)
library(tidyverse)

# 2.0 BOARD ----
## * Create Board ----
board_import <- board_folder("FADQ/1 Import/", versioned = TRUE)
board_import

board_prepared <- board_folder("FADQ/2 Prepared/", versioned = TRUE)
board_prepared

## * FADQ data Base ----
#Note: excel can open .dbf files. I opened them with excel and save them as .csv
# setwd("FADQ/1 Import/")
# idanpar <- list.files (pattern="^BDPPAD.*\\.csv$") %>%
#   map_df(~data.table::fread(.,encoding = "Latin-1",select=c("AN", "IDANPAR", "CODPRO1", "SUPHEC")))
# setwd("/Users/johaniefournier/Library/Mobile Documents/com~apple~CloudDocs/ADV/PINs")
# getwd()
#
# board_prepared %>% pin_write(idanpar, "idanpar")

## * FADQ avec Centroid ----
# C'est possible de le faire dans QGIS:
# 1) Layer->Add Layer->Add vector Layer
# 2) Vector->Geometry tools-> Centroid
# 3) Dans les couches affichées dans l'écran Layer de gauche: bouton de droit sur la couche centroid->Save as csv, spécifier le nom et l'emplacement de la sauvegarde et AB_XY

#C'est aussi possible de faire la même chose dans R
#importer le nouveau .shp
shape<- sf::read_sf(dsn = "FADQ/1 Import/BDPPAD_v03_2021/", layer = "BDPPAD_v03_AN_2021_s_20220322")
#Identifier les centroides
centroid_shape<-sf::st_centroid(shape) %>%
  sf::st_transform(., '+proj=longlat +ellps=GRS80 +no_defs')
#enregistrer la base de données
sf::st_write(centroid_shape, "FADQ/1 Import/st_centroid_shape_2021.csv", layer_options="GEOMETRY=AS_XY")

#Regrouper tous les centroides
setwd("FADQ/1 Import/")
centroid <- list.files (pattern="^st_centroid_shape.*\\.csv$") %>%
  map_df(~data.table::fread(.,encoding = "Latin-1",select=c("X", "Y", "AN", "IDANPAR", "CODPRO1", "SUPHEC")))
setwd("/Users/johaniefournier/Library/Mobile Documents/com~apple~CloudDocs/ADV/PINs")
getwd()

board_prepared %>% pin_write(centroid, "centroid")


## * Read Pins Validation ----
data<-board_prepared %>%
  pin_read("sol_texture")

