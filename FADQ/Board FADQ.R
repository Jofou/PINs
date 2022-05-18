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

#Ajouter le code des cultures
cultures<-readxl::read_excel("FADQ/1 Import/code culture.xlsx") %>%
  janitor::clean_names() %>%
  rename(culture_fadq=prox_descodprx) %>%
  mutate(culture_fadq=stringi::stri_trans_general(culture_fadq,id = "Latin-ASCII")) %>%
  mutate(culture_simple = case_when(
    grepl("foin", culture_fadq) ~ "foin",
    grepl("panic", culture_fadq) ~ "panic",
    grepl("feverole", culture_fadq) ~ "feverole",
    grepl("millet", culture_fadq) ~ "millet",
    grepl("soudan", culture_fadq) ~ "soudan",
    grepl("avoine", culture_fadq) ~ "avoine",
    grepl("ble", culture_fadq) ~ "ble",
    grepl("orge", culture_fadq) ~ "orge",
    grepl("seigle", culture_fadq) ~ "seigle",
    grepl("sarrasin", culture_fadq) ~ "sarrasin",
    grepl("triticale", culture_fadq) ~ "triticale",
    grepl("millet", culture_fadq) ~ "millet",
    grepl("canola", culture_fadq) ~ "canola",
    grepl("sorgho", culture_fadq) ~ "sorgho",
    grepl("tournesol", culture_fadq) ~ "tournesol",
    grepl("epeautre", culture_fadq) ~ "epeautre",
    grepl("lin", culture_fadq) ~ "lin",
    grepl("chanvre", culture_fadq) ~ "chanvre",
    grepl("gazon", culture_fadq) ~ "gazon",
    grepl("pois", culture_fadq) ~ "pois",
    grepl("haricot", culture_fadq) ~ "haricot",
    grepl("paturage", culture_fadq) ~ "paturage",
    grepl("soya", culture_fadq) ~ "soya",
    grepl("mais", culture_fadq) ~ "mais",
    grepl("ail", culture_fadq) ~ "ail",
    grepl("laitue", culture_fadq) ~ "laitue",
    grepl("oignon", culture_fadq) ~ "oignon",
    grepl("carotte", culture_fadq) ~ "carotte",
    grepl("piment", culture_fadq) ~ "piment",
    grepl("fraisier|fraise", culture_fadq) ~ "fraisier",
    grepl("zucchini", culture_fadq) ~ "zucchini",
    grepl("chou", culture_fadq) ~ "chou",
    grepl("radis", culture_fadq) ~ "radis",
    grepl("betterave", culture_fadq) ~ "betterave",
    grepl("epinard", culture_fadq) ~ "epinard",
    grepl("brocoli", culture_fadq) ~ "brocoli",
    grepl("poireau", culture_fadq) ~ "poireau",
    grepl("tomate", culture_fadq) ~ "tomate",
    grepl("concombre", culture_fadq) ~ "concombre",
    grepl("echalottes", culture_fadq) ~ "echalottes",
    grepl("asperge", culture_fadq) ~ "asperge",
    grepl("endive", culture_fadq) ~ "endive",
    grepl("courge", culture_fadq) ~ "courge",
    grepl("melon", culture_fadq) ~ "melon",
    grepl("tabac", culture_fadq) ~ "tabac",
    grepl("citrouille", culture_fadq) ~ "citrouille",
    grepl("pommes de terre", culture_fadq) ~ "pommes de terre",
    grepl("amenagement|agriculture|framboisier|framboise|pommier|bleuet|bleuetier|arbuste|arbustes|coniferes|gadellier|camerise|canneberges|vigne|poire|canneberges", culture_fadq) ~ "fruitiers et arbres",
    grepl("jachere|non cultivee|autres|friche|semi direct", culture_fadq) ~ "non-cultive",
    TRUE ~ as.character(.$culture_fadq))) %>%
  select(-culture_fadq) %>%
  rename(culture_fadq=culture_simple)


centroid_culture<-centroid %>%
  rename(cod=CODPRO1) %>%
  left_join(cultures, by=c("cod")) %>%
  select(-cod)

board_prepared %>% pin_write(centroid_culture, "centroid")

## * Read Pins Validation ----
data<-board_prepared %>%
  pin_read("sol_texture")

