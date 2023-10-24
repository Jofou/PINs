# CREATE BOARD IRDA ----

# 1.0 LIBRARY ----
library(pins)
library(tidyverse)

#Set working directory
current_working_directory = "/Users/johaniefournier/Library/Mobile Documents/com~apple~CloudDocs/ADV/PINs/IRDA"
setwd(current_working_directory)

# 2.0 BOARD IMPORT ----
#Done in each individual section for this workflow

# 3.0 PROCESS DATA ----
# 3.1 PPSD
ppsd<-readxl::read_xlsx("1 Import/PPSD_2022.xlsx")

# 3.2 Monteregie ----
## * Create Board
board_import <- board_folder("1 Import/Monteregie", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Monteregie", versioned = TRUE)

#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Monteregie/Liste.xlsx")

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Monteregie/", f))%>%
      sf::st_as_sf(.)%>%
      sf::st_transform(., 3978) %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Monteregie/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf(.)%>%
  sf::st_transform(., 3978) %>%
  sf::st_make_valid()


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_Monteregie")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.3 Estrie ----
## * Create Board
board_import <- board_folder("1 Import/Estrie", versioned = TRUE)

## Export Prepared Data
board_prepared <- board_folder("2 Prepared/Estrie", versioned = TRUE)

##Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Estrie/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Estrie/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Estrie/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()

#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_Estrie")
#board_collab %>% pin_write(test_all, "TS_Monteregie")



# 3.3 Centre du Québec ----
## * Create Board
board_import <- board_folder("1 Import/Centre du Quebec", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Centre du Quebec", versioned = TRUE)

#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Centre du Quebec/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Centre du Quebec/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Centre du Quebec/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_Centre_du_quebec")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.4 Bas St-Laurent ----
## * Create Board
board_import <- board_folder("1 Import/Bas St-Laurent/", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Bas St-Laurent/", versioned = TRUE)

#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Bas St-Laurent/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Bas St-Laurent/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Bas St-Laurent/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_bas_st_laurent")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.5 Capitale-Nationale ----
## * Create Board
board_import <- board_folder("1 Import/Capitale-Nationale/", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Capitale-Nationale/", versioned = TRUE)


#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Capitale-Nationale/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Capitale-Nationale/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Capitale-Nationale/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_capitale_nationale")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.6 Lanaudière ----
## * Create Board
board_import <- board_folder("1 Import/Lanaudiere/", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Lanaudiere/", versioned = TRUE)

#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Lanaudiere/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Lanaudiere/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Lanaudiere/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_lanaudiere")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.7 Laurentides ----
## * Create Board
board_import <- board_folder("1 Import/Laurentides/", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Laurentides", versioned = TRUE)

#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Laurentides/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Laurentides/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Laurentides/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_laurentides")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.8 Mauricie ----
## * Create Board
board_import <- board_folder("1 Import/Mauricie/", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Mauricie", versioned = TRUE)

#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Mauricie/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Mauricie/", f))%>%
      #sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Mauricie/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()

#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_mauricie")
#board_collab %>% pin_write(test_all, "TS_Monteregie")


# 3.9 Chaudière-Appalaches ----
## * Create Board
board_import <- board_folder("1 Import/Chaudiere-Appalaches/", versioned = TRUE)

# Export Prepared Data
board_prepared <- board_folder("2 Prepared/Chaudiere-Appalaches", versioned = TRUE)


# Repair imported data
f="Pedo_21L06202"
repair <-sf::read_sf(dsn = paste0("1 Import/Chaudiere-Appalaches/",f)) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()
mapview::mapview(repair)
sf::st_write(repair, paste0("1 Import/Chaudiere-Appalaches/", f, "/", f),append=TRUE, driver = "ESRI Shapefile")


#Data from: https://www.irda.qc.ca/fr/services/protection-ressources/sante-sols/information-sols/etudes-pedologiques/
# Liste des feuillets
feuillets<-readxl::read_xlsx("1 Import/Chaudiere-Appalaches/Liste.xlsx") %>%
  as.data.frame()

fe<-feuillets %>%
  {unique(.$no)}

# Importer les données réelles
for(f in fe){ #Fermes
  tryCatch({
    name <- paste0("shape_sf_", f)
    test <-sf::read_sf(dsn = paste0("1 Import/Chaudiere-Appalaches/", f))%>%
      sf::st_set_crs("EPSG:4326") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      mutate(feuillet = f)
    assign(name, test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Créer une liste comptenant tous les fichiers à joindre
# Empty list
test <- list()
for(f in fe){ #Fermes
  tryCatch({
    #extract data frame
    dat <- get(paste0("shape_sf_", f))
    # Add to list
    test <- append(test, list(dat))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#joint dataset
PPSD_2022<-readxl::read_xlsx("1 Import/Chaudiere-Appalaches/PPSD_2022.xlsx") %>%
  janitor::clean_names()

test_all <- test %>%
  reduce(bind_rows) %>%
  left_join(PPSD_2022, by=c("lien1")) %>%
  sf::st_as_sf() %>% #Transformer
  sf::st_transform("EPSG:4326") %>%
  sf::st_make_valid()


test<-test_all %>%
  sf::st_set_crs("EPSG:4326") %>%
  sf::st_transform("EPSG:3978") %>%
  select(sable, feuillet) %>%
  sf::st_centroid() %>%
  sf::st_make_valid() %>%
  mutate(longitude = sf::st_coordinates(.)[,1],
         latitude  = sf::st_coordinates(.)[,2]) %>%
  sf::st_transform("EPSG:4326") %>%
  as.data.frame()

mapview::mapview(test_all)


#Pin Prepared Data
board_prepared %>% pin_write(test_all, "TS_chaudiere_appalaches")
#board_collab %>% pin_write(test_all, "TS_Monteregie")



