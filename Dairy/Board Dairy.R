# CREATE BOARD SOL TYPE ----

# 1.0 LIBRARY ----
library(pins)
library(tidyverse)

# 2.0 BOARD IMPORT ----
## * Create Board ----
board_import <- board_folder("Type sol/1 Import/", versioned = TRUE)
board_import

# Export Prepared Data
board_prepared <- board_folder("Type sol/2 Prepared/", versioned = TRUE)
board_prepared

## * Write Pins ----
# Factoriser les types de sol
#importer les séries de sol et classes texturale
serie <- readxl::read_excel("Type sol/1 Import/Copie de Banque_donnees_sols.xls",sheet="nom_sol_complet_jointure", col_names = TRUE, col_types = NULL, skip=0) %>%
  rename(CODECANSIS="Code sol") %>%
  select("Nom sol", "CODECANSIS")

texture <- readxl::read_excel("Type sol/1 Import/Copie de Banque_donnees_sols.xls",sheet="FCS", col_names = TRUE, col_types = NULL, skip=0) %>%
  select("CODECANSIS", "SABLE_DEG1","LIMON_DEG1","ARGIL_DEG1", "SABLE_DEG2","LIMON_DEG2","ARGIL_DEG2", "SABLE_DEG3","LIMON_DEG3","ARGIL_DEG3")

sol_texture<-serie %>%
  left_join(texture, by="CODECANSIS") %>%
  filter(!is.na(SABLE_DEG1)) %>%
  mutate(sable =SABLE_DEG1*0.6+SABLE_DEG2*0.4,
         limon=LIMON_DEG1*0.6+LIMON_DEG2*0.4,
         argile=ARGIL_DEG1*0.6+ARGIL_DEG2*0.4) %>%
  rename(type_sol="Nom sol") %>%
  select("type_sol", "sable", "limon", "argile") %>%
  unique()

## * Write Pins ----
#Pin prepared Data here because processing already done before pinning strategy
board_prepared %>% pin_write(sol_texture, "sol_texture")


## * Read Pins Validation ----
data<-board_prepared %>%
  pin_read("sol_texture")

