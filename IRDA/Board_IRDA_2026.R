# ============================================================
# BOARD IRDA 2026 — Pipeline reproductible
# Couverture pédologique du Québec + tables de référence IRDA
#
# Sources :
#   - couverture_pedologique_2026_01.gpkg (1.33 GB, janvier 2026)
#   - Irda-ProprietesPedologiques-Basededonnees-2026.xlsx
#   - Irda-BDHPQ-Basededonnees-Avril2026.xlsx
#   - Irda-ProprietesPhysicoChimiquesCoucheSol-Basededonnees-2026.xlsx
#
# Structure PINs de sortie :
#   2 Prepared/{Region}/TS_{region}  (un pin par région agricole)
#
# Régions couvertes :
#   Monteregie, Estrie, Centre_du_Quebec, Bas_St_Laurent,
#   Capitale_Nationale, Lanaudiere, Laurentides, Mauricie,
#   Chaudiere_Appalaches
# ============================================================


# 1.0 LIBRAIRIES ----
library(pins)
library(tidyverse)
library(sf)
library(readxl)
library(janitor)


# 2.0 CHEMINS ----
dir_base    <- "/Users/johaniefournier/Library/Mobile Documents/com~apple~CloudDocs/ADV/PINs/IRDA"
dir_source  <- file.path(dir_base, "0 Source")
dir_prep    <- file.path(dir_base, "2 Prepared")

path_gpkg   <- file.path(dir_source, "couverture_pedologique_2026_01.gpkg")
path_pps    <- file.path(dir_source, "Irda-ProprietesPedologiques-Basededonnees-2026.xlsx")
path_bdhp   <- file.path(dir_source, "Irda-BDHPQ-Basededonnees-Avril2026.xlsx")
path_ppc    <- file.path(dir_source, "Irda-ProprietesPhysicoChimiquesCoucheSol-Basededonnees-2026.xlsx")


# 3.0 VÉRIFICATION DES FICHIERS SOURCE ----
sources <- c(path_gpkg, path_pps, path_bdhp, path_ppc)
missing <- sources[!file.exists(sources)]
if (length(missing) > 0) {
  stop("Fichiers manquants dans 0 Source :\n", paste(missing, collapse = "\n"))
}
message("✓ Tous les fichiers source présents")


# 4.0 CHARGER LES TABLES DE RÉFÉRENCE ----
message("Chargement des tables de référence...")

pps <- read_xlsx(path_pps) |>
  clean_names()

bdhp <- read_xlsx(path_bdhp) |>
  clean_names()

# PPC ÉESSAQ — horizon Ap1 seulement (couche de surface cultivée)
ppc_ap1 <- read_xlsx(path_ppc) |>
  clean_names() |>
  filter(str_detect(tolower(hzn), "^ap1|^ap 1")) |>
  # Garder les variables hydrologiques clés
  select(
    code_siscan,
    serie,
    hzn,
    conductivite_hydraulique,
    macroporosite,
    mva3,
    mos,
    argile,
    limon,
    sable,
    sable_tres_fin,
    cec_calculee,
    p_h_eau
  )

# PPC Inventaire 1990 — horizon 1 (couche de surface)
ppc_inv_h1 <- ppc_inv |>
  filter(horizon == 1) |>
  select(
    code_sol,
    horizon,
    cond_hyd,
    densite,
    sable,
    limon,
    argile,
    mo,
    cec,
    p_h_eau
  ) |>
  rename(
    cond_hyd_inv1990      = cond_hyd,
    densite_inv1990       = densite,
    mo_inv1990            = mo,
    cec_inv1990           = cec,
    p_h_eau_inv1990       = p_h_eau,
    sable_inv1990         = sable,
    limon_inv1990         = limon,
    argile_inv1990        = argile
  )

message(sprintf("✓ PPC Inventaire 1990 Ap1 : %d séries", nrow(ppc_inv_h1)))

message(sprintf(
  "✓ PPS : %d composantes | BDHP : %d composantes | PPC Ap1 : %d séries",
  nrow(pps), nrow(bdhp), nrow(ppc_ap1)
))


# 5.0 CHARGER LE GEOPACKAGE PROVINCIAL ----
message("Chargement du geopackage provincial (peut prendre 1-2 min)...")

# Inspecter les layers disponibles
layers <- st_layers(path_gpkg)
message("Layers disponibles : ", paste(layers$name, collapse = ", "))

# Charger la couverture géospatiale
couverture <- st_read(path_gpkg, layer = "Couverture_pedologique", quiet = TRUE) |>
  clean_names()

# Charger la table de relation polygone → composantes
couverture_pps <- st_read(path_gpkg, layer = "Couverture_pps", quiet = TRUE) |>
  clean_names() |>
  st_drop_geometry()

message(sprintf(
  "✓ Couverture : %d polygones | Couverture_pps : %d composantes",
  nrow(couverture), nrow(couverture_pps)
))


# 6.0 JOINTURES PROVINCIALES ----
message("Jointures en cours...")

sol_qc <- couverture |>
  left_join(couverture_pps, by = "code_polygone") |>
  left_join(pps, by = "composante") |>
  left_join(
    bdhp |>
      select(code_sol, groupe_hydrologique, permeabilite,
             code_structure, k, niveau_risque, texture,
             sable, limon, argile, sable_tres_fin, mo_predite) |>
      distinct(code_sol, .keep_all = TRUE),
    by = "code_sol"
  ) |>
  left_join(ppc_ap1, by = c("code_sol" = "code_siscan")) |>
  # Séries → conductivité hydraulique Inventaire 1990
  left_join(ppc_inv_h1, by = "code_sol") |>
  rename(
    appellation_cartographique = appellation_cartographique.x,
    no_etude                   = no_etude.x,
    sable_bdhp                 = sable.x,
    limon_bdhp                 = limon.x,
    argile_bdhp                = argile.x,
    sable_tf_bdhp              = sable_tres_fin.x,
    sable_ppc                  = sable.y,
    limon_ppc                  = limon.y,
    argile_ppc                 = argile.y,
    sable_tf_ppc               = sable_tres_fin.y
  ) |>
  select(
    -appellation_cartographique.y,
    -no_etude.y
  ) |>
  sf::st_make_valid()

message(sprintf("✓ Objet QC complet : %d polygones, %d colonnes",
                nrow(sol_qc), ncol(sol_qc)))


# 7.0 VALIDATION DE LA COUVERTURE ----
message("\n--- Validation des jointures ---")

n_total  <- nrow(sol_qc)
n_sols   <- sol_qc |> st_drop_geometry() |>
  filter(!sorte %in% c("Non-sol", "Ne s'applique pas") | is.na(sorte)) |>
  nrow()

val <- tibble(
  table = c(
    "PPS (classe_drainage)",
    "BDHP (groupe_hydrologique) — sols seulement",
    "PPC Inventaire 1990 (cond_hyd) — sols seulement",
    "PPC ÉESSAQ (macroporosite) — sols seulement"
  ),
  n_na = c(
    sum(is.na(sol_qc$classe_drainage)),
    sol_qc |> st_drop_geometry() |>
      filter(sorte == "Minéral") |>
      pull(groupe_hydrologique) |> is.na() |> sum(),
    sol_qc |> st_drop_geometry() |>
      filter(sorte == "Minéral") |>
      pull(cond_hyd_inv1990) |> is.na() |> sum(),
    sol_qc |> st_drop_geometry() |>
      filter(sorte == "Minéral") |>
      pull(macroporosite) |> is.na() |> sum()
  ),
  n_ref = c(
    nrow(sol_qc),
    n_sols, n_sols, n_sols
  )
) |>
  mutate(
    pct_couvert = round((1 - n_na / n_ref) * 100, 1),
    pct_na      = round(n_na / n_ref * 100, 1)
  )

print(val)


# Alerte si couverture PPS < 95%
if (val$pct_couvert[1] < 95) {
  warning(sprintf(
    "Couverture PPS insuffisante : %.1f%% seulement. Vérifier les clés de jointure.",
    val$pct_couvert[1]
  ))
}


# 8.0 DÉCOUPAGE PAR RÉGION ET PIN_WRITE ----
# Utiliser les limites administratives du Québec via rgeoboundaries
message("\nChargement des limites administratives...")

# Québec régions administratives — niveau 1 (provinces) → niveau 2 (régions)
# On utilise les données StatCan disponibles dans le package cancensus ou rgeoboundaries
regions_qc <- rgeoboundaries::gb_adm2("CAN") |>
  filter(str_detect(shapeName, regex(
    "montér|estrie|chaudière|capitale|mauricie|centre.*québec|lanaudière|laurentides|bas.*saint.*laurent",
    ignore_case = TRUE
  ))) |>
  st_transform(st_crs(sol_qc))

# Mapping nom région → slug pin
region_map <- tribble(
  ~pattern,                    ~pin_name,
  "montér",                    "TS_monteregie",
  "estrie",                    "TS_estrie",
  "chaudière",                 "TS_chaudiere_appalaches",
  "capitale",                  "TS_capitale_nationale",
  "mauricie",                  "TS_mauricie",
  "centre.*québec",            "TS_centre_du_quebec",
  "lanaudière",                "TS_lanaudiere",
  "laurentides",               "TS_laurentides",
  "bas.*saint.*laurent",       "TS_bas_st_laurent"
)

message("Découpage et écriture des pins par région...")

for (i in seq_len(nrow(region_map))) {
  pattern  <- region_map$pattern[i]
  pin_name <- region_map$pin_name[i]

  # Polygone de la région
  region_poly <- regions_qc |>
    filter(str_detect(tolower(shapeName), pattern))

  if (nrow(region_poly) == 0) {
    warning(sprintf("Région non trouvée pour le pattern : %s", pattern))
    next
  }

  # Intersection spatiale
  sol_region <- sol_qc |>
    st_intersection(st_union(region_poly)) |>
    st_transform(3978) |>    # NAD83 Canada Atlas — projection de travail
    st_make_valid()

  # Pin write
  board <- board_folder(
    file.path(dir_prep, str_remove(pin_name, "^TS_") |> str_replace_all("_", " ") |> str_to_title()),
    versioned = TRUE
  )

  board |> pin_write(sol_region, pin_name)

  message(sprintf("  ✓ %s : %d polygones", pin_name, nrow(sol_region)))
}

message("\n✓ Pipeline IRDA 2026 complété.")
