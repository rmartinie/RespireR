# RespireR

Quelques fonction R pour récupérer les mesures de qualité de l’air en Auvergne-Rhône-Alpes à partir des API publiques d’Atmo AURA.

L’objectif est simple :
- lister les stations fixes
- récupérer des mesures (NO2, PM10, PM2.5, O3, SO2)
- sur une ou plusieurs stations
- pour une ou plusieurs années

Ce n’est pas un package ultra-générique, mais plutôt un outil pratique pour explorer rapidement de façon automatisée les données Atmo côté Rhône-Alpes.


## Installation

Vous pouvez installer depuis GitHub avec :

    # install.packages("remotes")
    remotes::install_github("rmartinie/RespireR")
    

## Fonctions disponibles

### `get_list_stations()`

Récupère la liste des stations fixes Atmo AURA.

Retourne un `data.frame` avec : - `id_station` - `nom_station` - `date_debut`, `date_fin` - `en_service` - `typologie`

### `get_atmo()`

Récupère les mesures **pour une année donnée**.

-   stations : vecteur d’ID ou `data.frame` issu de `get_list_stations()`
-   polluants : codes numériques (ex : `24` pour PM10, `01` pour SO2)
-   année : ex `2024`


### `get_atmo_history()`

Récupère l’historique **sur plusieurs années**, et sort tout dans un seul tableau.


## Tables des polluants

| Code | Polluant |
|-----:|----------|
|   01 | SO₂      |
|   03 | NO₂      |
|   08 | O₃       |
|   24 | PM10     |
|   39 | PM2.5    |


## Exemple d’utilisation complet

``` r
# 1. On récupère d'abord la liste des stations
df_stations_ref <- get_list_stations()

# 2. On filtre les stations qui nous intéressent
stations <- df_stations_ref %>% 
  dplyr::filter(id_station %in% c("FR20017", "FR20062"))

# 3. On lance la récupération de l'historique
df_complet <- get_atmo_bulk(
  df_stations = stations, 
  polluant_id = c(24, 01),   # PM10 et SO2
  annee_debut = 2024,
  annee_fin = 2025
)

# 4. Aperçu du résultat
print(head(df_complet))
```
