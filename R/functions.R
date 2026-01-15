#' Récupérer la liste des stations Atmo Auvergne-Rhône-Alpes
#'
#' @return Un data.frame contenant les colonnes id_station, nom_station, date_debut, date_fin, en_service et typologie.
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr rename_with select
get_list_stations <- function() {
  url <- "https://sig.atmo-auvergnerhonealpes.fr/geoserver/opendata/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=opendata:stations_fixes_en_service&outputFormat=application/json"

  message("Récupération de la liste des stations...")

  data_json <- jsonlite::fromJSON(url, flatten = TRUE)

  stations_df <- data_json$features |>
    dplyr::rename_with(~ gsub("properties.", "", .x)) |>
    dplyr::select(
      .data$id_station,
      .data$nom_station,
      .data$date_debut,
      .data$date_fin,
      .data$en_service,
      .data$typologie
    )

  return(stations_df)
}



#' Récupérer les données de mesures pour une année précise
#'
#' @param station_input Un vecteur d'ID de stations ou un data.frame issu de get_list_stations.
#' @param polluant_ids Vecteur de codes polluants (ex: c("24", "08")).
#' @param year Année au format numérique (ex: 2024).
#' @param df_stations_ref Data.frame de référence pour joindre les noms de stations.
#' @return Un data.frame des mesures ou NULL si aucune donnée n'est trouvée.
#' @export
get_atmo <- function(station_input, polluant_ids, year, df_stations_ref = NULL) {

  # 1. Table de correspondance des polluants (Format interne à 2 chiffres)
  ref_polluants <- data.frame(
    id = c("03", "39", "24", "08", "01"),
    nom_polluant = c("NO2", "PM2.5", "PM10", "O3", "SO2"),
    stringsAsFactors = FALSE
  )

  # On normalise les IDs en entrée au format "08" pour la jointure
  polluant_ids <- sprintf("%02d", as.numeric(polluant_ids))

  if (length(polluant_ids) == 0) {
    stop("Aucun polluant fourni.")
  }
  # 2. Gestion des IDs de stations
  if (is.data.frame(station_input)) {
    # On s'assure que l'ID est bien en caractère
    ids_to_fetch <- as.character(station_input$id_station)
  } else {
    ids_to_fetch <- as.character(station_input)
  }

  date_debut <- paste0(year, "-01-01")
  date_fin   <- paste0(year, "-12-31")

  # 3. Fonction interne pour un couple (station, polluant)
  fetch_data <- function(sid, pid) {

    # CRUCIAL : L'API Atmo attend l'ID brut pour l'URL (ex: 8 et non 08)
    # C'est ce qui causait l'erreur dans votre version package
    pid_url <- as.numeric(pid)

    url <- paste("https://www.atmo-auvergnerhonealpes.fr/dataviz/dataviz/mesures",
                 sid, pid_url, date_debut, date_fin, sep = "/")

    tryCatch({
      res <- httr::GET(url)
      if (httr::status_code(res) != 200) return(NULL)

      # Lecture du contenu JSON
      content_text <- httr::content(res, "text", encoding = "UTF-8")
      raw_data <- jsonlite::fromJSON(content_text)

      # Vérification si données vides
      if (length(raw_data) == 0) return(NULL)
      df <- as.data.frame(raw_data)
      if (nrow(df) == 0) return(NULL)

      # Formatage du dataframe
      df <- df |>
        dplyr::select(date = 1, valeur = 2) |>
        dplyr::mutate(
          date = as.Date(as.POSIXct(date / 1000, origin = "1970-01-01", tz = "UTC")),
          station = as.character(sid),
          polluant = pid # On garde l'ID "08" pour la jointure avec ref_polluants
        )
      return(df)
    }, error = function(e) return(NULL))
  }

  # 4. Double boucle via expand.grid (Votre logique de base)
  message("Lancement : ", length(ids_to_fetch), " stations x ", length(polluant_ids), " polluants...")
  all_combinations <- expand.grid(sid = ids_to_fetch, pid = polluant_ids, stringsAsFactors = FALSE)

  results_list <- mapply(fetch_data, all_combinations$sid, all_combinations$pid, SIMPLIFY = FALSE)
  df_final <- dplyr::bind_rows(results_list)

  if (is.null(df_final) || nrow(df_final) == 0) {
    message("(!) Aucune donnée trouvée.")
    return(NULL)
  }

  # 5. Jointures finales

  # Ajout du nom du polluant
  df_final <- df_final |>
    dplyr::left_join(ref_polluants, by = c("polluant" = "id"))

  # Ajout du nom de la station si la référence est fournie
  if (!is.null(df_stations_ref)) {
    df_final <- df_final |>
      dplyr::left_join(dplyr::select(df_stations_ref, id_station, nom_station),
                       by = c("station" = "id_station")) |>
      dplyr::select(date, valeur, station, nom_station, polluant, nom_polluant)
  } else {
    df_final <- df_final |>
      dplyr::select(date, valeur, station, polluant, nom_polluant)
  }

  return(df_final)
}

#' Récupérer l'historique complet sur plusieurs années
#'
#' @param df_stations Data.frame de stations (issu de get_list_stations).
#' @param polluant_id Vecteur de codes polluants.
#' @param annee_debut Année de départ.
#' @param annee_fin Année de fin (par défaut année en cours).
#' @return Un data.frame consolidé.
#' @export
get_atmo_bulk <- function(df_stations, polluant_id, annee_debut, annee_fin = as.numeric(format(Sys.Date(), "%Y"))) {

  if (annee_debut > annee_fin) {
    stop("L'année de début ne peut pas être supérieure à l'année de fin.")
  }

  annees <- annee_debut:annee_fin

  historique_complet <- lapply(annees, function(an) {
    message("\n>>> Année : ", an)
    get_atmo(station_input = df_stations, polluant_ids = polluant_id, year = an, df_stations_ref = df_stations)
  })

  df_final <- dplyr::bind_rows(historique_complet)
  return(df_final)
}
