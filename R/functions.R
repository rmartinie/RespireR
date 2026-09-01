utils::globalVariables(c("id_station", "nom_station", "valeur", "station", "polluant", "nom_polluant", ".data"))

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

  data_json$features |>
    dplyr::rename_with(~ gsub("properties.", "", .x)) |>
    dplyr::select(
      .data$id_station,
      .data$nom_station,
      .data$date_debut,
      .data$date_fin,
      .data$en_service,
      .data$typologie
    )
}


#' Récupérer les données de mesures Atmo (quotidiennes ou horaires)
#'
#' @param station_input Un vecteur d'ID de stations ou un data.frame issu de get_list_stations.
#' @param polluant_ids Vecteur de codes polluants (ex: c("24", "08")).
#' @param date_debut Date de début ("YYYY-MM-DD" ou objet Date).
#' @param date_fin Date de fin ("YYYY-MM-DD" ou objet Date). Par défaut égale à date_debut.
#' @param hourly Si TRUE, renvoie les valeurs horaires (POSIXct) plutôt que journalières (Date). Défaut FALSE.
#' @param df_stations_ref Data.frame de référence pour joindre les noms de stations.
#' @return Un data.frame des mesures ou NULL si aucune donnée n'est trouvée.
#' @export
get_atmo <- function(station_input, polluant_ids, date_debut, date_fin = date_debut,
                     hourly = FALSE, df_stations_ref = NULL) {

  # 1. Table de correspondance des polluants (Format interne à 2 chiffres)
  ref_polluants <- data.frame(
    id = c("03", "39", "24", "08", "01"),
    nom_polluant = c("NO2", "PM2.5", "PM10", "O3", "SO2"),
    stringsAsFactors = FALSE
  )

  polluant_ids <- sprintf("%02d", as.numeric(polluant_ids))
  if (length(polluant_ids) == 0) {
    stop("Aucun polluant fourni.")
  }

  # 2. Gestion des IDs de stations
  ids_to_fetch <- if (is.data.frame(station_input)) {
    as.character(station_input$id_station)
  } else {
    as.character(station_input)
  }

  date_debut <- as.character(as.Date(date_debut))
  date_fin   <- as.character(as.Date(date_fin))

  # 3. Si hourly on change le format renvoyé
  convertir_date <- function(timestamp_ms) {
    dt <- as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = "UTC")
    if (hourly) dt else as.Date(dt)
  }

  # 4. Fonction interne pour un couple (station, polluant)
  fetch_data <- function(sid, pid) {
    pid_url <- sprintf("%02d", as.numeric(pid))

    url <- paste("https://www.atmo-auvergnerhonealpes.fr/dataviz/dataviz/mesures",
                 sid, pid_url, date_debut, date_fin, sep = "/")

    tryCatch({
      res <- httr::GET(url)
      if (httr::status_code(res) != 200) return(NULL)

      content_text <- httr::content(res, "text", encoding = "UTF-8")
      raw_data <- jsonlite::fromJSON(content_text)

      if (length(raw_data) == 0) return(NULL)
      df <- as.data.frame(raw_data)
      if (nrow(df) == 0) return(NULL)

      df |>
        dplyr::select(date = 1, valeur = 2) |>
        dplyr::mutate(
          date = convertir_date(date),
          station = as.character(sid),
          polluant = pid # On garde l'ID original pour la jointure
        )
    }, error = function(e) return(NULL))
  }

  # 5. Double boucle via expand.grid
  message("Lancement : ", length(ids_to_fetch), " stations x ", length(polluant_ids), " polluants...")
  all_combinations <- expand.grid(sid = ids_to_fetch, pid = polluant_ids, stringsAsFactors = FALSE)

  results_list <- mapply(fetch_data, all_combinations$sid, all_combinations$pid, SIMPLIFY = FALSE)
  df_final <- dplyr::bind_rows(results_list)

  if (is.null(df_final) || nrow(df_final) == 0) {
    message("(!) Aucune donnée trouvée.")
    return(NULL)
  }

  # 6. Jointures finales
  df_final <- df_final |>
    dplyr::left_join(ref_polluants, by = c("polluant" = "id"))

  if (!is.null(df_stations_ref)) {
    df_final <- df_final |>
      dplyr::left_join(dplyr::select(df_stations_ref, id_station, nom_station),
                       by = c("station" = "id_station")) |>
      dplyr::select(date, valeur, station, nom_station, polluant, nom_polluant)
  } else {
    df_final <- df_final |>
      dplyr::select(date, valeur, station, polluant, nom_polluant)
  }

  df_final
}


#' Récupérer l'historique complet Atmo (quotidien ou horaire) sur une plage de dates
#'
#' @param df_stations Data.frame de stations (issu de get_list_stations).
#' @param polluant_id Vecteur de codes polluants.
#' @param date_debut Date de début ("YYYY-MM-DD" ou objet Date).
#' @param date_fin Date de fin. Par défaut aujourd'hui.
#' @param hourly Si TRUE, boucle jour par jour et renvoie des valeurs horaires.
#'   Si FALSE (défaut), boucle année par année et renvoie des valeurs journalières.
#' @return Un data.frame consolidé.
#' @export
get_atmo_bulk <- function(df_stations, polluant_id, date_debut, date_fin = Sys.Date(),
                          hourly = FALSE) {

  date_debut <- as.Date(date_debut)
  date_fin   <- as.Date(date_fin)

  if (date_debut > date_fin) {
    stop("La date de début ne peut pas être supérieure à la date de fin.")
  }

  # Construction des périodes à parcourir : jour par jour en horaire, année par année sinon
  periodes <- if (hourly) {
    jours <- seq(date_debut, date_fin, by = "day")
    lapply(jours, function(j) list(debut = j, fin = j, label = as.character(j)))
  } else {
    annees <- as.numeric(format(date_debut, "%Y")):as.numeric(format(date_fin, "%Y"))
    lapply(annees, function(an) list(
      debut = as.Date(paste0(an, "-01-01")),
      fin   = as.Date(paste0(an, "-12-31")),
      label = as.character(an)
    ))
  }

  historique_complet <- lapply(periodes, function(p) {
    message("\n>>> ", if (hourly) "Jour : " else "Année : ", p$label)
    get_atmo(
      station_input = df_stations,
      polluant_ids = polluant_id,
      date_debut = p$debut,
      date_fin = p$fin,
      hourly = hourly,
      df_stations_ref = df_stations
    )
  })

  dplyr::bind_rows(historique_complet)
}
