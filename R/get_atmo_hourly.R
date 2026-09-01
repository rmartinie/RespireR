
utils::globalVariables(c("id_station", "nom_station", "valeur", "station", "polluant", "nom_polluant", ".data"))

#' @param station_input Un vecteur d'ID de stations ou un data.frame issu de get_list_stations.
#' @param polluant_ids Vecteur de codes polluants (ex: c("24", "08")).
#' @param year Année au format numérique (ex: 2024).
#' @param df_stations_ref Data.frame de référence pour joindre les noms de stations.
#' @return Un data.frame des mesures ou NULL si aucune donnée n'est trouvée.
#' @export

get_atmo_hourly <- function(station_input, polluant_ids, jour, df_stations_ref = NULL) {

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

  jour_debut <- as.character(as.Date(jour))
  jour_fin   <- as.character(as.Date(jour))

  # 3. Fonction interne pour un couple (station, polluant)
  fetch_data <- function(sid, pid) {

    # On s'assure que le PID envoyé à l'URL est toujours sur 2 chiffres avec un zéro initial
    # sprintf("%02d", ...) transforme 8 en "08" et garde "08" tel quel.
    pid_url <- sprintf("%02d", as.numeric(pid))

    url <- paste(
      "https://www.atmo-auvergnerhonealpes.fr/dataviz/dataviz/mesures",
      sid,
      pid_url,
      jour_debut,
      jour_fin,
      sep = "/"
    )

    tryCatch({
      res <- httr::GET(url)
      if (httr::status_code(res) != 200) return(NULL)

      content_text <- httr::content(res, "text", encoding = "UTF-8")
      raw_data <- jsonlite::fromJSON(content_text)

      if (length(raw_data) == 0) return(NULL)
      df <- as.data.frame(raw_data)
      if (nrow(df) == 0) return(NULL)

      df <- df |>
        dplyr::select(date = 1, valeur = 2) |>
        dplyr::mutate(
          date = as.POSIXct(date / 1000, origin = "1970-01-01", tz = "UTC"),
          station = as.character(sid),
          polluant = pid # On garde l'ID original pour la jointure
        )
      return(df)
    }, error = function(e) return(NULL))
  }

  # 4. Double boucle via expand.grid
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



#' Récupérer l'historique horaire sur plusieurs jours
#'
#' @param df_stations Data.frame de stations (issu de get_list_stations).
#' @param polluant_id Vecteur de codes polluants.
#' @param jour_debut Jour de début au format "YYYY-MM-DD".
#' @param jour_fin Jour de fin au format "YYYY-MM-DD".
#' @return Un data.frame consolidé des mesures horaires.
#' @export




get_atmo_bulk_hourly <- function(
    df_stations,
    polluant_id,
    jour_debut,
    jour_fin = Sys.Date()
) {

  jour_debut <- as.Date(jour_debut)
  jour_fin <- as.Date(jour_fin)

  if (jour_debut > jour_fin) {
    stop("Le jour de début ne peut pas être supérieur au jour de fin.")
  }

  jours <- seq(
    jour_debut,
    jour_fin,
    by = "day"
  )

  historique_complet <- lapply(jours, function(jour) {

    message("\n>>> Jour : ", jour)

    get_atmo_hourly(
      station_input = df_stations,
      polluant_ids = polluant_id,
      jour = jour,
      df_stations_ref = df_stations
    )
  })

  df_final_hourly <- dplyr::bind_rows(historique_complet)

  return(df_final_hourly)
}
