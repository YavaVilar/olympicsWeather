library(httr2)
library(tibble)
library(tidygeocoder)
library(plotly)

###-----------------------###
#     Demander l'API        #
###-----------------------###


url <- "https://api.open-meteo.com/v1/forecast"

request(url) |>
  req_url_query(latitude=48.85, longitude=2.35, hourly=c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"), .multi = "comma") |>
  req_perform() |> #On exécute la demande
  resp_body_json() |>
  as_tibble() |>
  View()


perform_request <- function(lat, long) {
  url <- "https://api.open-meteo.com/v1/forecast"
  response_table <-
    request(url) |>
    req_url_query(latitude=lat, longitude=long, hourly=c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"), .multi="comma") |>
    req_perform () |>
    resp_body_json() |>
    tibble::as_tibble()
  return(response_table)
}


resp <- perform_request(48.85, 2.35)

str(resp$hourly[2][[1]], 1)

unlist(resp$hourly[2][[1]], 1)

unnest_data <- function(resp){
tibble(data_heure=unlist(resp$hourly[1][[1]]),
         temperature_celsius=unlist(resp$hourly[2][[1]]),
         temperature_ressentie_celsius=unlist(resp$hourly[3][[1]]),
         precipitation_proba=unlist(resp$hourly[4][[1]]),
         precipitation=unlist(resp$hourly[5][[1]]))
}

perform_request(48.85, 3.35) |>  unnest_data() -> y
plot(y$temperature_celsius)

#Le tableau (tibble) comprend des informations générales liées à la demande météorologique ainsi qu'à l'emplacement des prévisions. 
#Il inclut des détails tels que la latitude, la longitude, l'heure de génération, le décalage UTC, le fuseau horaire, et d'autres éléments. Ce tableau est constitué de 5 lignes et 9 colonnes.
#La colonne "hourly" se présente sous forme de liste, englobant diverses données telles que l'heure (time), la température à 2 mètres (temperature_2m), la sensation thermique (apparent_temperature), la probabilité de précipitations (precipitation_probability), et les précipitations effectives (precipitation). Pour obtenir des prévisions pour tous les sites des Jeux Olympiques, des ajustements sont requis pour les coordonnées de latitude et de longitude

###------------------------###
#     Tests de la fontion    #
###------------------------###

usethis::use_test("unnest_response")
data_test <- y[c(1:5),]
library(testthat)

# 1: Vérification du nombre de lignes pour une prévision de 7 jours
test_that("La fonction renvoie le bon nombre de lignes", {
  expect_equal(nrow(data_test), 168, info = "Le résultat devrait contenir 168 lignes.")
})

# 2: Vérification du nom des colonnes en sortie  
expected_columns <- c("date_heure", "temperature_celsius", "temperature_ressentie", "precipitation_proba", "precipitation_mm")
expect_equal(names(data_test), expected_columns, info = "Les noms des colonnes devraient correspondre à ceux du dataframe.")

# 3: Vérification des valeurs de la colonne temperature
test_that("Les valeurs de la colonne temperature correspondent aux valeurs proposées en entrée", {
  # Supongamos que tienes un vector de temperaturas de entrada llamado 'temperatures_input'
  expect_equal(data_test$temperature_celsius, temperatures_input, 
               info = "Les valeurs de la colonne temperature devraient correspondre aux valeurs proposées en entrée.")
})

# 4: Vérification du nombre de colonnes en sortie
test_that("Le nombre de colonnes en sortie est correct", {
  expect_equal(ncol(data_test), length(expected_columns), 
               info = "Le nombre de colonnes en sortie devrait être égal à celui du vecteur 'expected_columns'.")
})

###-----------------------###
#      Adress en GPS        #
###-----------------------###

address_to_gps <- function(location) {
  # dataframe temporaire pour utilisation de geocode
  df_temp <- data.frame(address = location, stringsAsFactors = FALSE)

  # Appel à geocode avec le dataframe temporaire
  result <- geocode(
    .tbl = df_temp,
    address = "address",
    method = 'osm',
    limit = 1
  )
  # Vérifie si des coordonnées ont été trouvées
  if(nrow(result) > 0 && !is.na(result$lat[1]) && !is.na(result$long[1])) {
    return(c(result$lat[1], result$long[1]))
  } else {
    # Retourne une erreur si aucune coordonnée n'a été trouvée
    return("Aucune coordonnée GPS trouvée pour l'adresse fournie.")
  }
}

###-----------------------------------###
#      Fct interne Adress en GPS        #
###-----------------------------------###


get_gps_coordinate <- function(address) {
  address_to_gps(address)
}


###-----------------------###
#     Fonction numérique    #
###-----------------------###


#' get_forecast.numeric
#'
#' @param coordinates
#'
#' @return un dataframe
#' @export
#'
#'
get_forecast.numeric <- function(coordinates) {
  # Vérifier que l'entrée est un vecteur numérique de taille 2
  if (!is.numeric(coordinates) || length(coordinates) != 2) {
    stop("L'entrée doit être un vecteur numérique de taille 2.")
  }

  # Utiliser perform_request pour interroger une API météo avec les coordonnées
  weather_data <- perform_request(coordinates[1], coordinates[2])

  # Utiliser unnest_response pour structurer la réponse en un tibble lisible
  result <- unnest_data(weather_data)

  return(result)
}

###-----------------------###
#     Fonction caractère    #
###-----------------------###

#' get_forecast.character
#'
#' @param address
#'
#' @return un dataframe
#' @export
#'
#'
get_forecast.character <- function(address) {
  # Verificar que la entrada sea una cadena de caracteres de tamaño 1
  if (!is.character(address) || length(address) != 1) {
    stop("L'entrée doit être une chaîne de caractères de taille 1.")
  }

  # Convertir la dirección en coordenadas GPS utilizando get_gps_coordinate
  coordinates <- get_gps_coordinate(address)

  # Llamar a get_forecast.numeric con las coordenadas para obtener los pronósticos
  return(get_forecast.numeric(coordinates))
}



###-----------------------###
#     Fonction générique    #
###-----------------------###


#' get_forecast
#'
#' @param location(coordonnées_géographiques_ou_nom_du_site)
#'
#' @return un dataframe
#' @export
#' @import tidygeocoder httr2 tibble
get_forecast <- function(location) {
  UseMethod("get_forecast", location)
}



###------------------###
#     Point bonus      #
###------------------###


#' visualiser_pronostic
#'
#' @param pronostic
#'
#' @return quatre graphiques intéractifs
#' @export
#' @import plotly
visualiser_pronostic <- function(pronostic){
  p1 <- plot_ly(pronostic, x = ~data_heure, y = ~temperature_celsius, type = 'scatter', mode = 'lines', name = 'Température (°C)') %>%
    layout(xaxis = list(showticklabels = FALSE))  # Desactivar las etiquetas del eje x para el gráfico superior izquierdo

  p2 <- plot_ly(pronostic, x = ~data_heure, y = ~temperature_ressentie_celsius, type = 'scatter', mode = 'lines', name = 'Température ressentie (°C)')%>%
    layout(xaxis = list(showticklabels = FALSE))

  p3 <- plot_ly(pronostic, x = ~data_heure, y = ~precipitation, type = 'scatter', mode = 'bars', name = 'Précipitation (mm)') %>%
    layout(title = "Précipitation",
           xaxis = list(title = "Date et Heure"),
           yaxis = list(title = "Précipitation (mm)"),
           hovermode = "x unified")

  p4 <- plot_ly(pronostic, x = ~data_heure, y = ~precipitation_proba, type = 'scatter', mode = 'bars', name = 'Probabilité de précipitation (%)') %>%
    layout(title = "Prévision de la météo à 7 jours",
           xaxis = list(title = "Date et Heure"),
           yaxis = list(title = "Probabilité de précipitation (%)"),
           hovermode = "x unified")

  subplot(p1, p2, p3, p4, nrows = 2, titleX = FALSE) %>%
    layout(legend = list(traceorder = 'normal', tracegroupgap = 5), xaxis = list(showticklabels = FALSE))
}


#' get_forecast_visualisation
#'
#' @param location(coordonnées_géographiques_ou_nom_du_site)
#'
#' @return quatre graphiques intéractifs
#' @export
#' @import plotly
get_forecast_visualisation <- function(location) {
  pronostic <- get_forecast(location)
  visualisation <- visualiser_pronostic(pronostic)
  return(list(pronostic = pronostic, visualisation = visualisation))
}




