renv::restore()
library(yaml)
secrets<-read_yaml("secrets.yaml")
# X_API_ID<-read_yaml("secrets.yaml")[[1]]
# X_API_KEY<-read_yaml("secrets.yaml")[[2]]

#corr
secrets <- yaml::read_yaml("secrets.yaml")
X_API_ID <- secrets$X_API_ID
X_API_KEY <- secrets$X_API_KEY

# ROUTES_API_URL<-"https://api.traveltimeapp.com/v4/time-map"

#corr
ROUTES_API_URL <- "https://api.traveltimeapp.com/v4/routes"

#corr

request_body <- '
{
  "locations": [
    {
      "id": "point-from",
      "coords": {
        "lat": 51.5119637,
        "lng": -0.1279543
      }
    },
    {
      "id": "point-to-1",
      "coords": {
        "lat": 51.5156177,
        "lng": -0.0919983
      }
    }
  ],
  "departure_searches": [
    {
      "id": "departure-search",
      "transportation": {
        "type": "public_transport"
      },
      "departure_location_id": "point-from",
      "arrival_location_ids": [
        "point-to-1"
      ],
      "departure_time": "2024-06-26T18:00:00.000Z",
      "properties": [
        "travel_time",
        "route"
      ],
      "range": {
        "enabled": true,
        "max_results": 5,
        "width": 900
      }
    }
  ]
}'

  
#corr
  
headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "X-Application-Id" = X_API_ID,
    "X-Api-Key" = X_API_KEY
  )
  

# corr

response <- httr::POST(ROUTES_API_URL, body = request_body, encode = "json", headers)

content <- httr::content(response)

if (httr::status_code(response) == 200) {
  print("La requête a bien été traitée")
  content <- httr::content(response, as = "parsed")
  print(content)
} else {
  # Affichage d'un message d'erreur si le code de la réponse n'est pas 200
  print(sprintf("Une erreur est survenue. Code de la réponse : %d", httr::status_code(response)))
}

# 
# get_travel_time_api_response<-function(x1,y1,x2,y2){
#   httr::POST(ROUTES_API_URL,body =
# '{
#   "locations": [
#     {
#       "id": "point-from",
#       "coords": {
#         "lat": x1,
#         "lng": y1
#       }
#     },
#     {
#       "id": "point-to-1",
#       "coords": {
#         "lat": x2,
#         "lng": y2
#       }
#     }
#   ],
#   "departure_searches": [
#     {
#       "id": "departure-search",
#       "transportation": {
#         "type": "public_transport"
#       },
#       "departure_location_id": "point-from",
#       "arrival_location_ids": [
#         "point-to-1"
#       ],
#       "departure_time": "2024-06-26T18:00:00.000Z",
#       "properties": [
#         "travel_time",
#         "route"
#       ],
#       "range": {
#         "enabled": true,
#         "max_results": 5,
#         "width": 900
#       }
#     }
#   ]
# }'
#   , encode = "json", headers)
#   }
# 
# essai<-get_travel_time_api_response("51.5119637","-0.1279543","51.5156177","-0.0919983")
# 
# response <- httr::POST(ROUTES_API_URL, body = request_body, encode = "json", headers)


#corr

get_travel_time_api_response <- function(api_url, request_body) {
  # On prépare les headers
  headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "X-Application-Id" = X_API_ID,
    "X-Api-Key" = X_API_KEY
  )
  ## On envoie la requête avec les headers spécifiés
  response <- httr::POST(api_url, body = request_body, encode = "json", headers)
  
  # On vérifie s'il y a eu une erreur
  if (!httr::http_error(response)) {
    return(list(
      "Content" = httr::content(response, as = "parsed"),
      "Status_code" = httr::status_code(response)
    ))
  } else {
    # On affiche une message d'avertissement lorsque la requête n'a rien renvoyé
    warning("Failed to retrieve data: ", httr::http_status(response)$message)
    return(list(
      "Content" = NA,
      "Status_code" = httr::status_code(response)
    ))
  }
}



response_from_function <- get_travel_time_api_response(ROUTES_API_URL, request_body)

#corr

list_itinerary <- response_from_function[[1]]$results[[1]]$locations[[1]]$properties
print(list_itinerary)


##################Partie 2

# On définit l'URL des données
library(dplyr)
STATIONS_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/d22ba593-90a4-4725-977c-095d1f654d28"

stations_data <- read.csv2(STATIONS_DATA_URL)

stations_data%>%filter(libelle=="Lille-Flandres")%>%select(x_l93,y_l93)
coord_gares<-function(lib_gare){
  stations_data%>%filter(libelle==lib_gare)%>%select(x_l93,y_l93)
  }
coord_gares("Lille-Flandres")


#corr

STATION_NAME <- "Lille-Flandres"

coords <- stations_data |>
  dplyr::select(lng = x_wgs84, lat = y_wgs84, libelle) |>
  dplyr::filter(libelle == STATION_NAME) |>
  dplyr::summarise(lat = dplyr::first(as.numeric(lat)), lng = dplyr::first(as.numeric(lng))) |>
  unlist(use.names = FALSE)

get_station_coordinates <- function(station, data, verbose = TRUE) {
  if (station != "Strasbourg-Ville") {
    coords <- data |>
      dplyr::select(lng = x_wgs84, lat = y_wgs84, libelle) |>
      dplyr::filter(libelle == station) |>
      dplyr::summarise(lat = dplyr::first(as.numeric(lat)), lng = dplyr::first(as.numeric(lng))) |>
      unlist(use.names = FALSE)
  } else {
    coords <- c(48.584488, 7.735626)
  }
  
  # Si verbose est TRUE, on affiche les coordonnées
  if (verbose) {
    cat(sprintf("%s -> (%f, %f)\n", station, coords[1], coords[2]))
  }
  
  return(coords)
}

tlse_coords <- get_station_coordinates("Toulouse-Matabiau", stations_data, verbose = TRUE)


#corr
get_routes_api_json <- function(coords1, coords2) {
  # On créé le JSON pour l'API de routage en se basant sur celui de la sous-partie "Interaction avec l'API de routage de TravelTime"
  request_body <- sprintf('{
    "locations": [
      {
        "id": "point-from",
        "coords": {
          "lat": %f,
          "lng": %f
        }
      },
      {
        "id": "point-to-1",
        "coords": {
          "lat": %f,
          "lng": %f
        }
      }
    ],
    "departure_searches": [
      {
        "id": "departure-search",
        "transportation": {
          "type": "public_transport",
          "walking_time": 900,
          "cycling_time_to_station": 100,
          "parking_time": 0,
          "boarding_time": 0,
          "driving_time_to_station": 1800,
          "pt_change_delay": 0,
          "disable_border_crossing": false
        },
        "departure_location_id": "point-from",
        "arrival_location_ids": [
          "point-to-1"
        ],
        "departure_time": "2024-06-26T18:00:00.000Z",
        "properties": [
          "travel_time",
          "route"
        ],
        "range": {
          "enabled": true,
          "max_results": 5,
          "width": 43200
        }
      }
    ]
  }', coords1[1], coords1[2], coords2[1], coords2[2])
  return(request_body)
}

#corr
get_travel_time_between_stations <- function(station1, station2, data, verbose = TRUE) {
  # Si les stations sont identiques aucun trajet nécessaire
  if (station1 == station2) {
    return(NA)
  }
  
  
  # Récupérer les coordonnées pour les deux stations
  coordinates <- lapply(c(station1, station2), get_station_coordinates, data = data, verbose = FALSE)
  
  # Générer le JSON pour l'API de routage
  request_body <- get_routes_api_json(coordinates[[1]], coordinates[[2]])
  
  # Interroger l'API de routage
  response <- get_travel_time_api_response(ROUTES_API_URL, request_body)
  
  # Gérer la limitation du taux d'API
  if (response[[2]] == 429) {
    if (verbose) cat("Trop de requêtes, attente d'une minute...\n")
    Sys.sleep(60)
    return(get_travel_time_between_stations(station1, station2, data, verbose))
  }
  
  # Vérifier l'existence d'un itinéraire valide
  if (length(response[[1]]$results[[1]]$locations) == 0) {
    travel_time <- Inf
  } else {
    # Extraire les données de temps de trajet et trouver le temps de trajet minimum en heures
    travel_times <- sapply(response[[1]]$results[[1]]$locations[[1]]$properties, function(item) item$travel_time)
    travel_time <- min(travel_times) / 3600
  }
  
  # Afficher le temps de trajet si verbose
  if (verbose) {
    message_text <- sprintf("%s -> %s : %s heures\n", station1, station2, ifelse(is.infinite(travel_time), "Aucun itinéraire trouvé", round(travel_time, 2)))
    cat(message_text)
  }
  
  return(travel_time)
}

get_travel_time_between_stations("Paris-Montparnasse","Toulouse-Matabiau",data=stations_data)

# Define the stations
STATIONS <- c("Paris-Nord", "Lyon-Perrache", "Marseille-St-Charles", "Toulouse-Matabiau", "Lille-Flandres", "Bordeaux-St-Jean", "Nice-Ville", "Nantes", "Strasbourg-Ville", "Montpellier-St-Roch")

# Initialisation de la matrice
time_matrix <- matrix(NA, nrow = length(STATIONS), ncol = length(STATIONS), dimnames = list(STATIONS, STATIONS))

# On remplit la matrice avec toutes les pairs possibles
combinations <- combn(STATIONS, 2, simplify = FALSE)
for (pair in combinations) {
  travel_time <- get_travel_time_between_stations(pair[1], pair[2], stations_data, verbose = FALSE)
  time_matrix[pair[1], pair[2]] <- round(travel_time, 2)
  time_matrix[pair[2], pair[1]] <- round(travel_time, 2)
}

THRESHOLD <- 4.5

# On garde seulement la matrice triangulaire inférieur (car on la supposé symmétrique)
lower_tri_matrix <- lower.tri(time_matrix)

# On extrait les indices où la condition n'est pas respectée
under_threshold_indices <- which(time_matrix < THRESHOLD & lower_tri_matrix, arr.ind = TRUE)

# On crée une liste de paires qui remplissent les conditions
under_threshold_routes <- mapply(function(i, j) c(STATIONS[i], STATIONS[j]),
                                 i = under_threshold_indices[, 1],
                                 j = under_threshold_indices[, 2],
                                 SIMPLIFY = FALSE,
                                 USE.NAMES = FALSE
)

# On définit l'URL des données
AIR_TRAFFIC_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/0c0a451e-983b-4f06-9627-b5ff1bccd2fc"

air_traffic_df <- read.csv2(AIR_TRAFFIC_DATA_URL)

PKT<-air_traffic_df%>%filter()
air_traffic_df$LSN_DEP_NOM <- lapply(air_traffic_df$LSN_DEP_NOM, sub, pattern = "\\-.*", replacement  = "")
air_traffic_df$LSN_ARR_NOM <- lapply(air_traffic_df$LSN_ARR_NOM, sub, pattern = "\\-.*", replacement  = "")


#corr

get_air_traffic_between_cities <- function(city1, city2, data) {
  # Calcul du trafic dans les deux sens city1 -> city2 et city2 -> city1
  total_traffic <- data |>
    dplyr::filter(
      (grepl(city1, LSN_DEP_NOM, ignore.case = TRUE) & grepl(city2, LSN_ARR_NOM, ignore.case = TRUE)) |
        (grepl(city2, LSN_DEP_NOM, ignore.case = TRUE) & grepl(city1, LSN_ARR_NOM, ignore.case = TRUE))
    ) |>
    dplyr::summarise(traffic = sum(LSN_DIST * LSN_PAX_loc)) |> # calcul du PKT
    dplyr::pull(traffic)
  
  return(as.numeric(total_traffic))
}

get_air_traffic_between_cities("Paris","Toulouse", air_traffic_df)

# Fonction pour extraire les noms des villes à partir des noms des gares
extract_city_name <- function(station) {
  sapply(station, function(x) strsplit(x, "-")[[1]][1])
}

# Extraire les paires de villes
city_pairs <- lapply(under_threshold_routes, extract_city_name)

under_threshold_air_traffic <- 0

# Calculer le trafic aérien pour chaque paire
for (pair in city_pairs) {
  air_traffic <- get_air_traffic_between_cities(pair[1], pair[2], air_traffic_df)
  under_threshold_air_traffic <- under_threshold_air_traffic + air_traffic
}

GCO2_PER_PKT <- 80

# On estime les émissions de CO2 en tCO2éq
cat(sprintf("En 2019, environ %.2f tCO2éq aurait pu être évités", under_threshold_air_traffic * GCO2_PER_PKT / 1000000))

#######carto

secrets <- yaml::read_yaml("secrets.yaml")
STADIA_MAPS_API_KEY <- secrets$`API Key`

STYLE <- "outdoors"

TILES_URL <- sprintf("https://tiles.stadiamaps.com/tiles/%s/{z}/{x}/{y}{r}.png?api_key=%s", STYLE, STADIA_MAPS_API_KEY)

GCO2_PER_PKT <- 80

emission_by_route_list <- list()

for (pair in city_pairs) {
  coordinates <- lapply(names(pair), get_station_coordinates, data = stations_data, verbose = FALSE)
  emissions <- get_air_traffic_between_cities(pair[1], pair[2], air_traffic_df) * GCO2_PER_PKT / 1000000
  
  # Ajouter les données à la liste
  emission_by_route_list[[length(emission_by_route_list) + 1]] <- list(
    city1 = pair[1],
    city2 = pair[2],
    lat1 = coordinates[[1]][1],
    lng1 = coordinates[[1]][2],
    lat2 = coordinates[[2]][1],
    lng2 = coordinates[[2]][2],
    emissions = emissions
  )
}

emission_by_route_df <- dplyr::bind_rows(emission_by_route_list)

map <- leaflet::leaflet() |>
  leaflet::addTiles(urlTemplate = TILES_URL)



# Parcourir chaque ligne du dataframe
for (i in 1:nrow(emission_by_route_df)) {
  # Définir les options par défaut pour les lignes
  lat_vector <- c(emission_by_route_df$lat1[i], emission_by_route_df$lat2[i])
  lng_vector <- c(emission_by_route_df$lng1[i], emission_by_route_df$lng2[i])
  color <- "black" # couleur par défaut
  opacity <- 0.5
  weight <- 1 # poids par défaut
  
  # Si les émissions sont supérieures à zéro, ajuster la couleur et le poids
  if (emission_by_route_df$emissions[i] > 0) {
    color <- "red"
    weight <- emission_by_route_df$emissions[i] / 10000
  }
  
  # Ajouter des lignes à la carte
  map <- map |>
    leaflet::addPolylines(lat = lat_vector, lng = lng_vector, color = color, opacity = opacity, weight = weight)
}

# Définir les options de label personnalisées
custom_label_options <- leaflet::labelOptions(noHide = TRUE, style = list("background" = "rgba(255, 255, 255, 0.5)"))

# Fonction pour ajouter des marqueurs circulaires
add_circle_marker <- function(map, lat, lng, city, label_options) {
  map |>
    leaflet::addCircleMarkers(
      lat = lat,
      lng = lng,
      radius = 5,
      color = "#4444AA",
      label = as.character(city),
      labelOptions = label_options
    )
}

# Boucle pour ajouter des marqueurs pour chaque ligne du dataframe
for (i in 1:nrow(emission_by_route_df)) {
  map <- add_circle_marker(map, emission_by_route_df$lat1[i], emission_by_route_df$lng1[i], emission_by_route_df$city1[i], custom_label_options)
  map <- add_circle_marker(map, emission_by_route_df$lat2[i], emission_by_route_df$lng2[i], emission_by_route_df$city2[i], custom_label_options)
}
