#Indicadores de sistemas de transporte usando tidytransit

   ### Éste código fue escrito en base a las siguientes fuentes: https://github.com/r-transit/tidytransit, https://r-transit.github.io/tidytransit/ 

rm(list=ls())

##Librerias
devtools::install_github("r-transit/tidytransit")
library(tidytransit)
library(sf)
library(tidyverse)
library(dplyr)
library(hms)

##Archivos GTFS
setwd("D:/~")
gtfs_2019 <- read_gtfs("gtfs-cordoba.zip")
gtfs_2021 <- read_gtfs("cordoba.gtfs.zip")
gtfs_2022 <- read_gtfs("GTFS_Transporte_Público_7-22.zip")
gtfs_2023 <- read_gtfs("cordoba.gtfs_3.zip")

gtfs_2019 <- set_servicepattern(gtfs_2019)
gtfs_2021 <- set_servicepattern(gtfs_2021)
gtfs_2022 <- set_servicepattern(gtfs_2022)
gtfs_2023 <- set_servicepattern(gtfs_2023)

##  Longitud de las rutas
###2019
gtfs_2019 <- gtfs_as_sf(gtfs_2019)
gtfs_2019$shapes$length <- st_length(gtfs_2019$shapes)

shape_lengths_2019 <- gtfs_2019$shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)
###2021
gtfs_2021 <- gtfs_as_sf(gtfs_2021)
gtfs_2021$shapes$length <- st_length(gtfs_2021$shapes)

shape_lengths_2021 <- gtfs_2021$shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)
###2022
gtfs_2022 <- gtfs_as_sf(gtfs_2022)
gtfs_2022$shapes$length <- st_length(gtfs_2022$shapes)

shape_lengths_2022 <- gtfs_2022$shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)
###2023
gtfs_2023 <- gtfs_as_sf(gtfs_2023)
gtfs_2023$shapes$length <- st_length(gtfs_2023$shapes)

shape_lengths_2023 <- gtfs_2023$shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)

##  Resumen
###2019
service_pattern_summary_2019 <- gtfs_2019$trips %>%
  left_join(gtfs_2019$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths_2019, by="shape_id") %>%
  left_join(gtfs_2019$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))
service_pattern_summary_2019 <- gtfs_2019$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  left_join(service_pattern_summary_2019, by="servicepattern_id")

knitr::kable(service_pattern_summary_2019)

###2021
service_pattern_summary_2021 <- gtfs_2021$trips %>%
  left_join(gtfs_2021$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths_2021, by="shape_id") %>%
  left_join(gtfs_2021$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))
service_pattern_summary_2021 <- gtfs_2021$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  left_join(service_pattern_summary_2021, by="servicepattern_id")

knitr::kable(service_pattern_summary_2021)

###2022
service_pattern_summary_2022 <- gtfs_2022$trips %>%
  left_join(gtfs_2022$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths_2022, by="shape_id") %>%
  left_join(gtfs_2022$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))
service_pattern_summary_2022 <- gtfs_2022$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  left_join(service_pattern_summary_2022, by="servicepattern_id")

knitr::kable(service_pattern_summary_2022)

###2023
service_pattern_summary_2023 <- gtfs_2023$trips %>%
  left_join(gtfs_2023$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths_2023, by="shape_id") %>%
  left_join(gtfs_2023$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))
service_pattern_summary_2023 <- gtfs_2023$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  left_join(service_pattern_summary_2023, by="servicepattern_id")

knitr::kable(service_pattern_summary_2023)

##  service_id
###2019
service_ids_2019 <- gtfs_2019$.$servicepattern %>% 
  filter(servicepattern_id == 's_6e8fcad') %>% # servicio con mayor cantidad de dias en servicio y viajes, service_id=1, 16, 17, 18, 19, 20, 21, 22, 23, 24
  pull(service_id)
gtfs_2019$trips %>%
  filter(service_id %in% service_ids_2019) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n()) %>% 
  head(n = 10L) %>%
  knitr::kable()
###2021
service_ids_2021 <- gtfs_2021$.$servicepattern %>% 
  filter(servicepattern_id == 's_3a41b96') %>% # servicio con mayor cantidad de dias en servicio y viajes, service_id=1
  pull(service_id)
gtfs_2021$trips %>%
  filter(service_id %in% service_ids_2021) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n()) %>% 
  head(n = 10L) %>%
  knitr::kable()
###2022
service_ids_2022 <- gtfs_2022$.$servicepattern %>% 
  filter(servicepattern_id == 's_f1eafad') %>% # servicio con mayor cantidad de dias en servicio y viajes, service_id=1
  pull(service_id)
gtfs_2022$trips %>%
  filter(service_id %in% service_ids_2022) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n()) %>% 
  head(n = 10L) %>%
  knitr::kable()
###2023
service_ids_2023 <- gtfs_2023$.$servicepattern %>% 
  filter(servicepattern_id == 's_c87e317') %>% # servicio con mayor cantidad de dias en servicio y viajes, service_id=1
  pull(service_id)
gtfs_2023$trips %>%
  filter(service_id %in% service_ids_2023) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n()) %>% 
  head(n = 10L) %>%
  knitr::kable()
class(gtfs_2019$frequencies$start_time)

##  Intervalos
###2019
gtfs_2019$frequencies %>%
  subset(as.character(start_time) >= "06:00:00" & as.character(end_time) <= "20:00:00") %>%  # servicios entre las 6 de la mañana y las 8 de la noche, parámetro modificable
  summarise(mean=mean(headway_secs/60), min=min(headway_secs/60), max=max(headway_secs/60))
  ### El GTFS 2019 es formato frequency-based, por lo que tiene una tabla de frecuencias
n_distinct(gtfs_2019$stops) 
routes_sf_2019 <- get_route_geometry(gtfs_2019, service_ids = service_ids_2019)
routes_sf_2019 %>%
  mutate(length_m = st_length(routes_sf_2019)) %>%
  summarise(sum_km = sum(length_m)/1000, n=n_distinct(route_id)) # N° paradas y extensión de la red

###2021
stop_freq_2021 <- get_stop_frequency(gtfs_2021, start_time = "06:00:00", end_time = "20:00:00", 
                                     service_ids = service_ids_2021, by_route = TRUE) # servicios entre las 6 de la mañana y las 8 de la noche, parámetro modificable
stop_freq_2021 <- stop_freq_2021 %>%
  mutate(mean_headway=mean_headway/60)
stop_freq_2021 %>%
  summarise(mean=mean(mean_headway), n=n_distinct(stop_id), min=min(mean_headway), max=max(mean_headway)) # Valores de intervalos promedio, mínimo y máximo
n_distinct(gtfs_2021$stops)
routes_sf_2021 <- get_route_geometry(gtfs_2021, service_ids = service_ids_2021)
routes_sf_2021 %>%
  mutate(length_m = st_length(routes_sf_2021)) %>%
  summarise(sum_km = sum(length_m)/1000, n=n_distinct(route_id)) # N° paradas y extensión de la red
###2022
stop_freq_2022 <- get_stop_frequency(gtfs_2022, start_time = "06:00:00", end_time = "20:00:00", 
                                     service_ids = service_ids_2022, by_route = TRUE) # servicios entre las 6 de la mañana y las 8 de la noche, parámetro modificable
stop_freq_2022 <- stop_freq_2022 %>%
  mutate(mean_headway=mean_headway/60)
stop_freq_2022 %>%
  summarise(mean=mean(mean_headway), n=n_distinct(stop_id), min=min(mean_headway), max=max(mean_headway)) # Valores de intervalos promedio, mínimo y máximo
n_distinct(gtfs_2022$stops)
routes_sf_2022 <- get_route_geometry(gtfs_2022, service_ids = service_ids_2022)
routes_sf_2022 %>%
  mutate(length_m = st_length(routes_sf_2022)) %>%
  summarise(sum_km = sum(length_m)/1000, n=n_distinct(route_id)) # N° paradas y extensión de la red
###2023
stop_freq_2023 <- get_stop_frequency(gtfs_2023, start_time = "06:00:00", end_time = "20:00:00", 
                                     service_ids = service_ids_2023, by_route = TRUE) # servicios entre las 6 de la mañana y las 8 de la noche, parámetro modificable
stop_freq_2023 <- stop_freq_2023 %>%
  mutate(mean_headway=mean_headway/60)
stop_freq_2023 %>%
  summarise(mean=mean(mean_headway), n=n_distinct(stop_id), min=min(mean_headway), max=max(mean_headway)) # Valores de intervalos promedio, mínimo y máximo
n_distinct(gtfs_2023$stops)
routes_sf_2023 <- get_route_geometry(gtfs_2023, service_ids = service_ids_2023)
routes_sf_2023 %>%
  mutate(length_m = st_length(routes_sf_2023)) %>%
  summarise(sum_km = sum(length_m)/1000, n=n_distinct(route_id)) # N° paradas y extensión de la red

rm(list=ls())