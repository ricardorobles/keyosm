# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'



osmkey <- function(capa,key,tipo){

  library(sf)
  library(tidyverse)
  library(dplyr)
  library(osmdata)
  library(zoo)
  library(rvest)
  library(stringr)



  # Categorías

  # A continuación se descarga la tabla de datos de openstreemap de acuerdo a la categoría. Para seleccionar la categoría correcta buscar en el siguiente link "https://wiki.openstreetmap.org/wiki/Map_features"

  link <- paste("https://wiki.openstreetmap.org/wiki/Key:",key,sep="")

  tablaOSM <- rvest::read_html(link) %>% rvest::html_node(css ="table.wikitable") %>% rvest::html_table()
  head(tablaOSM)

  tablaOSM[tablaOSM == ''] <- NA

  tablaOSM_corregido <- tablaOSM %>% select(Key,Element,Value) %>% zoo::na.locf() %>% filter(Key==key)

  dato <- paste0("\"",tablaOSM_corregido$Key,"\"=\"",tablaOSM_corregido$Value,"\"",sep="")
  dato

  # Cargo capa

  bbox_base <- sf::st_bbox(st_read(capa) %>% sf::st_transform(4326))

  sector <- osmdata::opq(bbox=bbox_base, timeout = 300)

  # Descarga de datos

  sector_osm <- osmdata::add_osm_features(opq=sector, features =  dato)

  sector_datos <- osmdata::osmdata_sf(q=sector_osm)

  tipo_f <- paste("osm_",tipo,sep="")

  sector_tipo <- sector_datos[str_detect(names(sector_datos), tipo_f)]

  sector_final <- do.call(rbind.data.frame, sector_tipo)
}
