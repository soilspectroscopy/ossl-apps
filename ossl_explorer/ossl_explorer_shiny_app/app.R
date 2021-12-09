#
# This is the server logic of a Shiny web application. 
#

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(shinyjqui)
library(shinyWidgets)
library(shinyjs)
library(s2)
library(magrittr)
library(shinycssloaders)
library(jsonlite)
library(geojsonio)
library(plotly)
library(ggplot2)
#library(DT)
library(kableExtra)
library(shinyBS)
library(nominatim)
library(leafgl)
library(shinybusy)
# remotes::install_github("bhaskarvk/leaflet.extras", ref = remotes::github_pull("184")) 
# remotes::install_github("bhaskarvk/leaflet.extras")
# devtools::install_github("hrbrmstr/nominatim")
# install.packages("leafgl") # https://github.com/r-spatial/leafgl
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Data
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
mapQuestKey <- "???" # GET FROM MAPQUEST
# dirName = dirname(rstudioapi::getActiveDocumentContext()$path)
dirName = "/srv/shiny-server"
setwd(dirName)

soilsite.data = readRDS(file = "data/ossl_soilsite_v1.rds")
# soilsite.data = readRDS(url("http://s3.us-east-1.wasabisys.com/soilspectroscopy/ossl_import/ossl_soilsite_v1.rds", "rb"))
#soilsite.data$"latitude_wgs84_dd" = -soilsite.data$"latitude_wgs84_dd"

# soilsite.data = soilsite.data[!duplicated(soilsite.data), ]

# soilsite.data <- soilsite.data[1:10000, ]
# unique(soilsite.data$dataset.title_utf8_txt)
soilsite.data[soilsite.data$dataset.title_utf8_txt == "Soil physical and chemical properties, Megapit, RELEASE-2021 (DP1.00096.001)", ]$dataset.title_utf8_txt <- "NEON Megapit Soil Archive"

soillab.data = readRDS(file = "data/ossl_soillab_v1.rds")
# soillab.data = readRDS(url("http://s3.us-east-1.wasabisys.com/soilspectroscopy/ossl_import/ossl_soillab_v1.rds", "rb"))
# soillab.data = soillab.data[!duplicated(soillab.data), ]

soillabVar <- names(soillab.data %>% select_if(~!all(is.na(.))) )
soillab.df <- data_frame(name_soil = soillabVar[8:57]) %>%
  tidyr::separate(name_soil, c("chem", "string", "units"), "_") %>%
  dplyr::mutate(full_name = soillabVar[8:57])


mir.data = readRDS(file = "data/ossl_mir_v1.rds")
# summary(mir.data$scan.date.begin_iso.8601_yyyy.mm.dd)
mir.data$scan.date.begin_iso.8601_yyyy.mm.dd = as.character(mir.data$scan.date.begin_iso.8601_yyyy.mm.dd)
mir.data$scan.date.end_iso.8601_yyyy.mm.dd = as.character(mir.data$scan.date.end_iso.8601_yyyy.mm.dd)
# mir.data = readRDS(url("http://s3.us-east-1.wasabisys.com/soilspectroscopy/ossl_import/ossl_mir_v1.rds", "rb"))
# mir.data = mir.data[!duplicated(mir.data), ]
mirVar<- names(mir.data)

visnir.data = readRDS(file = "data/ossl_visnir_v1.rds")
# ima neki problem sa kolonama sa Date formatom
visnir.data$scan.date.begin_iso.8601_yyyy.mm.dd = as.character(visnir.data$scan.date.begin_iso.8601_yyyy.mm.dd)
visnir.data$scan.date.end_iso.8601_yyyy.mm.dd = as.character(visnir.data$scan.date.end_iso.8601_yyyy.mm.dd)
# visnir.data = readRDS(url("http://s3.us-east-1.wasabisys.com/soilspectroscopy/ossl_import/ossl_visnir_v1.rds", "rb"))
# visnir.data = visnir.data[!duplicated(visnir.data), ]
visnirVar <- names(visnir.data)

id.mir <- mir.data$id.layer_local_c
id.vnir <- visnir.data$id.layer_local_c

soilsite.data %<>% dplyr::mutate(id_mir = case_when(id.layer_local_c %in% id.mir ~ TRUE, 
                                                    !(id.layer_local_c %in% id.mir) ~ FALSE),
                                 id_vis = case_when(id.layer_local_c %in% id.vnir ~ TRUE, 
                                                    !(id.layer_local_c %in% id.vnir) ~ FALSE))
# soilsite.data1 <- soilsite.data %>% distinct(longitude_wgs84_dd, latitude_wgs84_dd)
soilsite.data_unique <- soilsite.data[!duplicated(soilsite.data[, c("longitude_wgs84_dd", "latitude_wgs84_dd")]), ]

# sp_bounds_0 <- st_read(dsn = "data/spatial_bounds_level_0_1_simplified.gpkg", layer = "spatial_bounds_level_0_simplified")
# sp_bounds_0 <- st_read(dsn = "data/spatial_bounds_level_0_1_simplified.gpkg", layer = "0_1_buffer_final")
sp_bounds_0 <- st_read(dsn = "data/spatial_bounds_level_0_1_simplified.gpkg", layer = "0_1_buffer") # oko originalnih granica
sp_bounds_1 <- st_read(dsn = "data/spatial_bounds_level_0_1_simplified.gpkg", layer = "spatial_bounds_level_1")

# # USA data
# sp_bounds_0 <- st_read(dsn = "data/USA/spatial_bounds_level_0_1.gpkg", layer = "spatial_bounds_level_0_simplified")
# sp_bounds_1 <- st_read(dsn = "data/USA/spatial_bounds_level_0_1.gpkg", layer = "spatial_bounds_level_1")

sf_dat <- st_as_sf(x = soilsite.data[!is.na(soilsite.data[, "longitude_wgs84_dd"]), ],
                   coords = c("longitude_wgs84_dd" , "latitude_wgs84_dd"),
                   crs = '+proj=longlat +datum=WGS84')

sf_dat_unique <- st_as_sf(x = soilsite.data_unique[!is.na(soilsite.data_unique[, "longitude_wgs84_dd"]), ],
                          coords = c("longitude_wgs84_dd" , "latitude_wgs84_dd"),
                          crs = '+proj=longlat +datum=WGS84')

# sf_dat <- sf_dat_unique
#library(rmapshaper)

#sp_bounds_0 <- ms_simplify(sp_bounds_0,
#                           keep = 0.05,
#                           keep_shapes = T)

#st_write(sp_bounds_0, dsn = "data/spatial_bounds_level_0_1_.gpkg", layer = "spatial_bounds_level_0_simplified")


# Point layer - labs

sf_labs_f <- st_read(dsn = "data/Registered Laboratories in GLOSOLAN.kml", layer = "Further registered laboratories in GLOSOLAN") %>% st_zm(drop = TRUE)
sf_labs_n <- st_read(dsn = "data/Registered Laboratories in GLOSOLAN.kml", layer = "National Reference Soil Laboratories in GLOSOLAN") %>% st_zm(drop = TRUE)

sf_labs_f$popup <- paste("<b>", toupper(sf_labs_f$Name), "</b><br><br><b>Name:</b> ", sf_labs_f$Laboratory_Name, "<br><br>",
                         "<b>Address:</b> ", sf_labs_f$Laboratory_Address, sep="")
sf_labs_n$popup <- paste("<b>", toupper(sf_labs_n$Name), "</b><br><br><b>Name:</b> ", sf_labs_n$Laboratory_Name, "<br><br>",
                         "<b>Address:</b> ", sf_labs_n$Laboratory_Address_, sep="")

sf_use_s2(FALSE)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# runApp()
# runApp(ui=ui, server=server)

source("ui.R")
source("server.R")
runApp(host="0.0.0.0", port=3838)







