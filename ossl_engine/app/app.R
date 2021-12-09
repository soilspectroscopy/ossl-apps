# Sys.setenv(AUTH0_USER = "dev-jr8e0409.us",
#            AUTH0_KEY = "LKN6Uzj2gTJQWSzAp0GdrVB57W8beQVv",
#            AUTH0_SECRET = "???") # GET FROM AUTH0

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# https://curso-r.github.io/auth0/#step-5-run
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# https://curso-r.github.io/auth0/
# https://stackoverflow.com/questions/68776860/how-to-validate-an-auth0-token-in-r-shiny-and-handle-errors

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyjqui)
library(shinyWidgets)
library(shinyjs)
library(magrittr)
library(shinycssloaders)
library(jsonlite)
library(plotly)
library(ggplot2)
library(DT)
library(kableExtra)
library(shinyBS)
library(yaml)
library(data.table)
require(pls)
library(randomForest)
library(tidyr)
library(auth0)
library(readxl)
library(shinybusy)
# install.packages('xgboost')
library(xgboost)
# install.packages('Cubist')
library(Cubist)
library(terra)
library(sp)
library(httr)
library(prospectr)
library(fastDummies)
library(cowplot)
library(matrixStats)
library(mlr)
library(parallelMap)
library(plotKML)
library(rworldmap)
# install.packages(c("openair", "yardstick"))

dirName = "/srv/shiny-server"
# dirName = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(dirName)
source("functions/ml.R")

# read csv
soil_model_df <- read.csv(file = "xlsx/SoilSpec4GG internal tables - OSSL_models_meta.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  as.data.frame()

# names(soil_model_df)

# read available instruments
inst <- readxl::read_xlsx(path = "xlsx/Instruments.xlsx", col_names = TRUE) %>% as.data.frame()

# ossl_models, database, table
data.dir = "/data/soilspectroscopy/"

a0_info <- auth0::auth0_info()

source("ui.R")
source("server.R")
# options(shiny.port = 8080)
options(shiny.maxRequestSize=20*1024^2)
# options(auth0_local = TRUE)
runApp(host="0.0.0.0", port=3838)
# runApp(host="0.0.0.0", port=8080)
