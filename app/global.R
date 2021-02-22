library(shiny)
library(magrittr)
library(shinyWidgets)
library(promises)
library(future)
library(shinycssloaders)

plan(multisession)

source("../R/plot_functions.R")
source("../R/table_functions.R")