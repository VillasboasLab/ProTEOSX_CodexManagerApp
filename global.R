library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinybusy)
library(tidyverse)
library(plotly)
library(DT)
library(RMariaDB)
library(officer)
library(RCurl)
library(png)
library(RColorBrewer)
### for resizing PPT images
library(magick)

# increase max upload file size
options(shiny.maxRequestSize = 10*1024^2)
# increase the ppt upload size to accept huge files
options(shiny.maxRequestSize=300*1024^2) # ~300MB max

### This lib was previously in CRAN, but for now, must be installed by dev.
if("dqshiny" %in% rownames(installed.packages()) == FALSE) {remotes::install_github("daqana/dqshiny")}
library(dqshiny)

page_files <- c("Views/home.R","Views/data.R","Views/entry.R", "Views/modularize_modals.R")
purrr::walk(page_files, source, local = environment())

#########################################
###          Global Constants         ###
#########################################
mariapw <- Sys.getenv('MARIADB_JCVB_PW')
mariaus <- Sys.getenv('MARIADB_JCVB_USER')
mariaurl <- Sys.getenv('MARIADB_HOST')

relavent_input_columns <- c("Thickness","Section_to_image_interval","H2O2_bleached","Date_Rendered","Date_Processed","Specimen_Origin","Source","Tissue_Type",
                            "Diagnosis","antigen_retrival","Stage","Z_stacks","Tile_Size")


########################################
###       Database Functions         ###
########################################
if(exists("mariapw") && mariapw != ""){
  if(dbCanConnect(drv=RMariaDB::MariaDB(), dbname="VillasboasLabCodex", username=mariaus, host=mariaurl, password=mariapw) ){
    print("Connecting to MariaDB Codex Tracking...")
    fCon <- dbConnect(drv=RMariaDB::MariaDB(), dbname="VillasboasLabCodex", username=mariaus, host=mariaurl, password=mariapw)
  } else {
    print("ERROR IN NETWORK CONNECTION!")
  }
} else {
  print("ERROR: NO DATABASE PARAMETERS EXIST!")
}
source("Db/helper_database.R")
source("Db/helper_ppt_summary.R")

