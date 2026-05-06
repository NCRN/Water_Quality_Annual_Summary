library(rmarkdown)
library(purrr)
library(dplyr)
library(crayon)
source('render.R')
source('R/get_codes.R')
source('R/congruency.R')

# GLOBAL CONSTANTS
YEAR<-2025
METADATA <- "data/wqp_ncrnwater_metadata.csv"
DATA <- "data/wqp.csv"

# before running the program,
# double-check that the data and metadata files match each other
check_congruency(DATA, METADATA)

codes <- get_codes(METADATA)

##HTML versions
dirname <- paste0('htmls_',YEAR)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}
OutFiles<-file.path(dirname ,paste0(codes,"_",YEAR,"_","summary.html"))
walk2(.x=codes, .y=OutFiles, .f=RenderSummary, Year=YEAR, Metadata_filename=file.path(METADATA), Data_filename=file.path(DATA), output='html')


##PDF versions
dirname <- paste0('pdfs_',YEAR)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}
OutFiles<-file.path(dirname ,paste0(codes,"_",YEAR,"_","summary.pdf"))
walk2(.x=codes, .y=OutFiles, .f=RenderSummary, Year=YEAR, Metadata_filename=METADATA, Data_filename=DATA, output='pdf')


