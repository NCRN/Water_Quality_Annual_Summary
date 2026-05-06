library(rmarkdown)
library(purrr)
library(dplyr)
source('R/render.R')
source('R/get_codes.R')
source('R/congruency.R')

# GLOBAL CONSTANTS
YEAR<-2025
METADATA <- "wqp_ncrnwater_metadata.csv"
DATA <- "wqp.csv"

# before running the program,
# double-check that the data and metadata files match each other
congruency(DATA, METADATA)

codes <- get_codes(METADATA)

##PDF versions
dirname <- paste0('pdfs_',YEAR)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}

OutFiles<-file.path(dirname ,paste0(codes,"_",YEAR,"_","summary.pdf"))

walk2(.x=codes, .y=OutFiles, .f=RenderSummary, Year=YEAR, Metadata_filename=METADATA, Data_filename=DATA, output='pdf')


##HTML versions
dirname <- paste0('htmls_',YEAR)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}

OutFiles<-file.path(dirname ,paste0(codes,"_",YEAR,"_","summary.html"))

walk2(.x=codes, .y=OutFiles, .f=RenderSummary, Year=YEAR, Metadata_filename=METADATA, Data_filename=DATA, output='html')
