library(rmarkdown)
library(purrr)
library(dplyr)
library(crayon)
library(NPSutils)
source('render.R')
source('R/get_codes.R')
source('R/congruency.R')

# if the data package is public:
# data_package_reference <- 2317661 # https://irma.nps.gov/DataStore/Reference/Profile/2317661
# mydata <- NPSutils::get_data_package(data_package_reference) # downloads from datastore into `.data/{myref}`
# pdf_reference <- 2316392
# mydata <- NPSutils::load_data_packages(myref) # reads data from `.data/{myref}`
# METADATA <- file.path('data', data_package_reference, "wqp_ncrnwater_metadata.csv")
# DATA <- file.path('data', data_package_reference, "wqp.csv")

# GLOBAL CONSTANTS
YEAR<-2025
METADATA <- file.path('data',"wqp_ncrnwater_metadata.csv")
DATA <- file.path('data', "wqp.csv")

# before running the program,
# double-check that the data and metadata files match each other
check_congruency(DATA, METADATA)

codes <- get_codes(METADATA)

##HTML versions
dirname <- paste0('htmls_',YEAR)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}
OutFiles<-file.path(paste0('../',dirname) ,paste0(codes,"_",YEAR,"_","summary.html"))
purrr::walk2(.x=codes, .y=OutFiles, .f=RenderSummary, Year=YEAR, Metadata_filename=METADATA, Data_filename=DATA, output='html')


##PDF versions
dirname <- paste0('pdfs_',YEAR)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}
OutFiles<-file.path(paste0('../',dirname) ,paste0(codes,"_",YEAR,"_","summary.pdf"))
walk2(.x=codes, .y=OutFiles, .f=RenderSummary, Year=YEAR, Metadata_filename=METADATA, Data_filename=DATA, output='pdf')


