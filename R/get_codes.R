# given a csv of NCRNWater metadata, return site codes in the form PARK_SITE (e.g., 'NACE_STCK' or 'ROCR_BAKE')

library(dplyr)
library(tidyr)

get_codes <- function(metadata_filename) {
  
  metadata <- read.csv(metadata_filename)
  
  metadata <- metadata %>%
    filter(IsActive=="True") %>%
    tidyr::separate(SiteCode, c("Network", "Park", "Site")) %>%
    dplyr::mutate(code=paste(Park, Site, sep="_"))
  
  codes <- metadata$code %>% unique
  
  return(codes)
  
}
