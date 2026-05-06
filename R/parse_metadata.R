parse_metadata <- function(metadata_filename) {
  metadata <-
    read.csv(metadata_filename) %>%
      tidyr::separate(SiteCode, c("Network", "Park", "Site")) %>%
      ##changing Rock Creek at Dumbarton Oaks to Downstream of Dumbarton Oaks
      dplyr::mutate(SiteName=case_when(SiteName=="Rock Creek at Dumbarton Oaks"~"Rock Creek Downstream of Dumbarton Oaks",
                                       TRUE~SiteName)) %>%
      ##creating column specifying karst v non-karst, cold vs warmwater, and nutrient ecoregion
      dplyr::mutate(karst=case_when(Park=="ANTI" | Park=="HAFE" | Park=="MONO" ~ "karst",
                                    Park!="ANTI" | Park!="HAFE" | Park!="MONO" ~ "non-karst")) %>%
      dplyr::mutate(nutrient_ecoregion=case_when(Park=="ANTI" | Park=="CATO" | Park=="HAFE" ~ "XI",
                                                 Park!="ANTI" | Park!="CATO" | Park!="HAFE" ~ "IX")) %>%
      dplyr::mutate(cold_water=case_when(Park=="CATO" ~ "coldwater",
                                         Park!="CATO" ~ "warmwater")) 
  return(metadata)
}