library(rmarkdown)
library(purrr)
library(dplyr)

Year<-2024

metadata <- read.csv("wqp_ncrnwater_metadata.csv")
metadata <- metadata %>%
  filter(IsActive=="True") %>%
  tidyr::separate(SiteCode, c("Network", "Park", "Site")) %>%
  dplyr::mutate(code=paste(Park, Site, sep="_"))

codes <- list(unique(metadata$code))

OutFiles<-paste0(codes,"-",Year,"-","summary.pdf")

RenderSummary<-function(Code, File, Year){ 
  render(input="wq_markdown_2.Rmd", params=list(Park=strsplit(Code, "_")[[1]][1], 
                                                Site=strsplit(Code, "_")[[1]][2], 
                                                Year=Year), output_file = File)
}

pwalk(list(c(codes, OutFiles, Year)), .f=RenderSummary)

# walk2(.x=codes, .y=OutFiles, Year=Year, .f=RenderSummary)


#ParksCodes<-c("ANTI","CATO", "GWMP","HAFE","MANA","MONO","NACE","PRWI","ROCR","WOTR")

#SitesCodes <- c()

# RenderIAR<-function(Park, Site, Year, File){ 
#   render(input='wq_markdown_2.Rmd',params=list(Park=Park, Site=Site, Year=Year), output_file = File)
# }

#walk2(.x=codes, .y=OutFiles, .f=RenderIAR, Year=Year)