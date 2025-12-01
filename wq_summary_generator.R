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