library(rmarkdown)
library(purrr)
library(dplyr)

Year<-2025

metadata <- read.csv("wqp_ncrnwater_metadata.csv")
metadata <- metadata %>%
  filter(IsActive=="True") %>%
  tidyr::separate(SiteCode, c("Network", "Park", "Site")) %>%
  dplyr::mutate(code=paste(Park, Site, sep="_"))

codes <- unique(metadata$code)

##PDF versions
dirname <- paste0('pdfs_',Year)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}
OutFiles_pdf<-file.path(dirname ,paste0(codes,"_",Year,"_","summary.pdf"))

RenderSummary<-function(Code, File, Year){ 
  render(input="wq_markdown_2.Rmd", params=list(Park=strsplit(Code, "_")[[1]][1], 
                                                Site=strsplit(Code, "_")[[1]][2], 
                                                Year=Year), output_file = File)
}

walk2(.x=codes, .y=OutFiles_pdf, .f=RenderSummary, Year=Year)


##HTML versions
dirname <- paste0('htmls_',Year)
if (dir.exists(dirname)==F){
  dir.create(dirname)
}

OutFiles_html<-file.path(dirname ,paste0(codes,"_",Year,"_","summary.html"))

RenderSummary_html<-function(Code, File, Year){ 
  render(input="wq_markdown_html.Rmd", params=list(Park=strsplit(Code, "_")[[1]][1], 
                                                Site=strsplit(Code, "_")[[1]][2], 
                                                Year=Year), output_file = File)
}


walk2(.x=codes, .y=OutFiles_html, .f=RenderSummary_html, Year=Year)
