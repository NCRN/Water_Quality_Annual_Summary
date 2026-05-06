library(rmarkdown)

RenderSummary <- function(Code, File, Year, Metadata_filename, Data_filename, output) {
  outputs <- c('pdf', 'html')
  if (output %in% outputs==F){
    stop(paste0("You provided `output`='",output,"'. `output` must be one of the following: ", paste(outputs, collapse=', ')))
  }
  
  if (output == 'pdf'){
    template = file.path('templates',"ncrn_water_quality_pdf.Rmd")
  } else if (output == 'html') {
    template =  file.path('templates',"ncrn_water_quality_html.Rmd")
  }
  
  rmarkdown::render(
    input=template
    ,params=list(
      Park=strsplit(Code, "_")[[1]][1]
      ,Site=strsplit(Code, "_")[[1]][2]
      ,Year=Year
      ,Metadata=Metadata_filename
      ,Data=Data_filename
    )
    ,output_file = File
  )
  
}
