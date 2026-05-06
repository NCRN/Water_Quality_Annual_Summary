pkgs <- c(
  'rmarkdown'
  ,'ggplot2'
  ,'dplyr'
  ,'purrr'
  ,'lubridate'
  ,'shiny'
  ,'tidyr'
  ,'Rmoji'
)


renv::install(pkgs, prompt=F, rebuild=T)
renv::install('Rmoji', prompt=F, rebuild=T)

