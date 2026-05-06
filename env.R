# steps to reproduce the R environment that makes the program run
# it's a good idea to make your working directory for this project in your C: drive, not in OneDrive
renv::activate()
pkgs <- c(
  'rmarkdown'
  ,'ggplot2'
  ,'dplyr'
  ,'purrr'
  ,'lubridate'
  ,'shiny'
  ,'tidyr'
  ,'Rmoji'
  ,'tinytex'
  ,'remotes'
)
renv::install(pkgs, prompt=F, rebuild=T)

library(tinytex)
tinytex::install_tinytex()

options(download.file.method = "wininet")
remotes::install_github('https://github.com/nationalparkservice/npsutils')
