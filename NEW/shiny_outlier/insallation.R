library(remotes)

# Si il y a un problème avec tar: https://github.com/r-dbi/RPostgres/issues/110
Sys.setenv(TAR = "/bin/tar")
remotes::install_github('OpenSILEX/opensilexClientToolsR', build_vignettes=FALSE, ref='1.0.0')

# Set broser to opera
options(browser = "firefox")
shiny::runApp('app')