#! /home/heinrich/miniconda3/envs/r_answer/bin/Rscript

# Set browser to opera
# options(browser = "firefox")

options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)
shiny::runApp('app')