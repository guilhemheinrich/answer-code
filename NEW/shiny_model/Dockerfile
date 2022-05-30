FROM openanalytics/r-base

LABEL maintainer "Dayma Attoumani <dayma.attoumani@inra.fr>"

ARG app_name=app
ENV APPNAME=${app_name}

ENV R_LIBS=https://cloud.r-project.org/

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0	\ 
    libxml2-dev \
    libxslt-dev

# basic shiny functionality
RUN R -e "install.packages(c('remotes','devtools','shiny','DT','glue','grDevices','EnvStats','knitr','dplyr','kableExtra','formattable','tidyr','plotly','scales','changepoint','forecast','TTR','stringr', 'shinycssloaders','caret','RColorBrewer','shinydashboard'), repos='https://cloud.r-project.org')"
RUN R -e "remotes::install_github('OpenSILEX/phisWSClientR', build_vignettes=TRUE, ref='2.3.0')"
RUN R -e "remotes::install_github('pnnl/Smisc')"

# copy the app to the image
# RUN mkdir /tmp/app
COPY ${APPNAME} /tmp/app

RUN echo "local({ options(shiny.port = 3838, shiny.host = '0.0.0.0') })" >> /usr/lib/R/etc/Rprofile.site
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(appDir = 'tmp/app')"]
