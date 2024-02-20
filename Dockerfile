FROM rocker/tidyverse:4.3.2
#FROM openanalytics/r-base
RUN apt-get -y update
RUN apt-get -y install libcurl4-openssl-dev
RUN apt-get -y install libssl-dev
RUN apt-get -y install cmake
RUN apt-get -y install libxml2-dev
RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("BiocManager")'
RUN R -e 'BiocManager::install("ComplexHeatmap")'
RUN R -e 'BiocManager::install("InteractiveComplexHeatmap")'
RUN R -e 'remotes::install_cran("Rlabkey")'
RUN R -e 'remotes::install_cran("circlize")'
RUN R -e 'remotes::install_cran("colourpicker")'
RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("ggplot2")'
RUN R -e 'remotes::install_cran("ggrepel")'
RUN R -e 'remotes::install_cran("rlang")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("shinyWidgets")'
RUN R -e 'remotes::install_cran("shinydashboard")'
RUN R -e 'remotes::install_cran("shinyjs")'
RUN R -e 'remotes::install_cran("golem")'
RUN R -e 'remotes::install_cran("scales")'
RUN R -e 'remotes::install_cran("DT")'
COPY Omics_*.tar.gz /app.tar.gz

RUN R -e 'remotes::install_local("/app.tar.gz")'

COPY Rprofile.site /usr/lib/R/etc/
# set host and port
EXPOSE 3838
#CMD  ["R", "-e","options('shiny.port'=3838,shiny.host='0.0.0.0'); vici::run_app()"]
CMD ["R", "-e", "Omics::run_app()"]
