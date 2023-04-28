FROM rocker/r-ver:4.1.0

# set cran date
ARG WHEN

# install git
RUN apt-get update
RUN apt-get install -y git

# set default library path
ENV R_LIBS_USER="/usr/local/lib/R/site-library"

# install libxml2-dev and rgcam pkg
RUN apt-get install libxml2-dev -y
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('JGCRI/rgcam')"

# install pkgs
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('markdown')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('Rcpp')"
RUN R -e "install.packages('stringi')"
RUN R -e "install.packages('rlang')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('gtable')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('gridExtra')"
RUN R -e "install.packages('magrittr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('here')"
RUN R -e "install.packages('extrafont')"
RUN R -e "install.packages('RColorBrewer')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyTree')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('rrapply')"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('xml2')"
RUN R -e "devtools::install_github('JorisChau/rrapply')"

# clone repo
RUN git clone https://github.com/bc3LC/gcamreport.git /root/gcamreport

# launch R
CMD ["R"]