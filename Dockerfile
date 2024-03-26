FROM rocker/r-base:4.1.0

ENV R_LIBS_USER="/usr/local/lib/R/site-library"
RUN chmod a+w /usr/local/lib/R/site-library

# Install required system libraries
RUN apt-get update \
  && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    git

# install pkgs
RUN apt-get update
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('JGCRI/rgcam')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('markdown')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('rlang')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('magrittr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tibble')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('here')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyTree')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('rrapply')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('xml2')"
RUN R -e "install.packages('writexl')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('rrapply', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('usethis')"
RUN R -e "remotes::install_github('JGCRI/rpackageutils')"

# clone repo
RUN apt-get install -y git
RUN git clone -b gcam-v7.0 https://github.com/bc3LC/gcamreport.git /root/gcamreport

# shiny dependencies
RUN apt-get --allow-releaseinfo-change update
RUN apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    xdg-utils \
    chromium-bsu

RUN ln -sf /usr/bin/chromium-bsu /usr/bin/x-www-browser && \
    ln -sf /usr/bin/chromium-bsu /usr/bin/gnome-www-browser
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

# working directory
WORKDIR /app/
COPY . /app

RUN addgroup --system app \
    && adduser --system --ingroup app app

# Update the package list to install Java JRE
RUN echo 'export PATH=$PATH:/usr/java/jre1.6.0_24/bin/' >> /root/.bashrc
RUN apt-get update && \
    apt-get install libasound2-data=1.2.11-1 && \
    apt-get install -y openjdk-8-jre && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# port and permissions
RUN chown app:app -R /app
USER app
EXPOSE 3838

CMD ["R"]
