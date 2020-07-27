FROM rocker/r-ver:3.6.2
RUN apt-get update && apt-get install -y  default-jre-headless gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libpng-dev libssh2-1-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.0")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "0.8.3")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-5")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("ggtext",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("gridExtra",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("mailR",upgrade="never", version = "0.4.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');boursefonciereforestiere::run_app()"
