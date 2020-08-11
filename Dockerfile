FROM rocker/shiny:3.6.2
RUN apt-get update && apt-get install -y  default-jre-headless gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libpng-dev libssh2-1-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("config")'
RUN R -e 'install.packages("golem")'
RUN R -e 'install.packages("DBI")'
RUN R -e 'install.packages("RSQLite")'
RUN R -e 'install.packages("leaflet")'
RUN R -e 'install.packages("dplyr")'
RUN R -e 'install.packages("sf")'
RUN R -e 'install.packages("shinyjs")'
RUN R -e 'install.packages("htmltools")'
RUN R -e 'install.packages("shinybusy")'
RUN R -e 'install.packages("shinyWidgets")'
RUN R -e 'install.packages("shinyBS")'
RUN R -e 'install.packages("stringr")'
RUN R -e 'install.packages("ggplot2")'
RUN R -e 'install.packages("ggtext")'
RUN R -e 'install.packages("tidyr")'
RUN R -e 'install.packages("gridExtra")'
RUN R -e 'install.packages("leaflet")'
RUN R CMD javareconf
RUN R -e 'install.packages("sf")'
RUN apt-get update
RUN apt-get install -y openjdk-11-jdk
RUN R CMD javareconf
RUN apt-get install -y libbz2-dev libicu-dev
RUN R -e 'install.packages("mailR")'	

RUN wget https://repo1.maven.org/maven2/javax/activation/javax.activation-api/1.2.0/javax.activation-api-1.2.0.jar
RUN wget https://repo1.maven.org/maven2/com/sun/activation/javax.activation/1.2.0/javax.activation-1.2.0.jar

RUN R -e 'file.copy("javax.activation-api-1.2.0.jar", system.file("java", package = "mailR"))'
RUN R -e 'file.copy("javax.activation-1.2.0.jar", system.file("java", package = "mailR"))'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
COPY boursefonciereforestiere_0.0.0.9000.tar.gz /build_zone
RUN R -e 'install.packages("boursefonciereforestiere_0.0.0.9000.tar.gz",repos=NULL)'
EXPOSE 3838

