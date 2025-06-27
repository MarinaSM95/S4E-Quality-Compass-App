FROM rocker/shiny

LABEL maintainer="Iñaki Ucar <inaki.ucar@uc3m.es>"

RUN Rscript -e 'install.packages("renv")'

COPY . /srv/shiny-server/S4E-Quality-Compass-App
WORKDIR /srv/shiny-server/S4E-Quality-Compass-App
RUN Rscript -e 'install.packages(unique(renv::dependencies()$Package))'
