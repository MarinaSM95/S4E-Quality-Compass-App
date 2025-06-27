FROM rocker/shiny

LABEL maintainer="Iñaki Ucar <inaki.ucar@uc3m.es>"

RUN Rscript -e 'install.packages("renv")'

WORKDIR /srv/shiny-server
RUN rm -rf *
RUN <<EOF cat >> index.html
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Refresh" content="0; url=./S4E-Quality-Compass-App/" />
  </head>
  <body></body>
</html>
EOF
COPY . ./S4E-Quality-Compass-App
RUN Rscript -e 'install.packages(unique(renv::dependencies()$Package))'
