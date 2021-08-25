FROM opencpu/base
RUN apt-get -y install libudunits2-dev libgdal-dev libproj-dev libgeos-dev libudunits2-dev libv8-dev libcairo2-dev libnetcdf-dev libgmp-dev
RUN R -e 'install.packages("rgdal")'
RUN R -e 'install.packages("reg")'
RUN R -e 'install.packages("BCEA")'
RUN R -e 'install.packages("splancs")'
RUN R -e 'remotes::install_version("ldr", version = "1.3.3", repos = "http://cran.uk.r-project.org")'
RUN R -e 'install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)'
RUN R -e 'remotes::install_github("resplab/inlaPrism")'
RUN echo "opencpu:opencpu" | chpasswd
