FROM r-base:4.0.5

ENV APP_DIRECTORY /opt/soep-plots
RUN mkdir ${APP_DIRECTORY}}

COPY ./ ${APP_DIRECTORY}

WORKDIR ${APP_DIRECTORY}


RUN apt-get update -y \
    && apt-get install -y \
    libcurl4-openssl-dev \
    libmagick++-dev \
    libssl-dev \
    git \
    && Rscript -e 'install.packages("remotes")' \
    && Rscript -e 'remotes::install_github("rstudio/renv")' \
    && Rscript -e "renv::install()" \
    && rm -rf /tmp/* \
    && rm -rf /var/lib/apt/lists/*
