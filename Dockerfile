FROM debian:stable-slim

ENV APP_DIRECTORY /opt/soep-plots
RUN mkdir ${APP_DIRECTORY}}

COPY ./ ${APP_DIRECTORY}

WORKDIR ${APP_DIRECTORY}


RUN apt-get update -y \
    && apt-get install -y \
    r-base \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    libcurl4-openssl-dev \
    libmagick++-dev \
    libssl-dev \
    git \
    && rm -rf /tmp/* \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('renv')"
RUN R -e "install.packages('languageserver')"

