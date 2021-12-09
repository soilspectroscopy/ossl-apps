FROM rocker/shiny:4.1.1
# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    libnode-dev libjq-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
ENV _R_SHLIB_STRIP_=true
RUN install2.r --error --skipinstalled \
    mongolite \
    jsonify \
    shiny \
    shinydashboard \
    leaflet \
    tidyverse \
    sf \
    RColorBrewer \
    data.table \
    plotly
RUN install2.r --error --skipinstalled \
    remotes \
    devtools \
    systemfonts \
    triebeard
RUN install2.r --error --skipinstalled \
    DT \
    shinythemes \
    shinyjqui \
    shinyWidgets \
    shinyjs \
    shinycssloaders \
    jsonlite \
    geojsonio \
    ggplot2 \
    kableExtra \
    shinyBS \
    s2 \
    magrittr
RUN install2.r --error --skipinstalled \
    leaflet.extras \
    leafgl \
    shinybusy
RUN install2.r --error --skipinstalled \
    yaml \
    pls \
    randomForest \
    tidyr \
    auth0 \
    readxl \
    xgboost \
    Cubist \
    terra \
    sp \
    httr \
    prospectr \
    fastDummies \
    cowplot \
    matrixStats \
    mlr \
    parallelMap \
    plotKML \
    rworldmap
RUN R -e 'install.packages(c("openair", "yardstick"))'

ENV AUTH0_USER=dev-b5nlb7-q.eu
ENV AUTH0_KEY=jBypUtmOeuorrxqNAZcL8YGq5oNQJ74J
ENV AUTH0_SECRET=??? # GET FROM AUTH0

COPY ./app/ /srv/shiny-server/
# RUN chmod 757 -R /srv/shiny-server/data/
USER shiny
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "source('/srv/shiny-server/app.R')"]
# CMD ["/usr/bin/shiny-server"]
# CMD ["tail","-f","/dev/null"]