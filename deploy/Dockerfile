# ParnellBenchmark — container image for Parnell Azure hosting.
# Build FROM THE REPO ROOT so .dockerignore (whitelist) governs the context:
#   docker build -f deploy/Dockerfile -t parnell-benchmark .
# The image contains ONLY the app, three function files, and the two
# precomputed aggregate rds files. No cow-level data (event_files /
# intermediate_files) can enter the build context — see /.dockerignore.

FROM rocker/shiny:4.5.0

RUN install2.r --error --skipinstalled \
    bslib tidyverse arrow gt lubridate scales here

WORKDIR /app
COPY ParnellBenchmark/ ParnellBenchmark/
COPY functions/fxn_parnell_cr.R \
     functions/fxn_parnell_eligibility.R \
     functions/fxn_own_herd_measures.R \
     functions/
COPY data/parnell_files/benchmark_data_v2.rds \
     data/parnell_files/own_data.rds \
     data/parnell_files/

# here::here() anchors on this marker, so the app's relative paths resolve.
RUN touch /app/.here

EXPOSE 3838
CMD ["R", "-e", "setwd('/app'); shiny::runApp('ParnellBenchmark', host='0.0.0.0', port=3838)"]
