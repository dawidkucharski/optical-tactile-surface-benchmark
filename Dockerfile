# Reproducible environment for full pipeline
FROM rocker/r-base:4.4.1

# System deps (fonts for xelatex optional; tinytex for PDF)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    ghostscript locales pandoc make g++ \
    && rm -rf /var/lib/apt/lists/*

# Locale
RUN sed -i 's/# *pl_PL.UTF-8/pl_PL.UTF-8/' /etc/locale.gen && locale-gen
ENV LANG=pl_PL.UTF-8 LC_ALL=pl_PL.UTF-8

# R packages
RUN install2.r --error \
    readxl ggplot2 dplyr readr stringr rmarkdown yaml glmnet ranger fastshap && \
    R -q -e "install.packages('tinytex', repos='https://cloud.r-project.org'); tinytex::install_tinytex();" || true

WORKDIR /workspace
COPY . /workspace

# Cache knitr dependencies ahead of time (optional)
RUN Rscript install_packages.R || true

# Default command: run full pipeline
CMD ["bash", "run_all.sh"]
