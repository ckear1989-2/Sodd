#!/bin/bash

# sudo apt-get install r-base-core
LIB=~/data/R/packages/
# R CMD INSTALL --help
# mkdir -p ~/data/R/packages/
# curl https://cran.r-project.org/src/contrib/data.table_1.13.2.tar.gz > ~/data/R/packages/data.table_1.13.2.tar.gz
# R CMD INSTALL ~/data/R/packages/data.table_1.13.2.tar.gz -l ~/data/R/packages/
# curl https://cran.r-project.org/src/contrib/gbm_2.1.8.tar.gz > $LIB/gbm.2.1.8.tar.gz
# R CMD INSTALL $LIB/gbm.2.1.8.tar.gz -l $LIB

# Rscript data_prep/download.R
Rscript data_prep/standardise.R

