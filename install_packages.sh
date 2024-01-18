#!/bin/bash

# amazon linux requirements
# sudo yum install texlive -y

# ubuntu requirements
# https://www.digitalocean.com/community/tutorials/how-to-set-up-rstudio-on-an-ubuntu-cloud-server
# sudo apt-get install r-base
# sudo apt-get install libapparmor1 gdebi-core
# wget http://download2.rstudio.org/rstudio-server-0.97.336-amd64.deb -O rstudio.deb
# sudo apt-get install pandoc
# sudo apt-get install libssl-dev
# sudo apt-get install libxml2-dev
# sudo apt-get install libgit2-dev
# sudo apt-get install texlive
# sudo apt-get install texlive-fonts-extra
# sudo apt-get install texinfo
# sudo apt-get install fail2ban
# sudo apt-get install libcurl4-openssl-dev

# mac requirements
# brew install r
# brew install pandoc
# brew install fail2ban

# CRAN=https://cran.r-project.org/src/contrib/
# LIB=~/data/R/packages/
# function install_package {
#   mkdir -p $LIB
#   curl ${CRAN}${1}.tar.gz > ${LIB}${1}.tar.gz
#   R CMD INSTALL ${LIB}${1}.tar.gz -l ${LIB}
# }
# install_package data.table_1.13.2
# install_package digest_0.6.27
# install_package glue_1.4.2
# install_package gtable_0.3.0
# install_package brio_1.1.0
# install_package R6_2.5.0
# install_package ps_1.5.0
# install_package processx_3.4.5
# install_package callr_3.5.1
# install_package rprojroot_2.0.2
# install_package assertthat_0.2.1
# install_package crayon_1.3.4
# install_package desc_1.2.0
# install_package fansi_0.4.1
# install_package cli_2.2.0
# install_package rlang_0.4.9
# install_package ellipsis_0.3.1
# install_package evaluate_0.14
# install_package jsonlite_1.7.1
# install_package lifecycle_0.2.0
# install_package magrittr_2.0.1
# install_package rstudioapi_0.13
# install_package prettyunits_1.1.1
# install_package withr_2.3.0
# install_package pkgbuild_1.1.0
# install_package pkgload_1.1.0
# install_package praise_1.0.0
# install_package diffobj_0.3.2
# install_package utf8_1.1.4
# install_package vctrs_0.3.5
# install_package pillar_1.4.7
# install_package pkgconfig_2.0.3
# install_package tibble_3.0.4
# install_package rematch2_2.1.2
# install_package waldo_0.2.3
# install_package testthat_3.0.0
# install_package isoband_0.2.3
# install_package scales_1.1.1
# install_package tibble_3.0.4
# install_package farver_2.0.3
# install_package labeling_0.4.2
# install_package colorspace_2.0-0
# install_package munsell_0.5.0
# install_package RColorBrewer_1.1-2
# install_package viridisLite_0.3.0
# install_package scales_1.1.1
# install_package ggplot2_3.3.2
# install_package gridExtra_2.3
# install_package resample_0.4
# install_package TeachingDemos_2.12
# install_package iterators_1.0.13
# install_package foreach_1.5.1
# install_package doParallel_1.0.16
# install_package xml2_1.3.2

# curl ${CRAN}xml2_1.3.2.tar.gz > ${LIB}xml2_1.3.2.tar.gz
# R CMD INSTALL ${LIB}xml2_1.3.2.tar.gz --configure-vars='INCLUDE_DIR="${LIB}" LIB_DIR="${LIB}"' -l "${LIB}"
# install_package huxtable_5.1.1
# sudo apt-get install libcurl4-openssl-dev
# install_package curl_4.3

# Rscript -e "install.packages(c(\"data.table\", \"ggplot2\", \"TeachingDemos\", \"devtools\", \
#     \"gridExtra\", \"huxtable\", \"gmailr\", \"cronR\", \"rmarkdown\", \"pryr\", \"png\"))"

# upgrade to gbm3 (dev version from  github)
# Rscript -e "library(\"devtools\"); install_github(\"gbm-developers/gbm3\")"

# install pretty.gtable package (dev version from github)
# Rscript -e "library(\"devtools\"); install_github(\"ckear1989-2/pretty.gtable\")"

# Rscript -e "devtools::setup(\"./\")"
# Rscript -e "devtools::load_all()"
# Rscript -e "devtools::document()"
# Rscript -e "devtools::build_manual()"
# Rscript -e "devtools::check(document=TRUE, manual=TRUE, force_suggests=TRUE, run_dont_test=FALSE)"
# Rscript -e "rmarkdown::render(\"README.Rmd\", output_file=\"README.md\")"
Rscript -e "devtools::install()"
Rscript "tests/create.test.data.R"
Rscript -e "testthat::test_file(\"tests/testthat/test_utils.R\", package=\"sodd\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_data_creation.R\", package=\"sodd\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_file_diff.R\", package=\"sodd\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_model_build.R\", package=\"sodd\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_documentation.R\", package=\"sodd\")"
Rscript -e "devtools::test()"

