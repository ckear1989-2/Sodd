Install
=======

    ## Not run:
    # see instructions to create gmail credentials
    # https://github.com/r-lib/gmailr
    # https://developers.google.com/gmail/api/quickstart/python
    Sys.setenv(GMAILR_APP="~/.gmail_credentials.json")
    Sys.setenv(GMAILR_SECRET="~/.secret")
    # if using 2fa with github, add access token at https://github.com/settings/tokens and paste in to a new file
    # Replace the file below with the name of your newly created access token file
    Sys.setenv(GITHUB_PAT=readLines("~/.github_access_token", warn=FALSE))
    install.packages("devtools")
    devtools::install_github("ckear1989/sodd")
    ## End(Not run)

    suppressPackageStartupMessages(library("sodd"))

Set Options
===========

    set.sodd.options(
      data.dir="~/sodd.data/", # where to save csv and rds files
      output.dir="~/sodd.output/", # where to save logs, pdfs etc.
      force.upcoming=TRUE, # simulate upcoming fixtures from test data if none available
      model.params=list(n.trees=10, train.fraction=0.9, n.cores=1, cv.folds=1), # hyperparameters for model
      n.lag=3, # how many previous fixtures to look back for predictor variables
      verbosity=0 # how much is output to logs 0=no output, 1=minimal, 2=all
    )

Create Modeling Data
====================

    leagues <- c("E0", "E1", "SP1", "SP2")
    dload.sodd.modeling.data(leagues, 3, check=TRUE)
    create.sodd.modeling.data(leagues, 3)

Run model
=========

    todays.model <- build.sodd.model(format((Sys.Date()-7), '%Y-%m-%d'), "act", keep.data=TRUE)

    ## Warning in read.model.data(adate, yvar, previous.model.as.offset, weights): no
    ## test matches

Get model output
================

    document.sodd.model(todays.model)

    ## Warning in plot.model(model, model$adate, model$train.a.dt, model$train.b.dt, :
    ## attempting to plot null model

    ## NULL

    strat <- upcoming.strategy.sodd.model(todays.model)

    ## Warning in upcoming.strategy.sodd.model(todays.model): attempting strategy table
    ## with null model

    grid::grid.draw(strat)

    email.sodd.model.results(format(Sys.Date()-7, "%Y-%m-%d"), "ckear1989@gmail.com")

Schedule daily model build
==========================

    # remove existing cronjob
    system("crontab -r")
    schedule.model.build(
      leagues=c("E0", "E1", "E2", "SP1", "D1", "I1", "F1"),
      years=5,
      address="ckear1989@gmail.com"
    )

    ## Adding cronjob:
    ## ---------------
    ## 
    ## ## cronR job
    ## ## id:   sodd.model.build
    ## ## tags: 
    ## ## desc: 
    ## 0 7 * * * /usr/lib/R/bin/Rscript '/home/conor/sodd.output//scheduled.model.R'  >> '/home/conor/sodd.output//scheduled.model.log' 2>&1
