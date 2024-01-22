Install
=======

    ## Not run:
    # see instructions to create gmail credentials
    # https://github.com/r-lib/gmailr
    # https://developers.google.com/gmail/api/quickstart/python
    Sys.setenv(GMAILR_APP = "~/.gmail_credentials.json")
    Sys.setenv(GMAILR_SECRET = "~/.secret")
    # if using 2fa with github, add access token at https://github.com/settings/tokens and paste in to a new file
    # Replace the file below with the name of your newly created access token file
    Sys.setenv(GITHUB_PAT = readLines("~/.github_access_token", warn = FALSE))
    install.packages("devtools")
    devtools::install_github("ckear1989/sodd")
    ## End(Not run)

    suppressPackageStartupMessages(library("sodd"))

Set Options
===========

    set.sodd.options(
      data.dir = "~/sodd.data/", # where to save csv and rds files
      output.dir = "~/sodd.output/", # where to save logs, pdfs etc.
      force.upcoming = TRUE, # simulate upcoming fixtures from test data if none available
      model.params = list(n.trees = 10, train.fraction = 0.9, cv.folds = 1), # hyperparameters for model
      n.lag = 3, # how many previous fixtures to look back for predictor variables
      verbosity = 0 # how much is output to logs 0=no output, 1=minimal, 2=all
    )

Create Modeling Data
====================

    leagues <- c("E0", "E1", "SP1", "SP2")
    dload.sodd.modeling.data(leagues, 3, check = TRUE)
    create.sodd.modeling.data(leagues, 3)

    ## Warning in read.all.data(leagues, years): matches in historic and upcoming

Run model
=========

    todays.model <- build.sodd.model(format((Sys.Date() - 7), "%Y-%m-%d"), "act")

    ## Warning in best_iter_out_of_bag(gbm_fit_obj): OOB generally underestimates
    ## the optimal number of iterations although predictive performance is reasonably
    ## competitive. Using cv_folds>1 when calling gbm usually results in improved
    ## predictive performance.

Get model output
================

    document.sodd.model(todays.model)

    ## Warning in gbm.doc::plot_model_perf(model): plotting model perf with no cv.

![](README_files/figure-markdown_strict/get-model-otuput-1.png)![](README_files/figure-markdown_strict/get-model-otuput-2.png)

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

    ## Warning in rebase.y(c(summary.dt[, weight], 0), summary.dt[, y]): y2 values
    ## constant when attempting rebase.

![](README_files/figure-markdown_strict/get-model-otuput-3.png)![](README_files/figure-markdown_strict/get-model-otuput-4.png)![](README_files/figure-markdown_strict/get-model-otuput-5.png)![](README_files/figure-markdown_strict/get-model-otuput-6.png)![](README_files/figure-markdown_strict/get-model-otuput-7.png)![](README_files/figure-markdown_strict/get-model-otuput-8.png)![](README_files/figure-markdown_strict/get-model-otuput-9.png)![](README_files/figure-markdown_strict/get-model-otuput-10.png)![](README_files/figure-markdown_strict/get-model-otuput-11.png)![](README_files/figure-markdown_strict/get-model-otuput-12.png)![](README_files/figure-markdown_strict/get-model-otuput-13.png)![](README_files/figure-markdown_strict/get-model-otuput-14.png)![](README_files/figure-markdown_strict/get-model-otuput-15.png)![](README_files/figure-markdown_strict/get-model-otuput-16.png)![](README_files/figure-markdown_strict/get-model-otuput-17.png)![](README_files/figure-markdown_strict/get-model-otuput-18.png)![](README_files/figure-markdown_strict/get-model-otuput-19.png)![](README_files/figure-markdown_strict/get-model-otuput-20.png)![](README_files/figure-markdown_strict/get-model-otuput-21.png)![](README_files/figure-markdown_strict/get-model-otuput-22.png)![](README_files/figure-markdown_strict/get-model-otuput-23.png)![](README_files/figure-markdown_strict/get-model-otuput-24.png)![](README_files/figure-markdown_strict/get-model-otuput-25.png)![](README_files/figure-markdown_strict/get-model-otuput-26.png)![](README_files/figure-markdown_strict/get-model-otuput-27.png)![](README_files/figure-markdown_strict/get-model-otuput-28.png)![](README_files/figure-markdown_strict/get-model-otuput-29.png)![](README_files/figure-markdown_strict/get-model-otuput-30.png)![](README_files/figure-markdown_strict/get-model-otuput-31.png)![](README_files/figure-markdown_strict/get-model-otuput-32.png)![](README_files/figure-markdown_strict/get-model-otuput-33.png)![](README_files/figure-markdown_strict/get-model-otuput-34.png)![](README_files/figure-markdown_strict/get-model-otuput-35.png)![](README_files/figure-markdown_strict/get-model-otuput-36.png)![](README_files/figure-markdown_strict/get-model-otuput-37.png)![](README_files/figure-markdown_strict/get-model-otuput-38.png)![](README_files/figure-markdown_strict/get-model-otuput-39.png)![](README_files/figure-markdown_strict/get-model-otuput-40.png)![](README_files/figure-markdown_strict/get-model-otuput-41.png)![](README_files/figure-markdown_strict/get-model-otuput-42.png)![](README_files/figure-markdown_strict/get-model-otuput-43.png)![](README_files/figure-markdown_strict/get-model-otuput-44.png)![](README_files/figure-markdown_strict/get-model-otuput-45.png)![](README_files/figure-markdown_strict/get-model-otuput-46.png)![](README_files/figure-markdown_strict/get-model-otuput-47.png)![](README_files/figure-markdown_strict/get-model-otuput-48.png)![](README_files/figure-markdown_strict/get-model-otuput-49.png)![](README_files/figure-markdown_strict/get-model-otuput-50.png)![](README_files/figure-markdown_strict/get-model-otuput-51.png)![](README_files/figure-markdown_strict/get-model-otuput-52.png)![](README_files/figure-markdown_strict/get-model-otuput-53.png)![](README_files/figure-markdown_strict/get-model-otuput-54.png)

    ## [1] "see model documentation in /home/conor/sodd.output/model_2024-01-15_act.pdf"

    strat <- upcoming.strategy.sodd.model(todays.model)
    grid::grid.draw(strat)

![](README_files/figure-markdown_strict/strategy-1.png)

    email.sodd.model.results(format(Sys.Date() - 7, "%Y-%m-%d"), "ckear1989@gmail.com")

    ## [1] "debugging email for 2024-01-15 ckear1989@gmail.com"

Schedule daily model build
==========================

    # remove existing cronjob
    system("crontab -r")
    schedule.model.build(
      leagues = c("E0", "E1", "E2", "SP1", "D1", "I1", "F1"),
      years = 5,
      address = "ckear1989@gmail.com"
    )

    ## Are you sure you want to add the specified cron job: '/usr/lib/R/bin/Rscript '/home/conor/sodd.output//scheduled.model.R'  >> '/home/conor/sodd.output//scheduled.model.log' 2>&1'? [y/n]:

    ## cron task already exists
    ## Warning in if (!input %in% "y") {: argument is of length zero
