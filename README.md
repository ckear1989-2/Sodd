sodd
================
Conor Kearney
2021-01-31

# Install

``` r
## Not run:
# see instructions to create gmail credentials
# https://github.com/r-lib/gmailr
# https://developers.google.com/gmail/api/quickstart/python
Sys.setenv(GMAILR_APP="~/.gmail_credentials")
# if using 2fa with github, add access token at https://github.com/# settings/tokens and paste in to a new file
# Replace the filde below with the name of your access token file
Sys.setenv(GITHUB_PAT=readLines("~/.github_access_token", warn=FALSE)))
install.packages("devtools")
devtools::install_github("ckear1989/sodd")
## End(Not run)
```

``` r
suppressPackageStartupMessages(library("sodd"))
```

# Set Options

``` r
set.sodd.options(
  data.dir="~/sodd.data/", # where to save csv and rds files
  output.dir="~/sodd.output/", # where to save logs, pdfs etc.
  force.upcoming=TRUE, # simulate upcoming fixtures from test data if none available
  model.params=list(n.trees=10, train.fraction=0.9, n.cores=1, cv.folds=1), # hyperparameters for model
  n.lag=3, # how many previous fixtures to look back for predictor variables
  verbosity=0 # how much is output to logs 0=no output, 1=minimal, 2=all
)
```

# Create Modeling Data

``` r
leagues <- c("E0", "E1", "SP1", "SP2")
dload.sodd.modeling.data(leagues, 3, check=TRUE)
create.sodd.modeling.data(leagues, 3)
```

# Run model

``` r
todays.model <- build.sodd.model(format((Sys.Date()-7), '%Y-%m-%d'), "act", keep.data=TRUE)
```

# Get model output

``` r
document.sodd.model(todays.model)
```

    ## [1] "see model documentation in /root/sodd.output/model_2021-01-24_act.pdf"

``` r
strat <- upcoming.strategy.sodd.model(todays.model)
grid::grid.draw(strat)
```

![](inst/extdata/README_files/output-1.png)

``` r
email.sodd.model.results(format(Sys.Date()-7, "%Y-%m-%d"), "ckear1989@gmail.com")
```

# Schedule daily model build

``` r
schedule.model.build(
  leagues=c("E0", "E1", "E2", "SP1", "D1", "I1", "F1"),
  years=5,
  address="ckear1989@gmail.com"
)
```

    ## cron task already exists
    ## Warning in cronR::cron_add(command = cmd, frequency = "daily", at = "7AM", : Can't add this job: a job with id 'sodd.model.build' already exists.
