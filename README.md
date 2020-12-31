sodd
================
Conor Kearney
2020-12-31

# Install

``` r
## Not run:
# see instructions to create gmail credentials
# https://github.com/r-lib/gmailr
# https://developers.google.com/gmail/api/quickstart/python
Sys.setenv(GMAILR_APP="~/.gmail_credentials")
# if using 2fa with github, add access token at https://github.com/settings/tokens
Sys.setenv(GITHUB_PAT=readLines("~/.github_access_token"))
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
  data.dir="~/sodd.data/",
  output.dir="~/sodd.output/",
  force.upcoming=TRUE
)
```

# Create Modeling Data

``` r
leagues <- c("E0", "E1", "SP1", "SP2")
dload.x.years(leagues, 3, quiet=TRUE, force=FALSE)
dload.current.year(quiet=TRUE)
dload.upcoming(quiet=TRUE)
create.sodd.modeling.data(leagues, 3)
```

# Run model

``` r
todays.model <- build.sodd.model(format((Sys.Date()-7), '%Y-%m-%d'), "act", n.trees=10)
```

# Get model output

``` r
document.sodd.model(todays.model)
```

    ## [1] "see model documentation in ~/sodd.output/model_2020-12-24_act.pdf"

``` r
strat <- upcoming.strategy.sodd.model(todays.model)
grid::grid.draw(strat)
```

![](inst/extdata/README_files/output-1.png)

``` r
email.sodd.model.results(format(Sys.Date()-7, "%Y-%m-%d"), "ckear1989@gmail.com")
```

    ## Using an auto-discovered, cached token.
    ## To suppress this message, modify your code or options to clearly consent to the use of a cached token.
    ## See gargle's "Non-interactive auth" vignette for more details:
    ## https://gargle.r-lib.org/articles/non-interactive-auth.html

    ## The gmailr package is using a cached token for ckear1989@gmail.com.

# Schedule daily model build

``` r
schedule.model.build(address="ckear1989@gmail.com")
```

    ## Adding cronjob:
    ## ---------------
    ## 
    ## ## cronR job
    ## ## id:   sodd.model.build
    ## ## tags: 
    ## ## desc: 
    ## 0 7 * * * /usr/lib/R/bin/Rscript '~/sodd.output//scheduled.model.R'  >> '~/sodd.output//scheduled.model.log' 2>&1
