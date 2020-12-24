sodd
================
Conor Kearney
2020-12-24

# Install

``` r
  Sys.setenv(GITHUB_PAT=readLines("~/.github_access_token"))
  install.packages("devtools")
  devtools::install_github("ckear1989/sodd")
```

``` r
  suppressPackageStartupMessages(library("sodd"))
```

# Set Options

``` r
  set_sodd_options(
    data.dir="~/sodd.data/",
    output.dir="~/sodd.output/",
    force.upcoming=TRUE
  )
```

# Create Modeling Data

``` r
  leagues <- c("E0", "E1", "SP1", "SP2")
  dload_x_years(leagues, 3, quiet=TRUE, force=FALSE)
  dload_current_year(quiet=TRUE)
  dload_upcoming(quiet=TRUE)
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

    ## [1] "see model documentation in ~/sodd.output/model_2020-12-17_act.pdf"

``` r
  strat <- upcoming.strategy.sodd.model(todays.model)
  grid::grid.draw(strat)
```

![](inst/extdata/README_files/output-1.png)
