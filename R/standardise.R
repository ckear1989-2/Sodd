
#' @import data.table
read.a.file <- function(a.file) {
  season <- match_id <- hometeam <- awayteam <- date <- ddate <- NULL
  s <- strsplit(a.file, "/") [[1]]
  l <- s[[(length(s)-2)]]
  s <- s[[(length(s)-1)]]
  if(!file.exists(a.file)) {
    dload.league.season(l, s)
    warning(paste("input file", a.file, "not found.",
    "Downloading", l, s))
  }
  a.dt <- fread(a.file)
  a.dt[, season := s]
  setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
  a.dt[nchar(date)==8, ddate := as.Date(date, "%d/%m/%y")]
  a.dt[nchar(date)==10, ddate := as.Date(date, "%d/%m/%Y")]
  a.dt[, date := ddate]
  a.dt[, ddate := NULL]
  cat0n('na date count: ', a.dt[is.na(date), .N], verbosity=2)
  a.dt <- a.dt[!is.na(date), ]
  a.dt[, match_id := paste0(date, "|", hometeam, "|", awayteam, collapse="|"), list(date, hometeam, awayteam)]
  a.dt <- a.dt[match_id != "||", ]
  a.dt
}

#' @import data.table
read.all.data <- function(leagues, years, warn.removal.of.dups=FALSE) {
  season <- ftr <- div <- hometeam <- awayteam <- date <- match_id <- NULL
  data.dir <- get.sodd.data.dir()
  yl <- expand.grid(all.years[1:years], leagues)
  all.csv <- paste0(
    data.dir,
    yl[[1]], "/", yl[[2]], ".csv"
  )
  cat0n(paste(all.csv, collapse="\n"), verbosity=1)
  alist <- lapply(all.csv, read.a.file)
  a.dt <- rbindlist(alist, fill=TRUE)
  setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
  cat0n(colnames(a.dt), verbosity=2)
  upcoming.dt <- read.a.file(file.path(data.dir, upcoming_fixtures))
  setnames(upcoming.dt, colnames(upcoming.dt), tolower(colnames(upcoming.dt)))
  upcoming.dt[, season := max(a.dt[, season])]
  for(var in colnames(a.dt)) {
    if(!var %in% colnames(upcoming.dt)) {
      if(is.numeric(a.dt[[var]])) {
        upcoming.dt[[var]] <- -1
      } else {
        upcoming.dt[[var]] <- "FUCK"
      }
    }
    if(all(is.na(upcoming.dt[[var]]))) {
      if(is.numeric(a.dt[[var]])) {
        upcoming.dt[[var]] <- -1
      } else {
        upcoming.dt[[var]] <- "FUCK"
      }
    }
  }
  upcoming.dt <- upcoming.dt[div %in% leagues, ]
  if(any(a.dt[, match_id] %in% upcoming.dt[, match_id])) {
    if(!isTRUE(warn.removal.of.dups)) {
      warning("matches in historic and upcoming")
    }
    upcoming.dt <- upcoming.dt[!match_id %in% a.dt[, match_id]]
  }
  if(upcoming.dt[, .N] == 0) {
    if(isTRUE(get.sodd.force.upcoming())) {
      upcoming.dt <- a.dt[date == max(a.dt[, date]), ]
      a.dt <- a.dt[date < max(a.dt[, date]), ]
      cat0n("forcing upcoming matches from a.dt", verbosity=1)
    }
  }
  upcoming.dt[, ftr := NULL]
  upcoming.dt[, ftr := "NA"]
  a.dt <- rbind(a.dt, upcoming.dt, fill=TRUE)
  cat0n(colnames(a.dt), verbosity=2)
  a.dt
}

#' @import data.table
#' @importFrom stats na.omit
transpose.rows <- quote({
  season <- hometeam <- awayteam <- ftr <- fthg <- ftag <- b365h <- b365d <-
  b365a <- match_id <- match_ip <- count <- act <- actr <- NULL
  # check teams in leagues
  for (l in leagues) {
    pprint(a.dt[div== l, list(count=.N), hometeam][order(-count)][1:5, ],
      paste(l, "teams"), verbosity=2)
  }
  match.dt <- a.dt[, list(count=.N), match_id][order(-count)]
  match.dt[count > 0, ]
  a.dt <- a.dt[, list(match_id, div, season, hometeam, awayteam, date, ftr, fthg, ftag, b365h, b365d, b365a)]
  pprint(summary(a.dt), "summary(a.dt)", verbosity=2)
  cat0n('data count with NA: ', a.dt[, .N], verbosity=2)
  a.dt <- na.omit(a.dt)
  cat0n('data count without NA: ', a.dt[, .N], verbosity=2)
  a.dt[, b365h := 1 / b365h]
  a.dt[, b365d := 1 / b365d]
  a.dt[, b365a := 1 / b365a]
  a.dt[, match_ip := b365h + b365d + b365a]
  a.dt <- rbind(
    a.dt[ftr != "NA", list(
      match_id, div, date, season, hometeam, awayteam, match_ip,
      actr=ftr, fthg=fthg, ftag=ftag, ip=b365h, act=as.numeric(ftr=='H'), ftr='H')],
    a.dt[ftr != "NA", list(
      match_id, div, date, season, hometeam, awayteam, match_ip,
      actr=ftr, fthg=fthg, ftag=ftag, ip=b365d, act=as.numeric(ftr=='D'), ftr='D')],
    a.dt[ftr != "NA", list(
      match_id, div, date, season, hometeam, awayteam, match_ip,
      actr=ftr, fthg=fthg, ftag=ftag, ip=b365a, act=as.numeric(ftr=='A'), ftr='A')],
    a.dt[ftr == "NA", list(
      match_id, div, date, season, hometeam, awayteam, match_ip,
      actr="NA", fthg=fthg, ftag=ftag, ip=b365h, act=-1, ftr='H')],
    a.dt[ftr == "NA", list(
      match_id, div, date, season, hometeam, awayteam, match_ip,
      actr="NA", fthg=fthg, ftag=ftag, ip=b365d, act=-1, ftr='D')],
    a.dt[ftr == "NA", list(
      match_id, div, date, season, hometeam, awayteam, match_ip,
      actr="NA", fthg=fthg, ftag=ftag, ip=b365a, act=-1, ftr='A')],
    fill=TRUE
  )
  pprint(a.dt[, .N, list(actr, ftr)], "actr, ftr count", verbosity=2)
  cat0n('data transposed count with NA: ', a.dt[, .N], verbosity=2)
  a.dt <- na.omit(a.dt)
  cat0n('data transposed count: ', a.dt[, .N], verbosity=2)
  setkey(a.dt, date)
  a.dt[, rn := seq(a.dt[, .N])]
})

#' @import data.table
result_lag <- function(a.dt, i) {
  hometeam <- awayteam <- date <- ftr <- act <- actr <- r <- pag <- pr <- r <-
  phg <- fthg <- pag <- ftag <- p_date <- trn <- pv <- pd <- pgf <-  pga <-
  hphr <- hph_date <- hphd <- apar <- apa_date <- apad <- hpr <- apr <- hpp <-
  app <- hphp <- apap <- NULL
  all.teams <- unique(c(a.dt[, hometeam], a.dt[, awayteam]))
  cat0n('team count: ', length(all.teams), verbosity=2)
  for(ateam in all.teams) {
    team.dt <- a.dt[(act == 1) & ((hometeam == ateam) | (awayteam == ateam)), ]
    team.up.dt <- a.dt[(act == -1) & ((hometeam == ateam) |
      (awayteam == ateam)), ][, list(hometeam, awayteam, date)]
    setkey(team.dt, date, hometeam, awayteam)
    team.dt <- unique(team.dt)
    team.dt[(hometeam == ateam) & (ftr == 'H'), r := 'W']
    team.dt[(hometeam == ateam) & (ftr == 'A'), r := 'L']
    team.dt[(awayteam == ateam) & (ftr == 'H'), r := 'L']
    team.dt[(awayteam == ateam) & (ftr == 'A'), r := 'W']
    team.dt[ftr == 'D', r := 'D']
    team.up.dt[, r := "NA"]
    setkey(team.up.dt, date, hometeam, awayteam)
    team.up.dt <- unique(team.up.dt)
    team.dt <- rbind(team.dt, team.up.dt, fill=TRUE)
    setkey(team.dt, date)
    team.dt[, trn := seq(team.dt[, .N])]
    l.team.dt <- team.dt[, list(
      hometeam, awayteam, pr=r, phg=fthg, pag=ftag, p_date=date, trn=trn+i)]
    l.team.dt[hometeam == ateam, pv := 'home']
    l.team.dt[awayteam == ateam, pv := 'away']
    l.team.dt[, hometeam := NULL]
    l.team.dt[, awayteam := NULL]
    setkey(team.dt, trn)
    setkey(l.team.dt, trn)
    team.dt <- merge(team.dt, l.team.dt, all.x=TRUE, all.y=FALSE)[, list(
      hometeam, awayteam, date, r, pr, pv, p_date, phg, pag)]
    team.dt[, pd := as.numeric(date - p_date)]
    team.dt[, p_date := NULL]
    team.dt[pv == "home", pgf := phg]
    team.dt[pv == "away", pgf := pag]
    team.dt[pv == "home", pga := pag]
    team.dt[pv == "away", pga := phg]
  
    h.team.dt <- team.dt[hometeam == ateam, list(
      hometeam, awayteam, date, r, pr, pv, pd, pgf, pga)]
    setkey(h.team.dt, date)
    h.team.dt[, trn := seq(h.team.dt[, .N])]
    hl.team.dt <- h.team.dt[, list(hphr=r, hph_date=date, trn=trn+i)]
    setkey(h.team.dt, trn)
    setkey(hl.team.dt, trn)
    h.team.dt <- merge(h.team.dt, hl.team.dt, all.x=TRUE, all.y=FALSE)[, list(
      hometeam, awayteam, date, hpr=pr, hpd=pd, hpv=pv, hphr, hph_date,
      hpgf=pgf, hpga=pga)]
    h.team.dt[, hphd := as.numeric(date - hph_date)]
    h.team.dt[, hph_date := NULL]

    a.team.dt <- team.dt[awayteam == ateam, list(
      hometeam, awayteam, date, r, pr, pv, pd, pgf, pga)]
    setkey(a.team.dt, date)
    a.team.dt[, trn := seq(a.team.dt[, .N])]
    al.team.dt <- a.team.dt[, list(apar=r, apa_date=date, trn=trn+i)]
    setkey(a.team.dt, trn)
    setkey(al.team.dt, trn)
    a.team.dt <- merge(a.team.dt, al.team.dt, all.x=TRUE, all.y=FALSE)[, list(
      hometeam, awayteam, date, apr=pr, apd=pd, apv=pv, apar, apa_date,
      apgf=pgf, apga=pga)]
    a.team.dt[, apad := as.numeric(date - apa_date)]
    a.team.dt[, apa_date := NULL]
 
    if(exists("h.teams.dt")) {
      h.teams.dt <- rbind(h.teams.dt, h.team.dt)
    } else {
      h.teams.dt <- h.team.dt
    }
    if(exists("a.teams.dt")) {
      a.teams.dt <- rbind(a.teams.dt, a.team.dt)
    } else {
      a.teams.dt <- a.team.dt
    }
  }
  cat0n('data count pre unique: ', a.dt[, .N], verbosity=2)
  setkey(a.dt, date, hometeam, awayteam)
  a.dt <- unique(a.dt)
  cat0n('data count post unique: ', a.dt[, .N], verbosity=2)

  cat0n('home count pre unique: ', a.teams.dt[, .N], verbosity=2)
  setkey(h.teams.dt, date, hometeam, awayteam)
  h.teams.dt <- unique(h.teams.dt)
  cat0n('home count post unique: ', a.teams.dt[, .N], verbosity=2)

  cat0n('away count pre unique: ', a.teams.dt[, .N], verbosity=2)
  setkey(a.teams.dt, date, hometeam, awayteam)
  a.teams.dt <- unique(a.teams.dt)
  cat0n('away count post unique: ', a.teams.dt[, .N], verbosity=2)

  cat0n('data count pre merge: ', a.dt[, .N], verbosity=2)
  a.dt <- merge(a.dt, h.teams.dt)
  a.dt <- merge(a.dt, a.teams.dt)
  cat0n('data count post merge: ', a.dt[, .N], verbosity=2)
  cat0n('data count missing hpr: ', a.dt[is.na(hpr), .N], verbosity=2)
  cat0n('data count missing apr: ', a.dt[is.na(apr), .N], verbosity=2)
  cat0n('data count missing hphr: ', a.dt[is.na(hphr), .N], verbosity=2)
  cat0n('data count missing apar: ', a.dt[is.na(apar), .N], verbosity=2)

  a.dt[, hpp := -1]
  a.dt[hpr == "L", hpp := 0]
  a.dt[hpr == "D", hpp := 1]
  a.dt[hpr == "W", hpp := 3]
  a.dt[, app := -1]
  a.dt[apr == "L", app := 0]
  a.dt[apr == "D", app := 1]
  a.dt[apr == "W", app := 3]
  a.dt[, hphp := -1]
  a.dt[hphr == "L", hphp := 0]
  a.dt[hphr == "D", hphp := 1]
  a.dt[hphr == "W", hphp := 3]
  a.dt[, apap := -1]
  a.dt[apar == "L", apap := 0]
  a.dt[apar == "D", apap := 1]
  a.dt[apar == "W", apap := 3]
  a.dt[, hpr := as.factor(hpr)]
  a.dt[, apr := as.factor(apr)]
  a.dt[, hphr := as.factor(hphr)]
  a.dt[, apar := as.factor(apar)]
  all.vars <- expand.grid(
    c("h", "a"), 
    c("pr", "pp", "pd", "pv", "pgf", "pga")
  )
  i.vars <- paste0(all.vars[[1]], all.vars[[2]])
  i.vars <- c(i.vars, paste0("h", c("phr", "phd", "php")))
  i.vars <- c(i.vars, paste0("a", c("par", "pad", "pap")))
  setnames(a.dt, i.vars, paste0(i.vars, i))
  rm(h.teams.dt)
  rm(a.teams.dt)
  a.dt
}

#' @import data.table
#' @importFrom stats na.omit
prep.modeling.vars <- function(a.dt, n.lag) {
  hpr1 <- hpr2 <- hpr3 <- hpr4 <- hpr5 <- apr1 <- apr2 <- apr3 <- apr4 <-
  apr5 <- hometeam <- awayteam <- season <- div <- ftr <- ip <- actr <- app <-
  app_cum <- app_cumi0 <- appi <- appi0 <- happ <- happ_cum <- hpp <-
  hpp_cum <- hpp_cumi0 <- hppi <- hppi0 <- NULL
  lag_result_i <- 1:n.lag
  for (i in lag_result_i) {
    a.dt <- result_lag(a.dt, i)
    setnames(a.dt, paste0("hpp", i), "hpp")
    setnames(a.dt, paste0("app", i), "app")
    a.dt[, happ := pmax(hpp, 0) - pmax(app)]
    setnames(a.dt, "hpp", paste0("hpp", i))
    setnames(a.dt, "app", paste0("app", i))
    setnames(a.dt, "happ", paste0("happ", i))
    if (i > 2) {
      setnames(a.dt, paste0("hpp_cum", (i-1)), "hpp_cumi0")
      setnames(a.dt, paste0("hpp", (i)), "hppi")
      setnames(a.dt, paste0("app_cum", (i-1)), "app_cumi0")
      setnames(a.dt, paste0("app", (i)), "appi")
      a.dt[, hpp_cum := pmax(hpp_cumi0, 0) + pmax(hppi, 0)]
      a.dt[, app_cum := pmax(app_cumi0, 0) + pmax(appi, 0)]
      a.dt[, happ_cum := hpp_cum - app_cum]
      setnames(a.dt, "hpp_cumi0", paste0("hpp_cum", (i-1)))
      setnames(a.dt, "hppi", paste0("hpp", (i)))
      setnames(a.dt, "app_cumi0", paste0("app_cum", (i-1)))
      setnames(a.dt, "appi", paste0("app", (i)))
      setnames(a.dt, "hpp_cum", paste0("hpp_cum", (i)))
      setnames(a.dt, "app_cum", paste0("app_cum", (i)))
      setnames(a.dt, "happ_cum", paste0("happ_cum", (i)))
    } else if(i == 2)  {
      setnames(a.dt, paste0("hpp", (i-1)), "hppi0")
      setnames(a.dt, paste0("hpp", (i)), "hppi")
      setnames(a.dt, paste0("app", (i-1)), "appi0")
      setnames(a.dt, paste0("app", (i)), "appi")
      a.dt[, hpp_cum := pmax(hppi0, 0) + pmax(hppi, 0)]
      a.dt[, app_cum := pmax(appi0, 0) + pmax(appi, 0)]
      a.dt[, happ_cum := hpp_cum - app_cum]
      setnames(a.dt, "hppi0", paste0("hpp", (i-1)))
      setnames(a.dt, "hppi", paste0("hpp", (i)))
      setnames(a.dt, "appi0", paste0("app", (i-1)))
      setnames(a.dt, "appi", paste0("app", (i)))
      setnames(a.dt, "hpp_cum", paste0("hpp_cum", (i)))
      setnames(a.dt, "app_cum", paste0("app_cum", (i)))
      setnames(a.dt, "happ_cum", paste0("happ_cum", (i)))
    }
  }
  # set character variables to factor for modeling
  a.dt[, hometeam := as.factor(hometeam)]
  a.dt[, awayteam := as.factor(awayteam)]
  a.dt[, season := as.factor(season)]
  a.dt[, div := as.factor(div)]
  a.dt[, ftr := as.factor(ftr)]
  a.dt[, ip := round(ip, 3)]
  pprint(summary(a.dt), "summary(a.dt)", verbosity=2)
  cat0n('data count with missings: ', a.dt[, .N], verbosity=2)
  a.dt <- na.omit(a.dt)
  cat0n('data count no missings: ', a.dt[, .N], verbosity=2)
  cat0n('data summary', verbosity=2)
  pprint(summary(a.dt), "summary(a.dt)", verbosity=2)
  # error with odds
  cat0n('data count odds error: ', a.dt[, .N], verbosity=2)
  a.dt <- a.dt[ip != -Inf, ]
  a.dt <- a.dt[ip != Inf, ]
  a.dt <- a.dt[(1/ip) != -Inf, ]
  a.dt <- a.dt[(1/ip) != Inf, ]
  cat0n('data count no odds error: ', a.dt[, .N], verbosity=2)
  pprint(a.dt[, .N, list(actr, ftr)], "actr, ftr count", verbosity=2)
  a.dt
}

report.memory <- function(obj=NULL) {
  if(is.package.available("pryr")) {
    suppressMessages(pryr::object_size(0))
    if(!is.null(obj)) {
      cat0n(deparse(substitute(obj)), " size: ", pryr::object_size(obj) * 1e-6, "Mb", verbosity=1)
    }
    gc()
    cat0n("R memory used: ", pryr::mem_used() * 1e-6, "Mb", verbosity=1)
    # cat0n("system memory free: ", as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) * 1e-3, "Mb", verbosity=1)
    # cat0n("system memory available: ", as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo", intern=TRUE)) * 1e-3, "Mb", verbosity=1)
    # cat0n("system memory total: ", as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE)) * 1e-3, "Mb", verbosity=1)
    # cat0n("system processes running: ", length(system("ps -x", intern=TRUE)), verbosity=1)
  }
}

#' @import data.table
save.modeling.data <- quote({
  setkey(a.dt, rn)
  data.dir <- get.sodd.data.dir()
  if(!file.exists(data.dir)) dir.create(data.dir)
  saveRDS(a.dt, file.path(data.dir, 'a.dt.rds'))
  report.memory(a.dt)
})

restandardise.model.dt <- function(dt, var) {
  a.ip <- d.ip <- ftr <- h.ip <- ip <- match_id <- NULL
  dt[, h.ip := -1]
  dt[, d.ip := -1]
  dt[, a.ip := -1]
  dt[ftr == "H", h.ip := ip]
  dt[ftr == "D", d.ip := ip]
  dt[ftr == "A", a.ip := ip]
  dt[, h.ip := max(h.ip), match_id]
  dt[, d.ip := max(d.ip), match_id]
  dt[, a.ip := max(a.ip), match_id]
  n.lag <- get.sodd.n.lag()
  dt <- unique(dt, by="match_id")
  dt
}

#' Create sodd modeling data
#'
#' @param leagues Character vector of leagues to use. Defaults to all.leagues
#' @param years Number of years to use. Defaults to 10
#' @return NULL
#' @family data_prep
#' @examples
#' \donttest{
#' create.sodd.modeling.data(years=5)
#' }
#' @export
create.sodd.modeling.data <- function(leagues=all.leagues, years=10){
  output.dir <- get.sodd.output.dir()
  if(!file.exists(output.dir)) dir.create(output.dir)
  if(get.sodd.verbosity() >= 1) {
    sink(file.path(output.dir, "standardise.log"), split=TRUE)
  } else {
    sink("/dev/null")
  }
  a.dt <- read.all.data(leagues, years)
  eval(transpose.rows)
  n.lag <- get.sodd.n.lag()
  a.dt <- prep.modeling.vars(a.dt, n.lag)
  eval(save.modeling.data)
  sink()
  invisible()
}

