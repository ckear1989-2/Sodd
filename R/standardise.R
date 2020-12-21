
read.a.file <- function(a.file) {
  s <- strsplit(a.file, "/") [[1]]
  s <- s[[(length(s)-1)]]
  a.dt <- data.table::fread(a.file)
  a.dt[, season := s]
  a.dt
}
read.all.data <- quote({
  alist <- lapply(
    all.csv
    ,
    read.a.file
  )
  a.dt <- data.table::rbindlist(alist, fill=TRUE)
  data.table::setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
  print(colnames(a.dt))
  upcoming.dt <- read.a.file(paste0("~/data/", upcoming_fixtures))
  data.table::setnames(upcoming.dt, colnames(upcoming.dt), tolower(colnames(upcoming.dt)))
  upcoming.dt[, season := max(a.dt[, season])]
  upcoming.dt[, ftr := NULL]
  upcoming.dt[, ftr := "NA"]
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
  a.dt[, match_id := paste0(date, "|", hometeam, "|", awayteam, collapse="|"), list(date, hometeam, awayteam)]
  upcoming.dt[, match_id := paste0(date, "|", hometeam, "|", awayteam, collapse="|"), list(date, hometeam, awayteam)]
  if(any(a.dt[, match_id] %in% upcoming.dt[, match_id])) {
    warning("matches in historic and upcoming")
    upcoming.dt <-upcoming.dt[!match_id %in% a.dt[, match_id]]
  }
  print(a.dt[, .N])
  a.dt <- rbind(a.dt, upcoming.dt, fill=TRUE)
  print(colnames(a.dt))
  print(a.dt[, .N])
  print(upcoming.dt[, .N])
})

transpose.rows <- quote({
  # check teams in leagues
  # for (l in leagues) print(a.dt[div== l, list(count=.N), hometeam][order(-count)][1:5, ])
  a.dt[, match_id := paste0(date, "|", hometeam, "|", awayteam, collapse="|"), list(date, hometeam, awayteam)]
  match.dt <- a.dt[, list(count=.N), match_id][order(-count)]
  match.dt
  match.dt[count > 0, ]
  a.dt <- a.dt[match_id != "||", ]
  a.dt[nchar(date)==8, ddate := as.Date(date, '%d/%m/%y')]
  a.dt[nchar(date)==10, ddate := as.Date(date, '%d/%m/%Y')]
  cat('na date count:', a.dt[is.na(date), .N], '\n')
  a.dt <- a.dt[!is.na(ddate), ]
  a.dt <- a.dt[, list(div, date=ddate, season, hometeam, awayteam, ftr, fthg, ftag, b365h, b365d, b365a)]
  cat('data summary\n')
  print(summary(a.dt))
  cat('null home team\n')
  a.dt[is.na(hometeam), ]
  a.dt[hometeam == '', ]
  cat('data count with NA', a.dt[, .N], '\n')
  a.dt <- na.omit(a.dt)
  cat('data count without NA', a.dt[, .N], '\n')
  a.dt[, b365h := 1 / b365h]
  a.dt[, b365d := 1 / b365d]
  a.dt[, b365a := 1 / b365a]
  a.dt[, sip := b365h + b365d + b365a]
  a.dt <- rbind(
    a.dt[ftr != "NA", list(div, date, season, hometeam, awayteam, actr=ftr, fthg=fthg, ftag=ftag, match_ip=(b365h+b365d+b365a), ip=b365h, act=as.numeric(ftr=='H'), ftr='H')],
    a.dt[ftr != "NA", list(div, date, season, hometeam, awayteam, actr=ftr, fthg=fthg, ftag=ftag, match_ip=(b365h+b365d+b365a), ip=b365d, act=as.numeric(ftr=='D'), ftr='D')],
    a.dt[ftr != "NA", list(div, date, season, hometeam, awayteam, actr=ftr, fthg=fthg, ftag=ftag, match_ip=(b365h+b365d+b365a), ip=b365a, act=as.numeric(ftr=='A'), ftr='A')],
    a.dt[ftr == "NA", list(div, date, season, hometeam, awayteam, actr="NA", fthg=fthg, ftag=ftag, match_ip=(b365h+b365d+b365a), ip=b365h, act=-1, ftr='H')],
    a.dt[ftr == "NA", list(div, date, season, hometeam, awayteam, actr="NA", fthg=fthg, ftag=ftag, match_ip=(b365h+b365d+b365a), ip=b365d, act=-1, ftr='D')],
    a.dt[ftr == "NA", list(div, date, season, hometeam, awayteam, actr="NA", fthg=fthg, ftag=ftag, match_ip=(b365h+b365d+b365a), ip=b365a, act=-1, ftr='A')]
  )
  a.dt[, match_id := paste0(date, "|", hometeam, "|", awayteam, collapse="|"), list(date, hometeam, awayteam)]
  cat('data transposed count with NA', a.dt[, .N], '\n')
  a.dt <- na.omit(a.dt)
  cat('data transposed count', a.dt[, .N], '\n')
  data.table::setkey(a.dt, date)
  a.dt[, rn := seq(a.dt[, .N])]
  
  all.teams <- unique(c(a.dt[, hometeam], a.dt[, awayteam]))
  cat('team count', length(all.teams), '\n')
  upcoming.dt <- a.dt[actr == "NA", ]
  if(upcoming.dt[, .N] == 0) {
    print(a.dt[, .N])
    stop("no upcoming matches")
  }
})

result_lag <- function(a.dt, i) {
  for(ateam in all.teams) {
  
    team.dt <- a.dt[(act == 1) & ((hometeam == ateam) | (awayteam == ateam)), ]
    team.up.dt <- a.dt[(act == -1) & ((hometeam == ateam) | (awayteam == ateam)), ][, list(hometeam, awayteam, date)]
    data.table::setkey(team.dt, date, hometeam, awayteam)
    team.dt <- unique(team.dt)
    team.dt[(hometeam == ateam) & (ftr == 'H'), r := 'W']
    team.dt[(hometeam == ateam) & (ftr == 'A'), r := 'L']
    team.dt[(awayteam == ateam) & (ftr == 'H'), r := 'L']
    team.dt[(awayteam == ateam) & (ftr == 'A'), r := 'W']
    team.dt[ftr == 'D', r := 'D']
    team.up.dt[, r := "NA"]
    data.table::setkey(team.up.dt, date, hometeam, awayteam)
    team.up.dt <- unique(team.up.dt)
    team.dt <- rbind(team.dt, team.up.dt, fill=TRUE)
    data.table::setkey(team.dt, date)
    team.dt[, trn := seq(team.dt[, .N])]
    l.team.dt <- team.dt[, list(
      hometeam, awayteam, pr=r, phg=fthg, pag=ftag, p_date=date, trn=trn+i)]
    l.team.dt[hometeam == ateam, pv := 'home']
    l.team.dt[awayteam == ateam, pv := 'away']
    l.team.dt[, hometeam := NULL]
    l.team.dt[, awayteam := NULL]
    data.table::setkey(team.dt, trn)
    data.table::setkey(l.team.dt, trn)
    team.dt <- merge(team.dt, l.team.dt, all.x=TRUE, all.y=FALSE)[, list(
      hometeam, awayteam, date, r, pr, pv, p_date, phg, pag)]
    team.dt[, pd := as.numeric(date - p_date)]
    team.dt[, p_date := NULL]
    team.dt[pv == "home", pgf := phg]
    team.dt[pv == "away", pgf := pag]
    team.dt[pv == "home", pga := pag]
    team.dt[pv == "away", pga := phg]
  
    h.team.dt <- team.dt[hometeam == ateam, list(hometeam, awayteam, date, r, pr, pv, pd, pgf, pga)]
    data.table::setkey(h.team.dt, date)
    h.team.dt[, trn := seq(h.team.dt[, .N])]
    hl.team.dt <- h.team.dt[, list(hphr=r, hph_date=date, trn=trn+i)]
    data.table::setkey(h.team.dt, trn)
    data.table::setkey(hl.team.dt, trn)
    h.team.dt <- merge(h.team.dt, hl.team.dt, all.x=TRUE, all.y=FALSE)[, list(
      hometeam, awayteam, date, hpr=pr, hpd=pd, hpv=pv, hphr, hph_date, hpgf=pgf, hpga=pga)]
    h.team.dt[, hphd := as.numeric(date - hph_date)]
    h.team.dt[, hph_date := NULL]

    a.team.dt <- team.dt[awayteam == ateam, list(hometeam, awayteam, date, r, pr, pv, pd, pgf, pga)]
    data.table::setkey(a.team.dt, date)
    a.team.dt[, trn := seq(a.team.dt[, .N])]
    al.team.dt <- a.team.dt[, list(apar=r, apa_date=date, trn=trn+i)]
    data.table::setkey(a.team.dt, trn)
    data.table::setkey(al.team.dt, trn)
    a.team.dt <- merge(a.team.dt, al.team.dt, all.x=TRUE, all.y=FALSE)[, list(
      hometeam, awayteam, date, apr=pr, apd=pd, apv=pv, apar, apa_date, apgf=pgf, apga=pga)]
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
  cat('data count pre unique', a.dt[, .N], '\n')
  data.table::setkey(a.dt, date, hometeam, awayteam)
  a.dt <- unique(a.dt)
  cat('data count post unique', a.dt[, .N], '\n')

  cat('home count pre unique', a.teams.dt[, .N], '\n')
  data.table::setkey(h.teams.dt, date, hometeam, awayteam)
  h.teams.dt <- unique(h.teams.dt)
  cat('home count post unique', a.teams.dt[, .N], '\n')

  cat('away count pre unique', a.teams.dt[, .N], '\n')
  data.table::setkey(a.teams.dt, date, hometeam, awayteam)
  a.teams.dt <- unique(a.teams.dt)
  cat('away count post unique', a.teams.dt[, .N], '\n')

  cat('data count pre merge', a.dt[, .N], '\n')
  a.dt <- merge(a.dt, h.teams.dt)
  a.dt <- merge(a.dt, a.teams.dt)
  cat('data count post merge', a.dt[, .N], '\n')
  cat('data count missing hpr', a.dt[is.na(hpr), .N], '\n')
  cat('data count missing apr', a.dt[is.na(apr), .N], '\n')
  cat('data count missing hphr', a.dt[is.na(hphr), .N], '\n')
  cat('data count missing apar', a.dt[is.na(apar), .N], '\n')

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
    c(
      "h",
      "a"
    ), 
    c(
      "pr",
      "pp",
      "pd",
      "pv",
      "pgf",
      "pga"
    )
  )
  i.vars <- paste0(all.vars[[1]], all.vars[[2]])
  i.vars <- c(i.vars, paste0("h", c("phr", "phd", "php")))
  i.vars <- c(i.vars, paste0("a", c("par", "pad", "pap")))
  print(i.vars)
  print(colnames(a.dt))
  data.table::setnames(a.dt, i.vars, paste0(i.vars, i))
  rm(h.teams.dt)
  rm(a.teams.dt)
  a.dt
}

prep.modeling.vars <- quote({
  lag_result_i <- 1:5
  for (i in lag_result_i) a.dt <- result_lag(a.dt, i)
  # print(head(a.dt))
  a.dt[, .N, hpr1]
  a.dt[, .N, apr1]
  a.dt[, .N, hpr2]
  a.dt[, .N, apr2]
  a.dt[, .N, hpr3]
  a.dt[, .N, apr3]
  a.dt[, .N, hpr4]
  a.dt[, .N, apr4]
  a.dt[, .N, hpr5]
  a.dt[, .N, apr5]
  a.dt[, hpp_cum2 := pmax(hpp1, 0) + pmax(hpp2, 0)]
  a.dt[, hpp_cum3 := hpp_cum2 + pmax(hpp3, 0)]
  a.dt[, hpp_cum4 := hpp_cum3 + pmax(hpp4, 0)]
  a.dt[, hpp_cum5 := hpp_cum4 + pmax(hpp5, 0)]
  a.dt[, app_cum2 := pmax(app1, 0) + pmax(app2, 0)]
  a.dt[, app_cum3 := app_cum2 + pmax(app3, 0)]
  a.dt[, app_cum4 := app_cum3 + pmax(app4, 0)]
  a.dt[, app_cum5 := app_cum4 + pmax(app5, 0)]
  # a.dt[(is.na(hpr4)) & (!is.na(apr4)), ]
  # a.dt[(is.na(apr4)) & (!is.na(hpr4)), ]
  # set character variables to factor for modeling
  a.dt[, hometeam := as.factor(hometeam)]
  a.dt[, awayteam := as.factor(awayteam)]
  a.dt[, season := as.factor(season)]
  a.dt[, div := as.factor(div)]
  a.dt[, ftr := as.factor(ftr)]
  a.dt[, ip := round(ip, 3)]
  summary(a.dt)
  cat('data count with missings', a.dt[, .N], '\n')
  a.dt[is.na(apr1), apr1 := "M"]
  a.dt[is.na(hpr1), hpr1 := "M"]
  a.dt[is.na(apr2), apr2 := "M"]
  a.dt[is.na(hpr2), hpr2 := "M"]
  a.dt[is.na(apr3), apr3 := "M"]
  a.dt[is.na(hpr3), hpr3 := "M"]
  a.dt[is.na(apr4), apr4 := "M"]
  a.dt[is.na(hpr4), hpr4 := "M"]
  a.dt[is.na(apr5), apr5 := "M"]
  a.dt[is.na(hpr5), hpr5 := "M"]
  a.dt[is.na(apd1), apd1 := -1]
  a.dt[is.na(hpd1), hpd1 := -1]
  a.dt[is.na(apd2), apd2 := -1]
  a.dt[is.na(hpd2), hpd2 := -1]
  a.dt[is.na(apd3), apd3 := -1]
  a.dt[is.na(hpd3), hpd3 := -1]
  a.dt[is.na(apd4), apd4 := -1]
  a.dt[is.na(hpd4), hpd4 := -1]
  a.dt[is.na(apd5), apd5 := -1]
  a.dt[is.na(hpd5), hpd5 := -1]
  for (x in colnames(a.dt)) {
    if(any(is.na(a.dt[[x]]))) print(x)
  }
  for (x in colnames(a.dt)) {
    if(any(is.na(a.dt[[x]]))) print(x)
  }
  a.dt <- na.omit(a.dt)
  cat('data count no missings', a.dt[, .N], '\n')
  cat('data summary', '\n')
  summary(a.dt)
  # error with odds
  cat('data count odds error', a.dt[, .N], '\n')
  a.dt <- a.dt[ip != -Inf, ]
  a.dt <- a.dt[ip != Inf, ]
  a.dt <- a.dt[(1/ip) != -Inf, ]
  a.dt <- a.dt[(1/ip) != Inf, ]
  cat('data count no odds error', a.dt[, .N], '\n')
})

save.modeling.data <- quote({
  data.table::setkey(a.dt, rn)
  if(!file.exists('~/data/R/rds/')) dir.create('~/data/R/rds/')
  saveRDS(a.dt, '~/data/R/rds/a.dt.rds')
})

# args = commandArgs()
# this_file <- "standardise.R"
# file_run <- ""
# if(length(args) > 3) file_run <- strsplit(args[[4]], "/")[[1]][[2]]
# # "--file=data_prep/standardise.R"
# # print(this_file)
# # print(file_run)
# if(file_run == this_file) {
#   if(!file.exists("logs/")) dir.create("logs")
#   sink("logs/standardise.log", split=TRUE)
#   eval(read.all.data)
#   eval(transpose.rows)
#   eval(prep.modeling.vars)
#   eval(save.modeling.data)
#   sink()
# }
