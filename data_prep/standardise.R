.libPaths('~/data/R/packages/')
.libPaths()
library("data.table")

print(getwd())
source("data_prep/constants.R")
all.leagues <- expand.grid(years, leagues)
all.csv <- paste0("~/data/", all.leagues[[1]], "/", all.leagues[[2]], ".csv")
all.csv
read.a.file <- function(a.file) {
  s <- strsplit(a.file, "/") [[1]]
  s <- s[[(length(s)-1)]]
  a.dt <- fread(a.file)
  a.dt[, season := s]
  a.dt
}
alist <- lapply(
  all.csv
  ,
  read.a.file
)
a.dt <- rbindlist(alist, fill=TRUE)

# error in odds
# a.dt[HomeTeam=="Brentford" & AwayTeam=="Blackburn", ]

# colnames(a.dt)

# check teams in leagues
for (l in leagues) print(a.dt[Div== l, list(count=.N), HomeTeam][order(-count)][1:5, ])
setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
a.dt[nchar(date)==8, ddate := as.Date(date, '%d/%m/%y')]
a.dt[nchar(date)==10, ddate := as.Date(date, '%d/%m/%Y')]
cat('na date count:', a.dt[is.na(date), .N], '\n')
a.dt <- a.dt[!is.na(ddate), ]
a.dt <- a.dt[, list(div, date=ddate, season, hometeam, awayteam, ftr, b365h, b365d, b365a)]
cat('data summary\n')
summary(a.dt)
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
  a.dt[, list(div, date, season, hometeam, awayteam, actr=ftr, ip=b365h, act=as.numeric(ftr=='H'), ftr='H')],
  a.dt[, list(div, date, season, hometeam, awayteam, actr=ftr, ip=b365d, act=as.numeric(ftr=='D'), ftr='D')],
  a.dt[, list(div, date, season, hometeam, awayteam, actr=ftr, ip=b365a, act=as.numeric(ftr=='A'), ftr='A')]
)
cat('data transposed count with NA', a.dt[, .N], '\n')
a.dt <- na.omit(a.dt)
cat('data transposed count', a.dt[, .N], '\n')
setkey(a.dt, date)
a.dt[, rn := seq(a.dt[, .N])]
a.dt[, gain := act - ip]
all.teams <- unique(c(a.dt[, hometeam], a.dt[, awayteam]))
cat('team count', length(all.teams), '\n')
result_lag <- function(i) {
  for(ateam in all.teams) {
  
    team.dt <- a.dt[(act == 1) & ((hometeam == ateam) | (awayteam == ateam)), ]
    team.dt[(hometeam == ateam) & (ftr == 'H'), r := 'W']
    team.dt[(hometeam == ateam) & (ftr == 'A'), r := 'L']
    team.dt[(awayteam == ateam) & (ftr == 'H'), r := 'L']
    team.dt[(awayteam == ateam) & (ftr == 'A'), r := 'W']
    team.dt[ftr == 'D', r := 'D']
    setkey(team.dt, date)
    team.dt[, trn :=seq(team.dt[, .N])]
    l.team.dt <- team.dt[, list(hometeam, awayteam, pr=r, p_date=date, trn=trn+i)]
    l.team.dt[hometeam == ateam, pv := 'home']
    l.team.dt[awayteam == ateam, pv := 'away']
    l.team.dt[, hometeam := NULL]
    l.team.dt[, awayteam := NULL]
    setkey(team.dt, trn)
    setkey(l.team.dt, trn)
    team.dt <- merge(team.dt, l.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, r, pr, pv, p_date)]
    team.dt[, pd := as.numeric(date - p_date)]
    team.dt[, p_date := NULL]
  
    h.team.dt <- team.dt[hometeam == ateam, list(hometeam, awayteam, date, r, pr, pv, pd)]
    setkey(h.team.dt, date)
    h.team.dt[, trn := seq(h.team.dt[, .N])]
    hl.team.dt <- h.team.dt[, list(hphr=r, hph_date=date, trn=trn+i)]
    setkey(h.team.dt, trn)
    setkey(hl.team.dt, trn)
    h.team.dt <- merge(h.team.dt, hl.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, hpr=pr, hpd=pd, hpv=pv, hphr, hph_date)]
    h.team.dt[, hphd := as.numeric(date - hph_date)]
    h.team.dt[, hph_date := NULL]

    a.team.dt <- team.dt[awayteam == ateam, list(hometeam, awayteam, date, r, pr, pv, pd)]
    setkey(a.team.dt, date)
    a.team.dt[, trn := seq(a.team.dt[, .N])]
    al.team.dt <- a.team.dt[, list(apar=r, apa_date=date, trn=trn+i)]
    setkey(a.team.dt, trn)
    setkey(al.team.dt, trn)
    a.team.dt <- merge(a.team.dt, al.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, apr=pr, apd=pd, apv=pv, apar, apa_date)]
    a.team.dt[, apad := as.numeric(date - apa_date)]
    a.team.dt[, apa_date := NULL]
 
    if(exists("h.teams.dt")) {
      h.teams.dt <- rbind(h.teams.dt, h.team.dt)
    } else {
      h.teams.dt <- h.team.dt
      print(h.teams.dt)
    }
    if(exists("a.teams.dt")) {
      a.teams.dt <- rbind(a.teams.dt, a.team.dt)
    } else {
      a.teams.dt <- a.team.dt
      print(a.teams.dt)
    }
  }
  cat('data count pre unique', a.dt[, .N], '\n')
  setkey(a.dt, date, hometeam, awayteam)
  a.dt <- unique(a.dt)
  cat('data count post unique', a.dt[, .N], '\n')

  cat('home count pre unique', a.teams.dt[, .N], '\n')
  setkey(h.teams.dt, date, hometeam, awayteam)
  h.teams.dt <- unique(h.teams.dt)
  cat('home count post unique', a.teams.dt[, .N], '\n')

  cat('away count pre unique', a.teams.dt[, .N], '\n')
  setkey(a.teams.dt, date, hometeam, awayteam)
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
  ivars <-c("hpr", "apr", "hpp", "app", "hpd", "apd", "hphr", "hphd", "hphp", "apar", "apad", "apap")
  setnames(a.dt, ivars, paste0(ivars, i))
  rm(h.teams.dt)
  rm(a.teams.dt)
  a.dt
}
lag_result_i <- 1:5
for (i in lag_result_i) a.dt <- result_lag(i)
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
a.dt[, gain := round(gain, 3)]
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
a.dt <- na.omit(a.dt)
cat('data count no missings', a.dt[, .N], '\n')
cat('data summary', '\n')
summary(a.dt)
# error with odds
cat('data count odds error', a.dt[, .N], '\n')
a.dt[gain == -Inf, ]
a.dt <- a.dt[gain != -Inf, ]
cat('data count no odds error', a.dt[, .N], '\n')

setkey(a.dt, rn)
if(!file.exists('~/data/R/rds/')) dir.create('~/data/R/rds/')
saveRDS(a.dt, '~/data/R/rds/a.dt.rds')

