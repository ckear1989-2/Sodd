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
  
    h.team.dt <- team.dt[hometeam == ateam, list(hometeam, awayteam, date, r)]
    setkey(h.team.dt, date)
    h.team.dt[, trn := seq(h.team.dt[, .N])]
    hl.team.dt <- h.team.dt[, list(hpr=r, hp_date=date, trn=trn+i)]
    setkey(h.team.dt, trn)
    setkey(hl.team.dt, trn)
    # print(h.team.dt)
    # print(hl.team.dt)
    h.team.dt <- merge(h.team.dt, hl.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, hpr, hp_date)]
    h.team.dt[, hpd := as.numeric(date - hp_date)]
    # print(h.team.dt)
    # q()

    a.team.dt <- team.dt[awayteam == ateam, list(hometeam, awayteam, date, r)]
    setkey(a.team.dt, date)
    a.team.dt[, trn := seq(a.team.dt[, .N])]
    al.team.dt <- a.team.dt[, list(apr=r, ap_date=date, trn=trn+i)]
    setkey(a.team.dt, trn)
    setkey(al.team.dt, trn)
    a.team.dt <- merge(a.team.dt, al.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, apr, ap_date)]
    a.team.dt[, apd := as.numeric(date - ap_date)]
    # print(a.team.dt)
 
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

  a.dt[, hpp := -1]
  a.dt[hpr == "L", hpp := 0]
  a.dt[hpr == "D", hpp := 1]
  a.dt[hpr == "W", hpp := 3]
  a.dt[, app := -1]
  a.dt[apr == "L", app := 0]
  a.dt[apr == "D", app := 1]
  a.dt[apr == "W", app := 3]
  a.dt[, hpr := as.factor(hpr)]
  a.dt[, apr := as.factor(apr)]
  setnames(a.dt, c("hpr", "apr", "hpp", "app", "hpd", "apd"), paste0(c("hpr", "apr", "hpp", "app", "hpd", "apd"), i))
  rm(h.teams.dt)
  rm(a.teams.dt)
  a.dt
}
lag_result_i <- 1:3
for (i in lag_result_i) a.dt <- result_lag(i)
# print(head(a.dt))
a.dt[, .N, hpr1]
a.dt[, .N, apr1]
a.dt[, .N, hpr2]
a.dt[, .N, apr2]
# set character variables to factor for modeling
a.dt[, hometeam := as.factor(hometeam)]
a.dt[, awayteam := as.factor(awayteam)]
a.dt[, season := as.factor(season)]
a.dt[, div := as.factor(div)]
a.dt[, ftr := as.factor(ftr)]
a.dt[, ip := round(ip, 3)]
a.dt[, gain := round(gain, 3)]
cat('data count with missings', a.dt[, .N], '\n')
a.dt[is.na(apr1), apr1 := "M"]
a.dt[is.na(hpr1), hpr1 := "M"]
a.dt[is.na(apr2), apr2 := "M"]
a.dt[is.na(hpr2), hpr2 := "M"]
a.dt[is.na(apr3), apr3 := "M"]
a.dt[is.na(hpr3), hpr3 := "M"]
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

