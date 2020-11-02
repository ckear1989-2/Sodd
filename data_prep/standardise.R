.libPaths('~/data/R/packages/')
.libPaths()
library("data.table")

alist <- lapply(
  c(
    '~/data/2021/E0.csv',
    '~/data/1920/E0.csv',
    '~/data/1819/E0.csv',
    '~/data/1718/E0.csv',
    '~/data/1617/E0.csv',
    '~/data/1516/E0.csv',
    '~/data/1415/E0.csv',
    '~/data/1314/E0.csv',
    '~/data/1213/E0.csv',
    '~/data/1112/E0.csv',
    '~/data/1011/E0.csv',
    '~/data/2021/E1.csv',
    '~/data/1920/E1.csv',
    '~/data/1819/E1.csv',
    '~/data/1718/E1.csv',
    '~/data/1617/E1.csv',
    '~/data/1516/E1.csv',
    '~/data/1415/E1.csv',
    '~/data/1314/E1.csv',
    '~/data/1213/E1.csv',
    '~/data/1112/E1.csv',
    '~/data/1011/E1.csv'
  ),
  fread
)
a.dt <- rbindlist(alist, fill=TRUE)
a.dt[HomeTeam=="Brentford" & AwayTeam=="Blackburn", ]
# colnames(a.dt)
a.dt[Div=='E0', ]
a.dt[Div=='E1', ]
setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
a.dt[nchar(date)==8, ddate := as.Date(date, '%d/%m/%y')]
a.dt[nchar(date)==10, ddate := as.Date(date, '%d/%m/%Y')]
print(paste0('na date count:', a.dt[is.na(date), .N]))
a.dt <- a.dt[!is.na(ddate), ]
summary(a.dt[, ddate])
a.dt <- a.dt[, list(div, date=ddate, hometeam, awayteam, ftr, b365h, b365d, b365a)]
summary(a.dt)
a.dt[is.na(hometeam), ]
a.dt[hometeam == '', ]
print(a.dt[, .N])
a.dt <- na.omit(a.dt)
print(a.dt[, .N])
a.dt[, b365h := 1 / b365h]
a.dt[, b365d := 1 / b365d]
a.dt[, b365a := 1 / b365a]
a.dt[, sip := b365h + b365d + b365a]
# head(a.dt)
a.dt <- rbind(
  a.dt[, list(div, date, hometeam, awayteam, actr=ftr, ip=b365h, act=as.numeric(ftr=='H'), ftr='H')],
  a.dt[, list(div, date, hometeam, awayteam, actr=ftr, ip=b365d, act=as.numeric(ftr=='D'), ftr='D')],
  a.dt[, list(div, date, hometeam, awayteam, actr=ftr, ip=b365a, act=as.numeric(ftr=='A'), ftr='A')]
)
print(a.dt[, .N])
a.dt <- na.omit(a.dt)
print(a.dt[, .N])
setkey(a.dt, date)
a.dt[, rn := seq(a.dt[, .N])]
a.dt[, gain := act - ip]
for(ateam in unique(a.dt[, hometeam])) {

  team.dt <- a.dt[(act == 1) & ((hometeam == ateam) | (awayteam == ateam)), ]
  team.dt[(hometeam == ateam) & (ftr == 'H'), r := 'W']
  team.dt[(hometeam == ateam) & (ftr == 'A'), r := 'L']
  team.dt[(awayteam == ateam) & (ftr == 'H'), r := 'L']
  team.dt[(awayteam == ateam) & (ftr == 'A'), r := 'W']
  team.dt[ftr == 'D', r := 'D']

  h.team.dt <- team.dt[hometeam == ateam, list(hometeam, awayteam, date, r)]
  setkey(h.team.dt, date)
  h.team.dt[, trn := seq(h.team.dt[, .N])]
  hl.team.dt <- h.team.dt[, list(hpr=r, trn=trn+1)]
  setkey(h.team.dt, trn)
  setkey(hl.team.dt, trn)
  h.team.dt <- merge(h.team.dt, hl.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, hpr)]

  a.team.dt <- team.dt[awayteam == ateam, list(hometeam, awayteam, date, r)]
  setkey(a.team.dt, date)
  a.team.dt[, trn := seq(a.team.dt[, .N])]
  al.team.dt <- a.team.dt[, list(apr=r, trn=trn+1)]
  setkey(a.team.dt, trn)
  setkey(al.team.dt, trn)
  a.team.dt <- merge(a.team.dt, al.team.dt, all.x=TRUE, all.y=FALSE)[, list(hometeam, awayteam, date, apr)]

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
print(colnames(a.dt,))
setkey(a.dt, date, hometeam, awayteam)
print(a.dt[, .N])
a.dt <- unique(a.dt)
print(a.dt[, .N])
print(colnames(h.teams.dt,))
setkey(h.teams.dt, date, hometeam, awayteam)
print(h.teams.dt[, .N])
h.teams.dt <- unique(h.teams.dt)
print(h.teams.dt[, .N])
print(colnames(a.teams.dt,))
setkey(a.teams.dt, date, hometeam, awayteam)
print(a.teams.dt[, .N])
a.teams.dt <- unique(a.teams.dt)
print(a.teams.dt[, .N])
a.dt <- merge(a.dt, h.teams.dt)
a.dt <- merge(a.dt, a.teams.dt)
print(head(a.dt))
a.dt[, .N, hpr]
a.dt[, .N, apr]
# set character variables to factor for modeling
a.dt[, hometeam := as.factor(hometeam)]
a.dt[, awayteam := as.factor(awayteam)]
a.dt[, div := as.factor(ftr)]
a.dt[, ftr := as.factor(ftr)]
a.dt[, hpr := as.factor(hpr)]
a.dt[, apr := as.factor(apr)]
print(a.dt[, .N])
a.dt <- na.omit(a.dt)
print(a.dt[, .N])
summary(a.dt)
a.dt[gain == -Inf, ]
a.dt <- a.dt[gain != -Inf, ]

setkey(a.dt, rn)
if(!file.exists('~/data/R/rds/')) dir.create('~/data/R/rds/')
saveRDS(a.dt, '~/data/R/rds/a.dt.rds')

