.libPaths('~/data/R/packages/')
.libPaths()
library("data.table")

a.dt <- fread('~/data/2021/E0.csv')
# head(a.dt)
# colnames(a.dt)
setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
a.dt <- a.dt[, list(div, date, time, hometeam, awayteam, ftr, b365h, b365d, b365a)]
a.dt[, b365h := 1 / b365h]
a.dt[, b365d := 1 / b365d]
a.dt[, b365a := 1 / b365a]
a.dt[, sip := b365h + b365d + b365a]
# head(a.dt)
a.dt <- rbind(
  a.dt[, list(div, date, time, hometeam, awayteam, ftr, ip=b365h, act=as.numeric(ftr=='H'))],
  a.dt[, list(div, date, time, hometeam, awayteam, ftr, ip=b365d, act=as.numeric(ftr=='D'))],
  a.dt[, list(div, date, time, hometeam, awayteam, ftr, ip=b365a, act=as.numeric(ftr=='A'))]
)
a.dt[, gain := act - ip]
# head(a.dt)
# a.dt[, list(count=.N, ip=sum(ip), act=sum(act), gain=sum(gain)), ftr]
# a.dt[, list(count=.N, ip=sum(ip), act=sum(act), gain=sum(gain)), date]

dir.create('~/data/R/rds/')
saveRDS(a.dt, '~/data/R/rds/a.dt.rds')

