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
    '~/data/1415/E0.csv'
  ),
  fread
)
a.dt <- rbindlist(alist, fill=TRUE)
# head(a.dt)
# colnames(a.dt)
setnames(a.dt, colnames(a.dt), tolower(colnames(a.dt)))
# class(a.dt[, date])
# a.dt[1:10, date]
# a.dt[, .N, nchar(date)]
# a.dt[nchar(date) == 8, date := paste0(strsplit(date, "/")[[1]][[1]], "/", strsplit(date, "/")[[1]][[2]], "/20", strsplit(date, "/")[[1]][[3]]), list(date, hometeam, awayteam)]
a.dt[nchar(date)==8, ddate := as.Date(date, '%d/%m/%y')]
a.dt[nchar(date)==10, ddate := as.Date(date, '%d/%m/%Y')]
# class(a.dt[, date])
# a.dt[1:10, date]
# a.dt[is.na(date), .N]
# a.dt[, list(date, rn)]
summary(a.dt[, date])
summary(a.dt[, ddate])
a.dt[, list(date, ddate)]
a.dt <- a.dt[, list(div, date, ddate, time, hometeam, awayteam, ftr, b365h, b365d, b365a)]
a.dt[, b365h := 1 / b365h]
a.dt[, b365d := 1 / b365d]
a.dt[, b365a := 1 / b365a]
a.dt[, sip := b365h + b365d + b365a]
# head(a.dt)
a.dt <- rbind(
  a.dt[, list(div, date, ddate, time, hometeam, awayteam, ftr, ip=b365h, act=as.numeric(ftr=='H'))],
  a.dt[, list(div, date, ddate, time, hometeam, awayteam, ftr, ip=b365d, act=as.numeric(ftr=='D'))],
  a.dt[, list(div, date, ddate, time, hometeam, awayteam, ftr, ip=b365a, act=as.numeric(ftr=='A'))]
)
setkey(a.dt, ddate)
a.dt[, rn := seq(a.dt[, .N])]
a.dt[, gain := act - ip]
# head(a.dt)
#class(a.dt[, hometeam])
a.dt[, hometeam := as.factor(hometeam)]
a.dt[, awayteam := as.factor(awayteam)]
a.dt[, ftr := as.factor(ftr)]
# a.dt[, list(count=.N, ip=sum(ip), act=sum(act), gain=sum(gain)), ftr]
# a.dt[, list(count=.N, ip=sum(ip), act=sum(act), gain=sum(gain)), date]

# dir.create('~/data/R/rds/')
saveRDS(a.dt, '~/data/R/rds/a.dt.rds')

