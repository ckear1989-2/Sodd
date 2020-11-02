.libPaths('~/data/R/packages')
library('data.table')
library('gbm')

set.seed(123)
a.dt <- readRDS('~/data/R/rds/a.dt.rds')
train.dt <- a.dt[date < as.Date('2020/10/01', '%Y/%m/%d'), ]
test.dt <- a.dt[date >= as.Date('2020/10/01', '%Y/%m/%d'), ]
xvar <- c(
  'hometeam',
  'awayteam',
  'div',
  'ftr',
  'ip',
  'hpr',
  'apr'
)
yvar <- 'gain'
formula <- as.formula(paste(yvar, paste(xvar, collapse="+"), sep="~"))
model <- gbm(
  formula=formula,
  data=train.dt,
  distribution="gaussian",
  train.fraction=0.7,
  n.trees=500,
  shrinkage=0.01,
  interaction.depth=3,
  keep.data=FALSE,
  verbose=TRUE
)
gbm.perf(model, plot.it=FALSE)
summary(model, plotit=FALSE)
train.dt[, gbmp := predict(model, train.dt)]
test.dt[, gbmp := predict(model, test.dt)]
train.dt[, list(gain=sum(gain), egain=sum(gbmp))]
test.dt[, list(gain=sum(gain), egain=sum(gbmp))]
train.dt[, gbmp := gbmp * sum(train.dt[, gain]) / sum(train.dt[, gbmp])]
test.dt[, gbmp := gbmp * sum(train.dt[, gain]) / sum(train.dt[, gbmp])]
train.dt[, list(gain=sum(gain), egain=sum(gbmp))]
test.dt[, list(gain=sum(gain), egain=sum(gbmp))]
summary(train.dt[, list(gain, gbmp)])
summary(test.dt[, list(gain, gbmp)])

summary(train.dt[, gbmp])
summary(test.dt[, gbmp])
test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
test.dt[
  gbmp>0,
  list(
    ftr=as.list(ftr),
    actr=as.list(actr),
    ip=as.list(ip),
    gbmp=as.list(gbmp),
    gain=as.list(gain)
  ),
  by=list(date, hometeam, awayteam)
]

test.dt[gbmp>0, list(gain=sum(gain), egain=sum(gbmp))]
