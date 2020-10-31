.libPaths('~/data/R/packages')
library('data.table')
library('gbm')

set.seed(123)
a.dt <- readRDS('~/data/R/rds/a.dt.rds')
train.dt <- a.dt[ddate < as.Date('2020/10/01', '%Y/%m/%d'), ]
test.dt <- a.dt[ddate >= as.Date('2020/10/01', '%Y/%m/%d'), ]
model <- gbm(as.formula('gain ~ hometeam + awayteam + ftr + ip'),
  data=train.dt, distribution="gaussian", train.fraction=0.7, n.trees=500, keep.data=FALSE, verbose=TRUE)
train.dt[, gbmp := predict(model, train.dt)]
test.dt[, gbmp := predict(model, test.dt)]
train.dt[, list(gain=sum(gain), egain=sum(gbmp))]
test.dt[, list(gain=sum(gain), egain=sum(gbmp))]
train.dt[, gbmp := gbmp * sum(train.dt[, gain]) / sum(train.dt[, gbmp])]
test.dt[, gbmp := gbmp * sum(train.dt[, gain]) / sum(train.dt[, gbmp])]
train.dt[, list(gain=sum(gain), egain=sum(gbmp))]
test.dt[, list(gain=sum(gain), egain=sum(gbmp))]

summary(train.dt[, gbmp])
summary(test.dt[, gbmp])
test.dt[gbmp>0, ]
test.dt[gbmp>0, list(gain=sum(gain), egain=sum(gbmp))]
