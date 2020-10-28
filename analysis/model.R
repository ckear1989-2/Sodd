.libPaths('~/data/R/packages')
library('data.table')
library('gbm')

set.seed(123)

a.dt <- readRDS('~/data/R/rds/a.dt.rds')
train.dt <- a.dt[rn < 0.7, ][order(rn)]
head(train.dt)
test.dt <- a.dt[rn >=0.7]
head(train.dt)
model <- gbm(as.formula('gain ~ hometeam + awayteam + ftr'), data=train.dt, train.fraction=0.7, n.trees=500, keep.data=FALSE, verbose=TRUE)
train.dt[, gbmp := predict(model, train.dt)]
test.dt[, gbmp := predict(model, test.dt)]
train.dt[, list(gain=sum(gain), egain=sum(gbmp))]
test.dt[, list(gain=sum(gain), egain=sum(gbmp))]
head(train.dt)

