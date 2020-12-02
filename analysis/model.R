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
  'hpr1',
  'apr1',
  'hpr2',
  'apr2',
  'hpp1',
  'app1',
  'hpp1',
  'app2'
)
yvar <- 'gain'
formula <- as.formula(paste(yvar, paste(xvar, collapse="+"), sep="~"))

# model params
train.fraction <- 0.7
n.trees <- 500
shrinkage <- 0.01
interaction.depth <- 4
cat("model params\n",
  "train.fraction", train.fraction, "\n",
  "n.trees", n.trees, "\n",
  "shrinkage", shrinkage, "\n",
  "interaction.depth", interaction.depth, "\n",
  "\n", sep=""
)

# build model
model <- gbm(
  formula=formula,
  data=train.dt,
  distribution="gaussian",
  train.fraction=train.fraction,
  n.trees=n.trees,
  shrinkage=shrinkage,
  interaction.depth=interaction.depth,
  keep.data=FALSE,
  verbose=TRUE
)

# model summary
cat("gbm perf\n")
gbm.perf(model, plot.it=FALSE)
cat("gbm summary\n")
summary(model, plotit=FALSE)

# score
train.dt[, gbmp := predict(model, train.dt)]
test.dt[, gbmp := predict(model, test.dt)]

# rebalance
cat("train pre-balance act=gain, pred=gbmp\n")
train.dt[, list(gain=sum(gain), gbmp=sum(gbmp))]
cat("test pre-balance act=gain, pred=gbmp\n")
test.dt[, list(gain=sum(gain), gbmp=sum(gbmp))]
balance_factor <- sum(train.dt[, gain]) / sum(train.dt[, gbmp])
train.dt[, gbmp := gbmp * balance_factor]
test.dt[, gbmp := gbmp * balance_factor]
cat("train post-balance act=gain, pred=gbmp\n")
train.dt[, list(gain=sum(gain), gbmp=sum(gbmp))]
cat("test post-balance act=gain, pred=gbmp\n")
test.dt[, list(gain=sum(gain), gbmp=sum(gbmp))]

# act pred summary
cat("summary train act=gain, pred=gbmp\n")
summary(train.dt[, list(gain, gbmp)])
cat("summary test act=gain, pred=gbmp\n")
summary(test.dt[, list(gain, gbmp)])

# postiive model prediciton
cat("train positive prediction count ", nrow(train.dt[gbmp > 0, ]), "\n")
cat("train positive prediction\n")
train.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
cat("test positive prediction count ", nrow(test.dt[gbmp > 0, ]), "\n")
cat("test positive prediction\n")
test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]

