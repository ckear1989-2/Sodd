.libPaths("~/data/R/packages")
suppressPackageStartupMessages({
  library("data.table")
  library("gbm")
  library("ggplot2")
  library("gridExtra")
  library("resample")
})
source("analysis/plot.R")

# TODO
# model performance polots
# plotting functions separate file

set.seed(123)
a.dt <- readRDS("~/data/R/rds/a.dt.rds")
train.dt <- a.dt[date < as.Date("2020/10/01", "%Y/%m/%d"), ]
test.dt <- a.dt[date >= as.Date("2020/10/01", "%Y/%m/%d"), ]
xvar <- c(
  "ip",
  "div",
  "ftr",
  "hpr1",
  "apr1",
  "hpp1",
  "app1",
  "hpd1",
  "apd1",
  "hpr2",
  "apr2",
  "hpp2",
  "app2",
  "hpd2",
  "apd2",
  "hpr3",
  "apr3",
  "hpp3",
  "app3",
  "hpd3",
  "apd3"
)
yvar <- "gain"
uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
formula <- as.formula(paste(yvar, paste(xvar, collapse="+"), sep="~"))

# model params
train.fraction <- 0.7
n.trees <- 15
shrinkage <- 0.01
interaction.depth <- 4

# print to log file
if(!file.exists("logs")) dir.create("logs")
sink("logs/model.log")

# model params
cat0n(rep("#", 30))
cat0n(
  "model params\n",
  "train.fraction:", train.fraction, "\n",
  "n.trees:", n.trees, "\n",
  "shrinkage:", shrinkage, "\n",
  "interaction.depth:", interaction.depth
)

# build model
cat0n(rep("#", 30))
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
cat0n(rep("#", 30))
best.trees <- gbm.perf(model, plot.it=FALSE, method="test")
cat0n("gbm perf best.trees=", best.trees)
cat0n("gbm summary")
summary(model, plotit=FALSE)

# score
train.dt[, gbmp := predict(model, train.dt, best.trees)]
test.dt[, gbmp := predict(model, test.dt, best.trees)]

# rebalance
cat0n(rep("#", 30))
cat0n("train pre-balance act, pred")
train.dt[, list(act=sum(gain), pred=sum(gbmp))]
cat0n("test pre-balance act, pred")
test.dt[, list(act=sum(gain), pred=sum(gbmp))]
balance_factor <- sum(train.dt[, gain]) / sum(train.dt[, gbmp])
train.dt[, gbmp := gbmp * balance_factor]
test.dt[, gbmp := gbmp * balance_factor]
cat0n("train post-balance act, pred")
train.dt[, list(act=sum(gain), pred=sum(gbmp))]
cat0n("test post-balance act, pred")
test.dt[, list(act=sum(gain), pred=sum(gbmp))]

# deviances
cat0n(rep("#", 30))
train.a.rows <- floor(train.dt[, .N] *train.fraction)
train.mean <- mean(train.dt[, gain])
train.dt[, mean_pred := train.mean]
train.dt[, null_dev:= ((gain - mean_pred) ** 2)]
train.dt[, model_dev:= ((gain - gbmp) ** 2)]
test.dt[, mean_pred := train.mean]
test.dt[, null_dev:= ((gain - mean_pred) ** 2)]
test.dt[, model_dev:= ((gain - gbmp) ** 2)]
train.a.dt <- train.dt[1:train.a.rows, ]
train.b.dt <- train.dt[train.a.rows:train.dt[, .N], ]
cat0n("train.a mean null dev=", mean(train.a.dt[, null_dev]))
cat0n("train.a mean model dev=", mean(train.a.dt[, model_dev]))
cat0n("train.b mean null dev=", mean(train.b.dt[, null_dev]))
cat0n("train.b mean model dev=", mean(train.b.dt[, model_dev]))
cat0n("test mean null dev=", mean(test.dt[, null_dev]))
cat0n("test mean model dev=", mean(test.dt[, model_dev]))

# act pred summary
cat0n(rep("#", 30))
cat0n("summary train act, pred")
summary(train.dt[, list(act=gain, pred=gbmp)])
cat0n("summary test act, pred")
summary(test.dt[, list(act=gain, pred=gbmp)])

# positive model prediciton
cat0n(rep("#", 30))
train.ppc <- train.dt[gbmp > 0, .N]
cat0n("train positive prediction count ", train.ppc)
if(train.ppc > 0) {
  cat0n("train positive prediction")
  train.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
  cat0n("train gain", sum(train.dt[gbmp>0, gain]))
  cat0n("train mean gain", mean(train.dt[gbmp>0, gain]))
}
test.ppc <- test.dt[gbmp > 0, .N]
cat0n("test positive prediction count ", test.ppc)
if(test.ppc > 0) {
  cat0n("test positive prediction")
  test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
  cat0n("test gain", sum(test.dt[gbmp>0, gain]))
  cat0n("test mean gain", mean(test.dt[gbmp>0, gain]))
}

# plots
plot.model(model, train.a.dt, train.b.dt, train.dt, uvar)

sink()

