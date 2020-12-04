.libPaths("~/data/R/packages")
suppressPackageStartupMessages({
  library("data.table")
  library("gbm")
  library("ggplot2")
  library("gridExtra")
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
  "hpr2",
  "apr2",
  "hpp2",
  "app2",
  "hpr3",
  "apr3",
  "hpp3",
  "app3"
)
yvar <- "gain"
uvar <- unique(c("date", "season", "hometeam", "awayteam", xvar))
formula <- as.formula(paste(yvar, paste(xvar, collapse="+"), sep="~"))

# model params
train.fraction <- 0.7
n.trees <- 25
shrinkage <- 0.01
interaction.depth <- 4

# print to log file
if(!file.exists("logs")) dir.create("logs")
sink("logs/model.log")

# model params
cat(
  "model params\n",
  "train.fraction:", train.fraction, "\n",
  "n.trees:", n.trees, "\n",
  "shrinkage:", shrinkage, "\n",
  "interaction.depth:", interaction.depth, "\n", sep=""
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
best.trees <- gbm.perf(model, plot.it=FALSE, method="test")
cat(paste("gbm perf best.trees=", best.trees, "\n"))
cat("gbm summary\n")
summary(model, plotit=FALSE)

# score
# help(gbm.perf)
train.dt[, gbmp := predict(model, train.dt, best.trees)]
test.dt[, gbmp := predict(model, test.dt, best.trees)]

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

# deviances
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
cat("train.a mean null dev", mean(train.a.dt[, null_dev]), "\n")
cat("train.b mean null dev", mean(train.b.dt[, null_dev]), "\n")
cat("train.a mean model dev", mean(train.a.dt[, model_dev]), "\n")
cat("train.b mean model dev", mean(train.b.dt[, model_dev]), "\n")
cat("test mean null dev", mean(test.dt[, null_dev]), "\n")
cat("test mean model dev", mean(test.dt[, model_dev]), "\n")

# act pred summary
cat("summary train act=gain, pred=gbmp\n")
summary(train.dt[, list(gain, gbmp)])
cat("summary test act=gain, pred=gbmp\n")
summary(test.dt[, list(gain, gbmp)])

# positive model prediciton
train.ppc <- train.dt[gbmp > 0, .N]
cat("train positive prediction count ", train.ppc, "\n")
if(train.ppc > 0) {
  cat("train positive prediction\n")
  train.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
  cat("train gain", sum(train.dt[gbmp>0, gain]), "\n")
  cat("train mean gain", mean(train.dt[gbmp>0, gain]), "\n")
}
test.ppc <- test.dt[gbmp > 0, .N]
cat("test positive prediction count ", test.ppc, "\n")
if(test.ppc > 0) {
  cat("test positive prediction\n")
  test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
  cat("test gain", sum(test.dt[gbmp>0, gain]), "\n")
  cat("test mean gain", mean(test.dt[gbmp>0, gain]), "\n")
}

# plots
plot.model(model, train.a.dt, train.b.dt, train.dt, uvar)

sink()

