.libPaths('~/data/R/packages')
library('data.table')
library('gbm')
library('ggplot2')

set.seed(123)
a.dt <- readRDS('~/data/R/rds/a.dt.rds')
train.dt <- a.dt[date < as.Date('2020/10/01', '%Y/%m/%d'), ]
test.dt <- a.dt[date >= as.Date('2020/10/01', '%Y/%m/%d'), ]
xvar <- c(
  "ip",
  'div',
  "season",
  'ftr',
  'hpr1',
  'apr1',
  'hpp1',
  'app1',
  'hpr2',
  'apr2',
  'hpp2',
  'app2',
  'hpr3',
  'apr3',
  'hpp3',
  'app3'
)
yvar <- 'gain'
uvar <- unique(c("date", "ip", "hometeam", "awayteam", xvar))
formula <- as.formula(paste(yvar, paste(xvar, collapse="+"), sep="~"))

# model params
train.fraction <- 0.7
n.trees <- 200
shrinkage <- 0.01
interaction.depth <- 4
cat(
  "model params\n",
  "train.fraction:", train.fraction, "\n",
  "n.trees:", n.trees, "\n",
  "shrinkage:", shrinkage, "\n",
  "interaction.depth:", interaction.depth, "\n"
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

# deviances
train.a.rows <- floor(train.dt[, .N] *train.fraction)
train.mean <- mean(train.dt[, gain])
train.dt[, mean_pred := train.mean]
train.dt[, null_dev:= ((gain - mean_pred) ** 2)]
train.dt[, model_dev:= ((gain - gbmp) ** 2)]
test.dt[, mean_pred := train.mean]
test.dt[, null_dev:= ((gain - mean_pred) ** 2)]
test.dt[, model_dev:= ((gain - gbmp) ** 2)]

# cat(sum(train.dt[, null_dev]))
# cat('train.a.rows', train.a.rows, '\n')
# cat('train.mean', train.mean, '\n')
# cat('train mean null dev', mean(train.dt[, null_dev]), '\n')
cat('train.a mean null dev', mean(train.dt[1:train.a.rows, null_dev]), '\n')
cat('train.b mean null dev', mean(train.dt[train.a.rows:train.dt[, .N], null_dev]), '\n')
# cat('train mean model dev', mean(train.dt[, model_dev]), '\n')
cat('train.a mean model dev', mean(train.dt[1:train.a.rows, model_dev]), '\n')
cat('train.b mean model dev', mean(train.dt[train.a.rows:train.dt[, .N], model_dev]), '\n')
cat('test mean null dev', mean(train.dt[, null_dev]), '\n')
cat('test mean model dev', mean(train.dt[, model_dev]), '\n')

# act pred summary
cat("summary train act=gain, pred=gbmp\n")
summary(train.dt[, list(gain, gbmp)])
cat("summary test act=gain, pred=gbmp\n")
summary(test.dt[, list(gain, gbmp)])

# positive model prediciton
cat("train positive prediction count ", nrow(train.dt[gbmp > 0, ]), "\n")
cat("train positive prediction\n")
train.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
cat("train gain", sum(train.dt[gbmp>0, gain]), "\n")
cat("train mean gain", mean(train.dt[gbmp>0, gain]), "\n")
cat("test positive prediction count ", nrow(test.dt[gbmp > 0, ]), "\n")
cat("test positive prediction\n")
test.dt[gbmp>0, list(date, hometeam, awayteam, ftr, actr, ip, gbmp, gain)][order(-gbmp)]
cat("test gain", sum(test.dt[gbmp>0, gain]), "\n")
cat("test mean gain", mean(test.dt[gbmp>0, gain]), "\n")

# univariates
univariate <- function(a.dt, x) {
  setnames(a.dt, x, "x")
  summary.dt <- a.dt[, list(act=sum(gain), pred=sum(gbmp), count=.N), x][order(-count)]
  setnames(a.dt, "x", x)
  summary.dt[, act := act / count]
  summary.dt[, pred := pred / count]
  if(is.numeric(summary.dt[, x])) setkey(summary.dt, x)
  summary.dt[, xn := seq(summary.dt[, .N])]
  row_count <- summary.dt[, .N]
  if(row_count > 100) {
    # other.dt <- summary.dt[xn > 100, ]
    # other.dt[, x := "OTHER"]
    # other.dt <- other.dt[, list(act=sum(act), pred=sum(pred), count=sum(count), xn=101), x]
    # other.dt[, act := act / count]
    # other.dt[, pred := pred / count]
    # summary.dt <- rbind(summary.dt[xn <= 100, ], other.dt)
    summary.dt <- summary.dt[xn <=100, ]
  }
  balance_factor <- min(summary.dt[, act]) / max(summary.dt[, count])
  summary.dt[, act_rs := act * (1 / balance_factor)]
  summary.dt[, pred_rs := pred * (1 / balance_factor)]
  # print(summary.dt)
  # print(balance_factor)
  plot.obj <- ggplot(summary.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=xn, y=count), stat="identity", fill="yellow", color="yellow", alpha=0.3)
  plot.obj <- plot.obj + geom_line(aes(x=xn, y=act_rs), stat="identity", color="red")
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=act_rs), stat="identity", color="red")
  plot.obj <- plot.obj + geom_line(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  plot.obj <- plot.obj + scale_y_continuous(
    name="count",
    sec.axis=sec_axis(~.*balance_factor, name="act, pred")
  )
  breaks <- summary.dt[, xn]
  labels <- summary.dt[, x]
  if(row_count >50) {
    breaks <- breaks[seq(1, row_count, 5)]
    labels <- labels[seq(1, row_count, 5)]
  }
  plot.obj <- plot.obj + scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj <- plot.obj + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  plot.obj <- plot.obj + ggtitle(x)
  plot.obj <- plot.obj + xlab(x)
  print(plot.obj)
}
pdf("model_output.pdf")
  for (x in uvar) {
    univariate(train.dt, x)
    univariate(test.dt, x)
  }
dev.off()

