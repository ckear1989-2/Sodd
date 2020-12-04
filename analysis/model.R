.libPaths("~/data/R/packages")
suppressPackageStartupMessages({
  library("data.table")
  library("gbm")
  library("ggplot2")
  library("gridExtra")
})

# TODO
# use n.trees to score
# y label cut off plots
# rescaling ip bug

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
n.trees <- 250
shrinkage <- 0.01
interaction.depth <- 4
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
cat("gbm perf\n")
gbm.perf(model, plot.it=FALSE)
cat("gbm summary\n")
summary(model, plotit=FALSE)

# score
n.trees <- gbm.perf(model, plot.it=FALSE)
# help(gbm.perf)
train.dt[, gbmp := predict(model, train.dt, n.trees=n.trees)]
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
    message <- paste("plotting top 100 of", summary.dt[, .N], "levels")
    summary.dt <- summary.dt[xn <= 100, ]
  }
  # balance_factor <- min(summary.dt[, act]) / max(summary.dt[, count])
  max_count <- max(summary.dt[!is.na(count), count])
  max_y <- max(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  max_abs_y <- max(abs(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred])))
  min_y <- min(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  range_y <- max_y - min_y
  summary.dt[, act_rs := ((act + max_abs_y) / (range_y)) * max_count]
  summary.dt[, pred_rs := ((pred + max_abs_y) / (range_y)) * max_count]
  if(range_y == 0) summary.dt[, act_rs := max_count / 2]
  # summary.dt[, act_rs := act * (1 / balance_factor)]
  # summary.dt[, pred_rs := pred * (1 / balance_factor)]
  plot.obj <- ggplot(summary.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=xn, y=count), stat="identity", fill="yellow", color="yellow", alpha=0.3)
  if(row_count > 1) {
    plot.obj <- plot.obj + geom_line(aes(x=xn, y=act_rs), stat="identity", color="red")
    plot.obj <- plot.obj + geom_line(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  }
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=act_rs), stat="identity", color="red")
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  if(range_y > 0) {
    plot.obj <- plot.obj + scale_y_continuous(
      name="count",
      sec.axis=sec_axis(~ ((. / max_count * range_y) - max_abs_y), name="act, pred")
    )
  } else {
    plot.obj <- plot.obj + scale_y_continuous(
      name="count",
      sec.axis=sec_axis(~ . / max_count * max_y, name="act, pred")
    )
  }
  # plot.obj <- plot.obj + scale_y_continuous(
  #   name="count",
  #   sec.axis=sec_axis(~.*balance_factor, name="act, pred")
  # )
  breaks <- summary.dt[, xn]
  labels <- summary.dt[, x]
  if(row_count > 50) {
    breaks <- breaks[seq(1, row_count, 5)]
    labels <- labels[seq(1, row_count, 5)]
  }
  plot.obj <- plot.obj + scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj <- plot.obj + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5), plot.title=element_text(hjust=0.5))
  plot.obj <- plot.obj + ggtitle(paste(deparse(substitute(a.dt)), x))
  plot.obj <- plot.obj + xlab(x)
  if(row_count > 100) plot.obj <- plot.obj + annotate("text", x=15, y=max(summary.dt[, count]), size=4, label=message)
  plot.obj
}
partial.plot <- function(model, x, a.dt) {
  if (x %in% xvar) {
    x.dt <- data.table(plot(model, x, return.grid=TRUE))
    setnames(x.dt, x, "x")
    setnames(a.dt, x, "x")
    summary.dt <- a.dt[, list(count=.N), x]
    setnames(a.dt, "x", x)
    setkey(summary.dt, x)
    setkey(x.dt, x)
    summary.dt <- merge(summary.dt, x.dt, all=TRUE)
    summary.dt[is.na(count), count := 0]
    how_many_digits <- function(x) {
      max(length(strsplit(as.character(x), ".")[[2]]))
    }
    if(is.numeric(summary.dt[, x])) {
      for (i in seq(summary.dt[, .N])) if(is.na(summary.dt[i, y])) summary.dt[i, y := summary.dt[i-1, y]]
      summary.dt <- summary.dt[count > 0, ]
    }
    row_count <- summary.dt[, .N]
    summary.dt[, xn := seq(row_count)]
    max_count <- max(summary.dt[!is.na(count), count])
    max_y <- max(summary.dt[!is.na(y), y])
    max_abs_y <- max(abs(summary.dt[!is.na(y), y]))
    min_y <- min(summary.dt[!is.na(y), y])
    range_y <- max_y - min_y
    summary.dt[, y_rs := ((y + max_abs_y) / (range_y)) * max_count]
    if(range_y == 0) summary.dt[, y_rs := max_count / 2]
    plot.obj <- ggplot(summary.dt)
    plot.obj <- plot.obj + geom_bar(aes(x=xn, y=count, group=1), stat="identity", fill="yellow", color="yellow", alpha=0.3)
    if(is.numeric(summary.dt[, x])) plot.obj <- plot.obj + geom_line(aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=2)
    if(is.factor(summary.dt[, x])) plot.obj <- plot.obj + geom_point(aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=8)
    if(range_y > 0) {
      plot.obj <- plot.obj + scale_y_continuous(
        name="count",
        sec.axis=sec_axis(~ ((. / max_count * range_y) - max_abs_y), name="partial")
      )
    } else {
      plot.obj <- plot.obj + scale_y_continuous(
        name="count",
        sec.axis=sec_axis(~ . / max_count * max_y, name="partial")
      )
    }
    breaks <- summary.dt[, xn]
    labels <- summary.dt[, x]
    if(row_count >50) {
      breaks <- breaks[seq(1, row_count, 5)]
      labels <- labels[seq(1, row_count, 5)]
    }
    plot.obj <- plot.obj + scale_x_continuous(breaks=breaks, labels=labels)
    plot.obj <- plot.obj + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5), plot.title=element_text(hjust=0.5))
    plot.obj <- plot.obj + ggtitle(x)
    plot.obj <- plot.obj + xlab(x)
  } else {
    plot.obj <- ggplot() +
      annotate("text", x=4, y=25, size=4, label=paste("variable", x, "not modeled")) +
      theme_void()
  }
  plot.obj
}
pdf("model_output.pdf", h=7, w=14)
  for (x in uvar) {
    grid.arrange(
      univariate(train.a.dt, x),
      univariate(train.b.dt, x),
      univariate(test.dt, x),
      partial.plot(model, x, train.dt),
      ncol=2
    )
  }
dev.off()

