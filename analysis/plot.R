
# univariate
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
  max_count <- max(summary.dt[!is.na(count), count])
  max_y <- max(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  min_y <- min(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  range_y <- max_y - min_y
  summary.dt[, act_rs := rebase.y(c(summary.dt[, count], 0), summary.dt[, act])]
  summary.dt[, pred_rs := rebase.y(c(summary.dt[, count], 0), summary.dt[, pred])]
  if(row_count == 1) {
    summary.dt[, act_rs := max_count / 2]
    summary.dt[, pred_rs := max_count / 2]
  }
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
      sec.axis=sec_axis(~ rebase.y(c(summary.dt[, act], summary.dt[, pred]), .), name="act, pred")
    )
  } else {
    plot.obj <- plot.obj + scale_y_continuous(
      name="count",
      sec.axis=sec_axis(~ . / max_count * max_y, name="act, pred")
    )
  }
  breaks <- summary.dt[, xn]
  labels <- summary.dt[, x]
  if(row_count > 50) {
    breaks <- breaks[seq(1, row_count, 5)]
    labels <- labels[seq(1, row_count, 5)]
  }
  plot.obj <- plot.obj + scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj <- plot.obj + theme(
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=element_text(vjust=0.5, hjust=0.5),
    axis.title.y=element_text(vjust=0.5, hjust=0.5)
  )
  plot.obj <- plot.obj + ggtitle(paste(deparse(substitute(a.dt)), x))
  plot.obj <- plot.obj + xlab(x)
  if(row_count > 100) plot.obj <- plot.obj + annotate("text", x=15, y=max(summary.dt[, count]), size=4, label=message)
  plot.obj
}

# partial plot
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
    min_y <- min(summary.dt[!is.na(y), y])
    range_y <- max_y - min_y
    summary.dt[, y_rs := rebase.y(c(summary.dt[, count], 0), summary.dt[, y])]
    if(range_y == 0) summary.dt[, y_rs := max_count / 2]
    plot.obj <- ggplot(summary.dt)
    plot.obj <- plot.obj + geom_bar(aes(x=xn, y=count, group=1), stat="identity", fill="yellow", color="yellow", alpha=0.3)
    if(is.numeric(summary.dt[, x])) plot.obj <- plot.obj + geom_line(aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=2)
    if(is.factor(summary.dt[, x])) plot.obj <- plot.obj + geom_point(aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=8)
    if(range_y > 0) {
      plot.obj <- plot.obj + scale_y_continuous(
        name="count",
        sec.axis=sec_axis(~ rebase.y(summary.dt[, y], .), name="partial")
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

plot.model.run <- function() {
  f <- "logs/model.log"
  label <- readChar(f, file.info(f)$size)
  plot.obj <- ggplot() +
    annotate("text", x=1, y=25, size=2, label=label) + theme_void()
  plot.obj
}
rebase.y <- function(y1, y2, verbose=FALSE) {
  max_y1 <- max(y1)
  max_y2 <- max(y2)
  min_y1 <- min(y1)
  min_y2 <- min(y2)
  range_y2 <- max_y2 - min_y2
  range_y1 <- max_y1 - min_y1

  new_y2a <- y2 - min_y2 # max sure all are >= 0
  max_new_y2a <- max(new_y2a)
  new_y2b <- new_y2a / max_new_y2a # rescale to (0 1)
  new_y2c <- new_y2b * range_y1 # stretch to (0, range_y1)
  new_y2 <- new_y2c + min_y1 # shift to (min_y1, max_y1)
  # logic checks
  if(isTRUE(verbose)) {
    print(all(new_y2a >= 0))
    print(all(c(min(new_y2b) ==0, max(new_y2b) == 1)))
    print(all(c(min(new_y2c) ==0, max(new_y2c) == range_y1)))
    print(all(c(min(new_y2) == min_y1, max(new_y2) == max_y1)))
  }
  new_y2
}
plot.model.perf <- function(model) {
  p.data <- data.table(train.a=model$train.error, train.b=model$valid.error)
  p.data[, trees := seq(p.data[, .N])]
  min_y1 <- min(p.data[, train.a])
  max_y1 <- max(p.data[, train.a])
  range_y1 <- max_y1 - min_y1
  p.data[, train.b_rs := rebase.y(p.data[, train.a], p.data[, train.b])]
  sf <- 0.2
  plot.obj <- ggplot(p.data)
  plot.obj <- plot.obj + geom_line(aes(x=trees, y=train.a), color="red", size=2)
  plot.obj <- plot.obj + geom_line(aes(x=trees, y=train.b_rs), color="blue", size=2)
  best.y <- min(p.data[, train.b_rs])
  best.x <- p.data[train.b_rs == best.y, trees]
  df <- data.frame(x1=c(best.x, best.x), x2=c(best.x, Inf), y1=c(best.y, best.y), y2=c(-Inf, best.y))
  plot.obj <- plot.obj + geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2), data=df, linetype="dashed")
  plot.obj <- plot.obj + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5), plot.title=element_text(hjust=0.5))
  plot.obj <- plot.obj + ggtitle("mean deviance on train.a and train.b")
  plot.obj <- plot.obj + ylab("mean deviance")
  plot.obj <- plot.obj + scale_y_continuous(
    limits=c((min_y1 - (sf * range_y1)), (max_y1 + (sf * range_y1))),
    name="train.a.mean.deviance",
    sec.axis=sec_axis(~ rebase.y(p.data[, train.b], .), name="train.b mean deviance")
  )
  plot.obj 
  plot.obj
}
plot.var.importance <- function() {
  plot.model.run()
}
plot.decile.perf <- function() {
  plot.model.run()
}

plot.model <- function(model, train.a.dt, train.b.dt, train.dt, uvar) {
  pdf("model_output.pdf", h=7, w=14)
    grid.arrange(plot.model.run(), plot.model.perf(model), plot.var.importance(), plot.decile.perf())
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
}

