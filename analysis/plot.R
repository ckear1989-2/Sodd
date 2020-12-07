
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
  new_row_count <- summary.dt[, .N]
  max_count <- max(summary.dt[!is.na(count), count])
  max_y <- max(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  min_y <- min(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  range_y <- max_y - min_y
  summary.dt[, act_rs := rebase.y(c(summary.dt[, count], 0), c(summary.dt[, act], summary.dt[, pred]), nreturn=new_row_count)]
  summary.dt[, pred_rs := rebase.y(c(summary.dt[, count], 0), c(summary.dt[, pred], summary.dt[, act]), nreturn=new_row_count)]
  plot.obj <- ggplot(summary.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=xn, y=count), stat="identity", fill="yellow", color="yellow", alpha=0.3)
  if(row_count > 1) {
    plot.obj <- plot.obj + geom_line(aes(x=xn, y=act_rs), stat="identity", color="red")
    plot.obj <- plot.obj + geom_line(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  }
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=act_rs), stat="identity", color="red")
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  plot.obj <- plot.obj + scale_y_continuous(
    name="count",
    sec.axis=sec_axis(~ rebase.y(c(summary.dt[, act], summary.dt[, pred]), .), name="act, pred")
  )
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
    axis.title.y=element_text(vjust=0.5, hjust=0.5),
    panel.border=element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggtitle(paste(deparse(substitute(a.dt)), x))
  plot.obj <- plot.obj + xlab(x)
  if(row_count > 100) plot.obj <- plot.obj + annotate("text", x=15, y=max(summary.dt[, count]), size=4, label=message)
  plot.obj
}

# partial plot
partial.plot <- function(model, x, a.dt) {
  print(attributes(model))
  if (x %in% model$var.names) {
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
    plot.obj <- plot.obj + theme(
      axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
      plot.title=element_text(hjust=0.5),
      panel.border=element_rect(colour="black", fill=NA, size=2)
    )
    plot.obj <- plot.obj + ggtitle(x)
    plot.obj <- plot.obj + xlab(x)
  } else {
    plot.obj <- ggplot() +
      annotate("text", x=4, y=25, size=4, label=paste("variable", x, "not modeled")) +
      theme_void() + theme(panel.border=element_rect(colour="black", fill=NA, size=2))
  }
  plot.obj
}

plot.model.run <- function(adate) {
  f <- paste0("logs/model_", adate, ".log")
  label <- trimws(readChar(f, file.info(f)$size))
  label <- strsplit(label, paste0(rep("#", 30), collapse=""), fixed=TRUE)[[1]]
  a_text_grob <- function(ii) {
    ilabel <- label[[ii]]
    ilines <- strsplit(ilabel, "\n")[[1]]
    lines <- length(ilines)
    ititle <- ilines[2]
    print(ititle)
    ilabel <- paste0(ilines[3:lines], collapse="\n")
    print(ilabel)
    if(lines > 12) {
      ilabel <- paste(paste0(ilines[3:10], collapse="\n"), "...", sep="\n")
      lines <- 10
    }
    xi <- unit(1.0, "lines")
    yt <- unit(8.0, "lines")
    yl <- unit((11.5-(lines/2)), "lines")
    grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill=ii, lwd=2, col="black", alpha=0.5)),
      grid::textGrob(
        label=ititle,
        x=xi, y=yt,
        gp=grid::gpar(fontsize=12, fontface="bold"), just="left"),
      grid::textGrob(
        label=ilabel,
        x=xi, y=yl,
        gp=grid::gpar(fontsize=8), just="left")
    )
  }
  gs <- lapply(c(2, 3, 6, 9) , a_text_grob)
  arrangeGrob(grobs=gs, ncol=2)
}

rebase.y <- function(y1, y2, nreturn=length(y2), verbose=FALSE) {
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
  new_y2[1:nreturn]
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
  plot.obj <- plot.obj + theme(
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=element_text(hjust=0.5),
    panel.border=element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggtitle("mean deviance on train.a and train.b")
  plot.obj <- plot.obj + scale_y_continuous(
    limits=c((min_y1 - (sf * range_y1)), (max_y1 + (sf * range_y1))),
    name="train.a.mean.deviance",
    sec.axis=sec_axis(~ rebase.y(p.data[, train.b], .), name="train.b mean deviance")
  )
  plot.obj
}

plot.var.importance <- function(model) {
  p.dt <- data.table(summary(model, plotit=FALSE))
  p.dt[, sv := -rel.inf]
  setkey(p.dt, sv)
  p.dt[, x := seq(p.dt[, .N])]
  plot.obj <- ggplot(p.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=x, y=rel.inf), stat="identity", color="yellow", fill="yellow", alpha=0.3)
  plot.obj <- plot.obj + theme(
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=element_text(hjust=0.5),
    panel.border=element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggtitle("modeled variables relative influence")
  plot.obj <- plot.obj + ylab("relative influence")
  plot.obj <- plot.obj + xlab("modeled variable")
  breaks <- p.dt[, x]
  labels <- p.dt[, var]
  plot.obj <- plot.obj + scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj
}

plot.decile.perf <- function(train.a.dt, train.b.dt, test.dt) {
  # sort predictions into deciles
  setkey(train.a.dt, gbmp)
  setkey(train.b.dt, gbmp)
  setkey(test.dt, gbmp)
  train.a.dt[, rn := seq(train.a.dt[, .N])]
  train.b.dt[, rn := seq(train.b.dt[, .N])]
  test.dt[, rn := seq(test.dt[, .N])]
  train.a.dt[, decile := cut(train.a.dt[, rn], breaks=quantile(train.a.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)]
  train.b.dt[, decile := cut(train.b.dt[, rn], breaks=quantile(train.b.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)]
  test.dt[, decile := cut(test.dt[, rn], breaks=quantile(test.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)]
  summary.dt <- rbind(
    train.a.dt[, list(dt="train.a", dtc="red", gain=sum(gain), gbmp=sum(gbmp), count=.N), decile],
    train.b.dt[, list(dt="train.b", dtc="blue", gain=sum(gain), gbmp=sum(gbmp), count=.N), decile],
    test.dt[, list(dt="test", dtc="green", gain=sum(gain), gbmp=sum(gbmp), count=.N), decile]
  )
  summary.dt[, gain := gain / count]
  summary.dt[, gbmp := gbmp / count]
  row_count <- summary.dt[, .N]
  summary.dt[, gain_rs := rebase.y(summary.dt[, count], c(summary.dt[, gain], summary.dt[, gbmp]), nreturn=row_count)]
  summary.dt[, gbmp_rs := rebase.y(summary.dt[, count], c(summary.dt[, gbmp], summary.dt[, gain]), nreturn=row_count)]
  plot.obj <- ggplot(summary.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=decile, y=count, color=dt, fill=dt), stat="identity", position="dodge", alpha=0.3)
  plot.obj <- plot.obj + geom_line(aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + geom_line(aes(x=decile, y=gbmp_rs, group=dt, color=dt), stat="identity", linetype="dashed")
  # plot.obj <- plot.obj + geom_point(aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + geom_point(aes(x=decile, y=gbmp_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + theme(
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=element_text(hjust=0.5),
    panel.border=element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggtitle("actual and predicted gain by predicted deciles")
  plot.obj <- plot.obj + scale_y_continuous(
    name="count",
    sec.axis=sec_axis(~ rebase.y(c(summary.dt[, gain], summary.dt[, gbmp]), .), name="act, pred")
  )
  plot.obj
}

grid.square <- quote({
  grid::grid.rect(x=0.25, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.25, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
})

plot.model <- function(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, uvar) {
  pdf(paste0("model_output_", adate, ".pdf"), h=7, w=14)
    grid.arrange(
      plot.model.run(adate),
      plot.model.perf(model),
      plot.var.importance(model),
      plot.decile.perf(train.a.dt, train.b.dt, test.dt)
    )
    eval(grid.square)
    for (x in uvar) {
      grid.arrange(
        univariate(train.a.dt, x),
        univariate(train.b.dt, x),
        univariate(test.dt, x),
        partial.plot(model, x, train.dt),
        ncol=2
      )
      eval(grid.square)
    }
  dev.off()
}

