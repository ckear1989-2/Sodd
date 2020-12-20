
source("analysis/strategy.R")
source("utils/utils.R")
source("data_prep/download.R")

# univariate
univariate <- function(a.dt, x) {
  setnames(a.dt, x, "x")
  summary.dt <- a.dt[, list(act=sum(y), pred=sum(gbmp), count=.N, weight=sum(weight)), x][order(-weight)]
  setnames(a.dt, "x", x)
  summary.dt[, act := act / weight]
  summary.dt[, pred := pred / weight]
  if(is.numeric(summary.dt[, x])) setkey(summary.dt, x)
  summary.dt[, xn := seq(summary.dt[, .N])]
  row_count <- summary.dt[, .N]
  if(row_count > 100) {
    message <- paste("plotting top 100 of", summary.dt[, .N], "levels")
    summary.dt <- summary.dt[xn <= 100, ]
  }
  new_row_count <- summary.dt[, .N]
  max_weight <- max(summary.dt[!is.na(weight), weight])
  max_y <- max(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  min_y <- min(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  range_y <- max_y - min_y
  summary.dt[, act_rs := rebase.y(c(summary.dt[, weight], 0), c(summary.dt[, act], summary.dt[, pred]), nreturn=new_row_count)]
  summary.dt[, pred_rs := rebase.y(c(summary.dt[, weight], 0), c(summary.dt[, pred], summary.dt[, act]), nreturn=new_row_count)]
  plot.obj <- ggplot2::ggplot(summary.dt)
  plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x=xn, y=weight), stat="identity", fill="yellow", color="yellow", alpha=0.3)
  if(row_count > 1) {
    plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=xn, y=act_rs), stat="identity", color="red")
    plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=xn, y=pred_rs), stat="identity", color="blue")
  }
  plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x=xn, y=act_rs), stat="identity", color="red")
  plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x=xn, y=pred_rs), stat="identity", color="blue")
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name="weight",
    sec.axis=ggplot2::sec_axis(~ rebase.y(c(summary.dt[, act], summary.dt[, pred]), .), name="act, pred")
  )
  breaks <- summary.dt[, xn]
  labels <- summary.dt[, x]
  if(row_count > 50) {
    breaks <- breaks[seq(1, row_count, 5)]
    labels <- labels[seq(1, row_count, 5)]
  }
  if(length(breaks) != length(labels)) {
    print(summary.dt)
    print(breaks)
    print(labels)
    stop("length breaks labels differ")
  }
  plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(vjust=0.5, hjust=0.5),
    axis.title.y=ggplot2::element_text(vjust=0.5, hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle(paste(deparse(substitute(a.dt)), x))
  plot.obj <- plot.obj + ggplot2::xlab(x)
  if(row_count > 100) plot.obj <- plot.obj + ggplot2::annotate("text", x=15, y=max(summary.dt[, weight]), size=4, label=message)
  plot.obj
}

# partial plot
partial.plot <- function(model, x, a.dt) {
  # print(attributes(model))
  if (x %in% model$var.names) {
    x.dt <- data.table(plot(model, x, return.grid=TRUE))
    setnames(x.dt, x, "x")
    setnames(a.dt, x, "x")
    summary.dt <- a.dt[, list(count=.N, weight=sum(weight)), x]
    setnames(a.dt, "x", x)
    setkey(summary.dt, x)
    setkey(x.dt, x)
    summary.dt <- merge(summary.dt, x.dt, all=TRUE)
    summary.dt[is.na(weight), weight := 0]
    if(is.numeric(summary.dt[, x])) {
      for (i in seq(summary.dt[, .N])) if(is.na(summary.dt[i, y])) summary.dt[i, y := summary.dt[i-1, y]]
      summary.dt <- summary.dt[weight > 0, ]
    }
    row_count <- summary.dt[, .N]
    setkey(summary.dt, x)
    summary.dt[, xn := seq(row_count)]
    max_weight <- max(summary.dt[!is.na(weight), weight])
    max_y <- max(summary.dt[!is.na(y), y])
    min_y <- min(summary.dt[!is.na(y), y])
    range_y <- max_y - min_y
    summary.dt[, y_rs := rebase.y(c(summary.dt[, weight], 0), summary.dt[, y])]
    if(range_y == 0) summary.dt[, y_rs := max_weight / 2]
    plot.obj <- ggplot2::ggplot(summary.dt)
    plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x=xn, y=weight, group=1), stat="identity", fill="yellow", color="yellow", alpha=0.3)
    if(is.numeric(summary.dt[, x])) plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=2)
    if(is.factor(summary.dt[, x])) plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=8)
    if(range_y > 0) {
      plot.obj <- plot.obj + ggplot2::scale_y_continuous(
        name="weight",
        sec.axis=ggplot2::sec_axis(~ rebase.y(summary.dt[, y], .), name="partial")
      )
    } else {
      plot.obj <- plot.obj + ggplot2::scale_y_continuous(
        name="weight",
        sec.axis=ggplot2::sec_axis(~ . / max_weight * max_y, name="partial")
      )
    }
    breaks <- summary.dt[, xn]
    labels <- summary.dt[, x]
    if(row_count >50) {
      breaks <- breaks[seq(1, row_count, 5)]
      labels <- labels[seq(1, row_count, 5)]
    }
    if(length(breaks) != length(labels)) {
      stop("length breaks labels differ")
    }
    plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks=breaks, labels=labels)
    plot.obj <- plot.obj + ggplot2::theme(
      axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
      plot.title=ggplot2::element_text(hjust=0.5),
      panel.border=ggplot2::element_rect(colour="black", fill=NA, size=2)
    )
    plot.obj <- plot.obj + ggplot2::ggtitle(x)
    plot.obj <- plot.obj + ggplot2::xlab(x)
  } else {
    plot.obj <- ggplot2::ggplot() +
      ggplot2::annotate("text", x=4, y=25, size=4, label=paste("variable", x, "not modeled")) +
      ggplot2::theme_void() + ggplot2::theme(panel.border=ggplot2::element_rect(colour="black", fill=NA, size=2))
  }
  plot.obj
}

plot.model.run <- function(model, train.a.dt, train.b.dt, test.dt, upcoming.dt) {
  model.param.p.obj <- plot.model.param(model)
  model.perf.p.obj <- plot.data.perf(train.a.dt, train.b.dt, test.dt, upcoming.dt)
  strat.p.obj <- plot.strategies(test.dt)
  gs <- list(model.param.p.obj[[1]], model.param.p.obj[[2]], model.perf.p.obj, strat.p.obj)
  lay <- rbind(
    c(1,3),
    c(2,3),
    c(4,4),
    c(4,4)
  )
  gridExtra::arrangeGrob(grobs=gs, layout_matrix=lay)
}
find_cell <- function(table, row, col, name="core-fg") {
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}
padding <- grid::unit.c(grid::unit(2, "mm"), grid::unit(2, "mm"))
table.theme <- function(fs) {
  gridExtra::ttheme_default(
    core=list(
      fg_params=list(fontsize=fs, just="left"),
      padding=padding
    ),
    rowhead=list(
      fg_params=list(fontsize=fs, fontface="bold", just="left"),
      padding=padding
    ),
    colhead=list(
      fg_params=list(fontsize=fs, fontface="bold", just="left"),
      padding=padding
    )
  )
}
colorise.tableGrob <- function(obj, dt, col1, col2, fs=12) {
  # set all font sizes
  for(x in 1:(ncol(dt)+1)) {
    for (y in 1:(nrow(dt)+1)) {
      for(fg in c("colhead-fg", "rowhead-fg", "core-fg")) {
        ind <- find_cell(obj, y, x, fg)
        if(!length(ind) > 0) {
          next
        } else {
          obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize=fs, just="left")
        }
      }
    }
  }
  # bold first row
  for(x in 1:(ncol(dt)+1)) {
    for(fg in c("core-fg", "colhead-fg")) {
      ind <- find_cell(obj, 1, x, fg)
      if(!length(ind) > 0) {
        next
      } else {
        obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize=fs, fontface="bold", just="left")
      }
    }
  }
  # bold first column
  for(x in 1:(nrow(dt)+1)) {
    for(fg in c("core-fg", "rowhead-fg")) {
      ind <- find_cell(obj, x, 1, fg)
      if(!length(ind) > 0) {
        next
      } else {
        obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize=fs, fontface="bold", just="left")
      }
    }
  }
  # alternate colors
  for(x in 1:(ncol(dt)+1)) {
    for (y in 1:(nrow(dt)+1)) {
      for(bg in c("colhead-bg", "rowhead-bg", "core-bg")) {
        ind <- find_cell(obj, y, x, bg)
        if(!length(ind) > 0) {
          next
        } else {
            if((y %% 2) == 0) {
              fill <- col1
            } else {
              fill <- col2
            }
            obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fill=fill, col="white", just="left")
        }
      }
    }
  }
  obj
}

plot.model.param <- function(model) {
  params <- c(
    "train.fraction",
    "cv.folds",
    "n.trees",
    "shrinkage",
    "interaction.depth",
    "family",
    "train.error",
    "valid.error",
    "cv.error",
    "oobag.improve"
  )
  params <- gsub("\\.", "\n", params)
  best.trees <- gbm::gbm.perf(model, plot.it=FALSE, method="test")
  vals <- c(
    round(model$train.fraction, 2),
    model$cv.folds,
    model$n.trees,
    round(model$shrinkage, 3),
    round(model$interaction.depth),
    model$distribution$name,
    round(model$train.error[[best.trees]], 4),
    round(model$valid.error[[best.trees]], 4),
    round(model$cv.error[[best.trees]], 4),
    round(model$oobag.improve[[best.trees]], 4)
  )
  params.dt.0 <- t(data.frame(parameter=params[1:5], value=vals[1:5]))
  p.obj.0 <- gridExtra::tableGrob(params.dt.0, theme=table.theme(10), cols=NULL)
  p.obj.0 <- colorise.tableGrob(p.obj.0, params.dt.0, "red1", "red3", 10)
  params.dt.1 <- t(data.frame(parameter=params[6:10], value=vals[6:10]))
  p.obj.1 <- gridExtra::tableGrob(params.dt.1, theme=table.theme(10), cols=NULL)
  p.obj.1 <- colorise.tableGrob(p.obj.1, params.dt.1, "red1", "red3", 10)
  # p.obj <- gridExtra::arrangeGrob(list(p.obj.0, p.obj.1), layout=rbind(c(1), c(2)))
  p.obj.0 <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="red1", lwd=0, col="black", alpha=0.5)),
    p.obj.0
  )
  p.obj.1 <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="red1", lwd=0, col="black", alpha=0.5)),
    p.obj.1
  )
  list(p.obj.0, p.obj.1)
}

plot.data.perf <- function(train.a.dt, train.b.dt, test.dt, upcoming.dt) {
  dts <- c(
    "train.a",
    "train.b",
    "test",
    "upcoming"
  )
  min.date <- c(
    min(train.a.dt[, date]),
    min(train.b.dt[, date]),
    min(test.dt[, date]),
    min(upcoming.dt[, date])
  )
  max.date <- c(
    max(train.a.dt[, date]),
    max(train.b.dt[, date]),
    max(test.dt[, date]),
    max(upcoming.dt[, date])
  )
  records <- c(
    train.a.dt[, .N],
    train.b.dt[, .N],
    test.dt[, .N],
    upcoming.dt[, .N]
  )
  matches <- c(
    length(unique(train.a.dt[, match_id])),
    length(unique(train.b.dt[, match_id])),
    length(unique(test.dt[, match_id])),
    length(unique(upcoming.dt[, match_id]))
  )
  null.dev <- c(
    round(mean(train.a.dt[, null_dev]), 4),
    round(mean(train.b.dt[, model_dev]), 4),
    round(mean(test.dt[, null_dev]), 4),
    NA
  )
  model.dev <- c(
    round(mean(train.a.dt[, model_dev]), 4),
    round(mean(train.b.dt[, null_dev]), 4),
    round(mean(test.dt[, model_dev]), 4),
    NA
  )
  devs.dt <- data.frame(
    data=dts,
    min.date=min.date,
    max.date=max.date,
    records=records,
    matches=matches,
    null.dev=null.dev,
    model.dev=model.dev
  )
  setnames(devs.dt, colnames(devs.dt), gsub("\\.", "\n", colnames(devs.dt)))
  p.obj <- gridExtra::tableGrob(devs.dt, theme=table.theme(7), rows=NULL)
  p.obj <- colorise.tableGrob(p.obj, devs.dt, "gold", "gold3", 7)
  p.obj <- grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill="gold", lwd=2, col="black", alpha=0.5)),
      p.obj
  )
  p.obj
}

plot.strategies <- function(a.dt) {
  strategy <- c(
    "all",
    "fav",
    "out",
    "home",
    "draw",
    "away",
    "top_pct_10",
    "top_pct_5",
    "top_pct_1",
    "top_per_match"
  )
  stake <- sapply(strategy, function(x) sum(a.dt[[paste0("strat_", x)]]))
  gain <- sapply(strategy, function(x) round(sum(a.dt[[paste0("gain_", x)]]), 2))
  stake_wtd <- sapply(strategy, function(x) sum(a.dt[[paste0("strat_", x, "_wtd")]]))
  gain_wtd <- sapply(strategy, function(x) round(sum(a.dt[[paste0("gain_", x, "_wtd")]]), 2))
  strategy <- gsub("_", "\n", strategy)
  strat.dt <- t(data.frame(strategy=strategy, stake=stake, gain=gain, stake_wtd=stake_wtd, gain_wtd=gain_wtd))
  strat.p.obj <- gridExtra::tableGrob(strat.dt, theme=table.theme(10), cols=NULL)
  strat.p.obj <- colorise.tableGrob(strat.p.obj, strat.dt, "grey90", "grey95", 10)
  strat.p.obj <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="grey90", lwd=2, col="black", alpha=0.5)),
    strat.p.obj
  )
  strat.p.obj
}

plot.model.perf <- function(model) {
  p.data <- data.table(
    train.a=model$train.error,
    train.b=model$valid.error,
    cv=model$cv.error
  )
  p.data[, trees := seq(p.data[, .N])]
  min_y1 <- min(p.data[, train.a])
  max_y1 <- max(p.data[, train.a])
  range_y1 <- max_y1 - min_y1
  p.data[, cv_rs := rebase.y(c(p.data[, train.a], p.data[, train.b]), p.data[, cv])]
  sf <- 0.2
  plot.obj <- ggplot2::ggplot(p.data)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=trees, y=train.a), color="red", size=2)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=trees, y=train.b), color="blue", size=2)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=trees, y=cv_rs), color="green", size=2)
  best.y <- min(p.data[, train.b])
  best.x <- p.data[train.b == best.y, trees]
  df <- data.frame(x1=c(1, best.x), x2=c(best.x, best.x), y1=c(best.y, best.y), y2=c(best.y, -Inf))
  plot.obj <- plot.obj + ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, xend=x2, yend=y2), data=df, linetype="dashed")
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("mean deviance on train.a, train.b and cv")
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name="train.a, train.b mean.deviance",
    sec.axis=ggplot2::sec_axis(~ rebase.y(p.data[, cv], .), name="cv mean.deviance")
  )
  plot.obj
}

plot.var.importance <- function(model) {
  p.dt <- data.table(summary(model, plotit=FALSE))
  p.dt[, sv := -rel.inf]
  setkey(p.dt, sv)
  p.dt[, x := seq(p.dt[, .N])]
  plot.obj <- ggplot2::ggplot(p.dt)
  plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x=x, y=rel.inf), stat="identity", color="yellow", fill="yellow", alpha=0.3)
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("modeled variables relative influence")
  plot.obj <- plot.obj + ggplot2::ylab("relative influence")
  plot.obj <- plot.obj + ggplot2::xlab("modeled variable")
  breaks <- p.dt[, x]
  labels <- p.dt[, var]
  if(length(breaks) != length(labels)) {
    print(summary.dt)
    print(breaks)
    print(labels)
    stop("length breaks labels differ")
  }
  plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj
}

plot.decile.perf <- function(train.a.dt, train.b.dt, test.dt) {
  # sort predictions into deciles
  setkey(train.a.dt, gbmp)
  setkey(train.b.dt, gbmp)
  setkey(test.dt, gbmp)
  if(train.a.dt[is.na(gbmp),.N] > 0) {
    print(train.a.dt[is.na(gbmp), ])
    stop("train a na model predictions")
  }
  if(train.b.dt[is.na(gbmp),.N] > 0) {
    print(train.b.dt[is.na(gbmp), ])
    stop("train b na model predictions")
  }
  if(test.dt[is.na(gbmp),.N] > 0) {
    print(test.dt[is.na(gbmp), ])
    stop("test na model predictions")
  }
  train.a.dt[, rn := seq(train.a.dt[, .N])]
  train.b.dt[, rn := seq(train.b.dt[, .N])]
  test.dt[, rn := seq(test.dt[, .N])]
  train.a.dt[, decile := as.numeric(as.character(cut(train.a.dt[, rn], breaks=quantile(train.a.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  train.b.dt[, decile := as.numeric(as.character(cut(train.b.dt[, rn], breaks=quantile(train.b.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  test.dt[, decile := as.numeric(as.character(cut(test.dt[, rn], breaks=quantile(test.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  summary.dt <- rbind(
    train.a.dt[, list(dt="train.a", dtc="red", gain=sum(gain), gain_wtd=sum(gain_all_wtd), count=.N, weight=sum(weight)), decile],
    train.b.dt[, list(dt="train.b", dtc="blue", gain=sum(gain), gain_wtd=sum(gain_all_wtd), count=.N, weight=sum(weight)), decile],
    test.dt[, list(dt="test", dtc="green", gain=sum(gain), gain_wtd=sum(gain_all_wtd), count=.N, weight=sum(weight)), decile]
  )
  summary.dt[, gain := gain / weight]
  summary.dt[, gain_wtd := gain_wtd / weight]
  row_count <- summary.dt[, .N]
  summary.dt[, gain_rs := rebase.y(summary.dt[, weight], summary.dt[, gain], nreturn=row_count)]
  summary.dt[, gain_wtd_rs := rebase.y(summary.dt[, weight], summary.dt[, gain_wtd], nreturn=row_count)]
  plot.obj <- ggplot2::ggplot(summary.dt)
  plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x=decile, y=weight, color=dt, fill=dt), stat="identity", position="dodge", alpha=0.3)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=decile, y=gain_wtd_rs, group=dt, color=dt), stat="identity", linetype="dotted")
  # plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("actual and predicted gain by predicted deciles")
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name="weight",
    sec.axis=ggplot2::sec_axis(~ rebase.y(c(summary.dt[, gain], summary.dt[, gain_wtd]), .), name="gain, gain_wtd")
  )
  plot.obj
}

grid.square <- quote({
  grid::grid.rect(x=0.25, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.25, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
})

detailed.strat.gtable <- function(a.dt, recent.dt, aname) {
  a.thin.dt <- a.dt[strat_top_per_match > 0, list(
    match_id,
    ftr,
    actr,
    ip,
    pred_prob=round(pred_prob, 3),
    odds=round(odds, 3),
    pred_odds=round(pred_odds, 3),
    spread=round(spread, 3),
    pred_spread=round(pred_spread, 3),
    strat_top_per_match,
    gain_top_per_match=round(gain_top_per_match, 3),
    strat_top_per_match_wtd,
    gain_top_per_match_wtd=round(gain_top_per_match_wtd, 3)
    )][order(-pred_spread)][1:20, ]
  if(all(is.na(a.thin.dt[, ftr]))) {
    a.thin.dt[, ftr := NULL]
    setkey(a.thin.dt, match_id)
    setkey(recent.dt, match_id)
    a.thin.dt <- merge(a.thin.dt, recent.dt, all.x=TRUE, all.y=FALSE, by="match_id")
    a.thin.dt[, spread := NA]
  }
  a.thin.dt <- rbind(
    a.thin.dt,
    data.table(
      match_id="total",
      strat_top_per_match=sum(a.thin.dt[, strat_top_per_match]),
      strat_top_per_match_wtd=sum(a.thin.dt[, strat_top_per_match_wtd]),
      gain_top_per_match=sum(a.thin.dt[, gain_top_per_match]),
      gain_top_per_match_wtd=sum(a.thin.dt[, gain_top_per_match_wtd])
   ), fill=TRUE)
  setnames(a.thin.dt, colnames(a.thin.dt), gsub("_", "\n", colnames(a.thin.dt)))
  p.obj <- gridExtra::tableGrob(a.thin.dt, theme=table.theme(16), rows=NULL)
  p.obj <- colorise.tableGrob(p.obj, a.thin.dt, "grey90", "grey95", 16)

  set_row_border <- function(obj, row, color) {
    # row + 1 because of header
    gtable::gtable_add_grob(obj, grobs=grid::rectGrob(gp=grid::gpar(fill=color, lwd=2, col=color, alpha=0.5)), t=(row+1.02), b=(row+1.98), l=1.02, r=(ncol(obj)+1))
  }

  correct_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, ftr] == a.thin.dt[i, actr]) & (!a.thin.dt[i, actr] == "NA"))
  incorrect_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, ftr] != a.thin.dt[i, actr]) & (!a.thin.dt[i, actr] == "NA"))
  unknown_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, actr] == "NA") | is.na(a.thin.dt[i, actr]))

  set_row_border(p.obj, 0, "black")
  for (i in 1:a.thin.dt[, .N]) {
    if (isTRUE(correct_preds[[i]])) p.obj <- set_row_border(p.obj, i, "green")
    if (isTRUE(incorrect_preds[[i]])) p.obj <- set_row_border(p.obj, i, "red")
    if (isTRUE(unknown_preds[[i]])) p.obj <- set_row_border(p.obj, i, "black")
  }
  p.obj <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="grey90", lwd=0, col="black", alpha=0.5)),
    grid::textGrob(
      label=paste0(aname, " strategy top per match topn=", (a.thin.dt[, .N] -1)),
      gp=grid::gpar(fontsize=16, fontface="bold", fill="black", col="black"),
      x=0.5,
      y=0.9,
    ),
    p.obj
  )
  p.obj
}

plot.detailed.strategy <- function(test.dt, upcoming.dt) {
  # dload_current_year(quiet=TRUE)
  recent.csv <- paste0("~/data/", years[[1]], "/", leagues, ".csv")
  alist <- lapply(
    recent.csv
    ,
    fread
  )
  recent.dt <- rbindlist(alist, fill=TRUE)
  setnames(recent.dt, colnames(recent.dt), tolower(colnames(recent.dt)))
  recent.dt[, date := as.Date(date, '%d/%m/%y')]
  recent.dt[, match_id := paste0(date, "|", hometeam, "|", awayteam, collapse="|"), list(date, hometeam, awayteam)]
  recent.dt <- recent.dt[, list(match_id, actr=ftr)]

  p.obj.test <- detailed.strat.gtable(test.dt, recent.dt, "test")
  p.obj.upcoming <- detailed.strat.gtable(upcoming.dt, recent.dt, "upcoming")
  grid::grid.newpage(); grid::grid.draw(p.obj.test)
  grid::grid.newpage(); grid::grid.draw(p.obj.upcoming)
}

plot.model <- function(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, logfile) {
  pdffile <- gsub(".log", ".pdf", logfile)
  pdf(pdffile, h=7, w=14)
    gridExtra::grid.arrange(
      plot.model.run(model, train.a.dt, train.b.dt, test.dt, upcoming.dt),
      plot.model.perf(model),
      plot.var.importance(model),
      plot.decile.perf(train.a.dt, train.b.dt, test.dt)
    )
    eval(grid.square)
    plot.detailed.strategy(test.dt, upcoming.dt)
    for (x in uvar) {
      gridExtra::grid.arrange(
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

