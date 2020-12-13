
source("analysis/strategy.R")
source("utils/utils.R")

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
  plot.obj <- ggplot(summary.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=xn, y=weight), stat="identity", fill="yellow", color="yellow", alpha=0.3)
  if(row_count > 1) {
    plot.obj <- plot.obj + geom_line(aes(x=xn, y=act_rs), stat="identity", color="red")
    plot.obj <- plot.obj + geom_line(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  }
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=act_rs), stat="identity", color="red")
  plot.obj <- plot.obj + geom_point(aes(x=xn, y=pred_rs), stat="identity", color="blue")
  plot.obj <- plot.obj + scale_y_continuous(
    name="weight",
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
  if(row_count > 100) plot.obj <- plot.obj + annotate("text", x=15, y=max(summary.dt[, weight]), size=4, label=message)
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
    how_many_digits <- function(x) {
      max(length(strsplit(as.character(x), ".")[[2]]))
    }
    if(is.numeric(summary.dt[, x])) {
      for (i in seq(summary.dt[, .N])) if(is.na(summary.dt[i, y])) summary.dt[i, y := summary.dt[i-1, y]]
      summary.dt <- summary.dt[weight > 0, ]
    }
    row_count <- summary.dt[, .N]
    summary.dt[, xn := seq(row_count)]
    max_weight <- max(summary.dt[!is.na(weight), weight])
    max_y <- max(summary.dt[!is.na(y), y])
    min_y <- min(summary.dt[!is.na(y), y])
    range_y <- max_y - min_y
    summary.dt[, y_rs := rebase.y(c(summary.dt[, weight], 0), summary.dt[, y])]
    if(range_y == 0) summary.dt[, y_rs := max_weight / 2]
    plot.obj <- ggplot(summary.dt)
    plot.obj <- plot.obj + geom_bar(aes(x=xn, y=weight, group=1), stat="identity", fill="yellow", color="yellow", alpha=0.3)
    if(is.numeric(summary.dt[, x])) plot.obj <- plot.obj + geom_line(aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=2)
    if(is.factor(summary.dt[, x])) plot.obj <- plot.obj + geom_point(aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=8)
    if(range_y > 0) {
      plot.obj <- plot.obj + scale_y_continuous(
        name="weight",
        sec.axis=sec_axis(~ rebase.y(summary.dt[, y], .), name="partial")
      )
    } else {
      plot.obj <- plot.obj + scale_y_continuous(
        name="weight",
        sec.axis=sec_axis(~ . / max_weight * max_y, name="partial")
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

plot.model.run <- function(model, train.a.dt, train.b.dt, test.dt) {
  model.param.p.obj <- plot.model.param(model)
  deviances.p.obj <- plot.deviances(train.a.dt, train.b.dt, test.dt)
  strat.p.obj <- plot.strategies(test.dt)
  gs <- list(model.param.p.obj, deviances.p.obj, strat.p.obj)
  lay <- rbind(
    c(1,2),
    c(3,3)
  )
  arrangeGrob(grobs=gs, layout_matrix=lay)
}
find_cell <- function(table, row, col, name="core-fg") {
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}
padding <- grid::unit.c(grid::unit(2, "mm"), grid::unit(2, "mm"))
table.theme <- function(fs) {
  ttheme_default(
    core=list(
      fg_params=list(fontsize=fs, hjust=0, x=0.1),
      padding=padding
    ),
    rowhead=list(
      fg_params=list(fontsize=fs, fontface="bold", hjust=0, x=0.1),
      bg_params=list(fill=blues9[1:3], col=NA),
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
          obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize=fs)
        }
      }
    }
  }
  # bold first row
  for(x in 1:(ncol(dt)+1)) {
    fg <- "core-fg"
    if("colhead-fg" %in% obj$layout$name) fg <- "colhead-fg"
    ind <- find_cell(obj, 1, x, fg)
    if(!length(ind) > 0) {
      next
    } else {
      obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize=fs, fontface="bold")
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
            obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fill=fill, col="white")
        }
      }
    }
  }
  obj
}
plot.model.param <- function(model) {
  params <- c("n.trees", "shrinkage", "interaction.depth", "train.fraction", "train.error", "valid.error")
  params <- gsub(".", "\n", params, fixed=TRUE)
  vals <- c(
    round(model$n.trees),
    round(model$shrinkage, 3),
    round(model$interaction.depth),
    round(model$train.fraction, 2),
    round(model$train.error[[model$n.trees]], 4),
    round(model$valid.error[[model$n.trees]], 4)
  )
  params.dt <- t(data.frame(params, value=vals))
  p.obj <- tableGrob(params.dt, theme=table.theme(7), cols=NULL)
  p.obj <- colorise.tableGrob(p.obj, params.dt, "red1", "red3", 7)
  p.obj <- grid::grobTree(
      grid::textGrob(
        label="Parameters",
        gp=grid::gpar(fontsize=16, fontface="bold", fill="black", col="black"),
        x=0.5,
        y=0.9
      ),
      grid::rectGrob(gp=grid::gpar(fill="red1", lwd=2, col="black", alpha=0.5)),
      p.obj
  )
  p.obj
}
plot.deviances <- function(train.a.dt, train.b.dt, test.dt) {
  devs <- c(
    "train.a_mean_null",
    "train.a_mean_model",
    "train.b_mean_null", 
    "train.b_mean_model",
    "test_mean_null",
    "test_mean_model"
  )
  devs <- gsub("_", "\n", devs, fixed=TRUE)
  vals <- c(
    round(mean(train.a.dt[, null_dev]), 4),
    round(mean(train.a.dt[, model_dev]), 4),
    round(mean(train.b.dt[, null_dev]), 4),
    round(mean(train.b.dt[, model_dev]), 4),
    round(mean(test.dt[, null_dev]), 4),
    round(mean(test.dt[, model_dev]), 4)
  )
  devs.dt <- t(data.frame(devs, value=vals))
  p.obj <- tableGrob(devs.dt, theme=table.theme(8), cols=NULL)
  p.obj <- colorise.tableGrob(p.obj, devs.dt, "blue1", "blue4", 8)
  p.obj <- grid::grobTree(
      grid::textGrob(
        label="Deviances",
        gp=grid::gpar(fontsize=16, fontface="bold", fill="black", col="black"),
        x=0.5,
        y=0.9
      ),
      grid::rectGrob(gp=grid::gpar(fill="blue1", lwd=2, col="black", alpha=0.5)),
      p.obj
  )
  p.obj
}
plot.strategies <- function(a.dt) {
  strategy <- c("all_results", "all_fav", "all_out", "all_home", "all_draw", "all_away", "top_pct10", "top_pct5", "top_pct1")
  strategy <- gsub("_", "\n", strategy)
  stake <- c(
    a.dt[, .N],
    sum(a.dt[, strat_fav]),
    sum(a.dt[, strat_out]),
    sum(a.dt[, strat_home]),
    sum(a.dt[, strat_draw]),
    sum(a.dt[, strat_away]),
    sum(a.dt[, strat_top_pct_10]),
    sum(a.dt[, strat_top_pct_5]),
    sum(a.dt[, strat_top_pct_1])
  )
  gain <- c(
    round(sum(a.dt[, gain]), 2),
    round(sum(a.dt[, gain_fav]), 2),
    round(sum(a.dt[, gain_out]), 2),
    round(sum(a.dt[, gain_home]), 2),
    round(sum(a.dt[, gain_draw]), 2),
    round(sum(a.dt[, gain_away]), 2),
    round(sum(a.dt[, gain_top_pct_10]), 2),
    round(sum(a.dt[, gain_top_pct_5]), 2),
    round(sum(a.dt[, gain_top_pct_1]), 2)
  )
  stake_wtd <- c(
    sum(a.dt[, strat_all_wtd]),
    sum(a.dt[, strat_fav_wtd]),
    sum(a.dt[, strat_out_wtd]),
    sum(a.dt[, strat_home_wtd]),
    sum(a.dt[, strat_draw_wtd]),
    sum(a.dt[, strat_away_wtd]),
    sum(a.dt[, strat_top_pct_10_wtd]),
    sum(a.dt[, strat_top_pct_5_wtd]),
    sum(a.dt[, strat_top_pct_1_wtd])
  )
  gain_wtd <- c(
    round(sum(a.dt[, gain_all_wtd]), 2),
    round(sum(a.dt[, gain_fav_wtd]), 2),
    round(sum(a.dt[, gain_out_wtd]), 2),
    round(sum(a.dt[, gain_home_wtd]), 2),
    round(sum(a.dt[, gain_draw_wtd]), 2),
    round(sum(a.dt[, gain_away_wtd]), 2),
    round(sum(a.dt[, gain_top_pct_10_wtd]), 2),
    round(sum(a.dt[, gain_top_pct_5_wtd]), 2),
    round(sum(a.dt[, gain_top_pct_1_wtd]), 2)
  )
  strat.dt <- t(data.frame(strategy=strategy, stake=stake, gain=gain, stake_wtd=stake_wtd, gain_wtd=gain_wtd))
  strat.p.obj <- tableGrob(strat.dt, theme=table.theme(12), cols=NULL)
  strat.p.obj <- colorise.tableGrob(strat.p.obj, strat.dt, "grey90", "grey95")
  strat.p.obj <- grid::grobTree(
      grid::textGrob(
        label="Strategy",
        # note font color goes to most recently used fill and not black???
        gp=grid::gpar(fontsize=16, fontface="bold", fill="black", col="black"),
        x=0.5,
        y=0.9
      ),
      grid::rectGrob(gp=grid::gpar(fill="grey90", lwd=2, col="black", alpha=0.5)),
      strat.p.obj
  )
  strat.p.obj
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
    train.a.dt[, list(dt="train.a", dtc="red", gain=sum(gain), gain_wtd=sum(gain_all_wtd), count=.N, weight=sum(weight)), decile],
    train.b.dt[, list(dt="train.b", dtc="blue", gain=sum(gain), gain_wtd=sum(gain_all_wtd), count=.N, weight=sum(weight)), decile],
    test.dt[, list(dt="test", dtc="green", gain=sum(gain), gain_wtd=sum(gain_all_wtd), count=.N, weight=sum(weight)), decile]
  )
  summary.dt[, gain := gain / weight]
  summary.dt[, gain_wtd := gain_wtd / weight]
  row_count <- summary.dt[, .N]
  summary.dt[, gain_rs := rebase.y(summary.dt[, weight], summary.dt[, gain], nreturn=row_count)]
  summary.dt[, gain_wtd_rs := rebase.y(summary.dt[, weight], summary.dt[, gain_wtd], nreturn=row_count)]
  plot.obj <- ggplot(summary.dt)
  plot.obj <- plot.obj + geom_bar(aes(x=decile, y=weight, color=dt, fill=dt), stat="identity", position="dodge", alpha=0.3)
  plot.obj <- plot.obj + geom_line(aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + geom_line(aes(x=decile, y=gain_wtd_rs, group=dt, color=dt), stat="identity", linetype="dotted")
  # plot.obj <- plot.obj + geom_point(aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + theme(
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=element_text(hjust=0.5),
    panel.border=element_rect(colour="black", fill=NA, size=2)
  )
  plot.obj <- plot.obj + ggtitle("actual and predicted gain by predicted deciles")
  plot.obj <- plot.obj + scale_y_continuous(
    name="weight",
    sec.axis=sec_axis(~ rebase.y(c(summary.dt[, gain], summary.dt[, gain_wtd]), .), name="gain, gain_wtd")
  )
  plot.obj
}

grid.square <- quote({
  grid::grid.rect(x=0.25, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.25, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
})

plot.detailed.strategy <- function(test.dt, upcoming.dt) {
  test.thin.dt <- test.dt[strat_top_pct_5_wtd > 0, list(
    match_id, ftr, actr, ip, odds, gbmp, strat_top_pct_5, gain_top_pct_5, strat_top_pct_5_wtd, gain_top_pct_5_wtd
    )][order(-gbmp)]
  upcoming.thin.dt <- upcoming.dt[strat_top_pct_5_wtd > 0, list(
    match_id, ftr, ip, odds, gbmp, strat_top_pct_5, strat_top_pct_5_wtd
    )][order(-gbmp)]
  p.obj.test <- tableGrob(test.thin.dt, rows=NULL)
  p.obj.test <- colorise.tableGrob(p.obj.test, test.thin.dt, "grey90", "grey95", 8)
  p.obj.upcoming <- tableGrob(upcoming.thin.dt, rows=NULL)
  p.obj.upcoming <- colorise.tableGrob(p.obj.upcoming, upcoming.thin.dt, "grey90", "grey95", 8)
  p <- list(grid::grobTree(p.obj.test), grid::grobTree(p.obj.upcoming))
  title.test <- grid::grobTree( 
      grid::textGrob(
        label=paste("test strategy top pct 5 n=", test.thin.dt[, .N]),
        gp=grid::gpar(fontsize=12, fontface="bold", fill="black", col="black"),
        x=0.5,
        y=0.9
      ))
  title.upcoming <- grid::grobTree( 
      grid::textGrob(
        label=paste("upcoming strategy top pct 5 n=", upcoming.thin.dt[, .N]),
        gp=grid::gpar(fontsize=12, fontface="bold", fill="black", col="black"),
        x=0.5,
        y=0.9
      ))
  grid.arrange(title.test, p.obj.test, nrow=2, ncol=1)
  grid.arrange(title.upcoming, p.obj.upcoming, nrow=2, ncol=1)
}

plot.model <- function(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, logfile) {
  pdffile <- gsub(".log", ".pdf", logfile)
  pdf(pdffile, h=7, w=14)
    grid.arrange(
      plot.model.run(model, train.a.dt, train.b.dt, test.dt),
      plot.model.perf(model),
      plot.var.importance(model),
      plot.decile.perf(train.a.dt, train.b.dt, test.dt)
    )
    eval(grid.square)
    plot.detailed.strategy(test.dt, upcoming.dt)
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

