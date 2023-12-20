
#' @import ggplot2
univariate <- function(a.dt, x) {
  act <- act_rs <- y <- pred <- gbmp <- count <- weight <- xn <- pred_rs <- NULL
  setnames(a.dt, x, "x")
  summary.dt <- a.dt[, list(
    act=sum(y), pred=sum(gbmp), count=.N, weight=sum(weight)
    ), x][order(-weight)]
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
    panel.border=ggplot2::element_rect(colour="black", fill=NA, linewidth=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle(paste(deparse(substitute(a.dt)), x))
  plot.obj <- plot.obj + ggplot2::xlab(x)
  if(row_count > 100) plot.obj <- plot.obj + annotate("text", x=15, y=max(summary.dt[, weight]), size=4, label=message)
  plot.obj
}

#' @import ggplot2
partial.plot <- function(model, x, a.dt) {
  weight <- y <- xn <- y_rs <- NULL
  # print(attributes(model))
  if (x %in% model$var.names) {
    x.dt <- data.table(plot.gbm(model, x, return.grid=TRUE))
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
    if(is.numeric(summary.dt[, x])) plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=xn, y=y_rs, group=1), stat="identity", color="green", linewidth=2)
    if(is.factor(summary.dt[, x])) plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x=xn, y=y_rs, group=1), stat="identity", color="green", size=8)
    if(range_y > 0) {
      plot.obj <- plot.obj + scale_y_continuous(
        name="weight",
        sec.axis=ggplot2::sec_axis(~ rebase.y(summary.dt[, y], .), name="partial")
      )
    } else {
      plot.obj <- plot.obj + scale_y_continuous(
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
      panel.border=ggplot2::element_rect(colour="black", fill=NA, linewidth=2)
    )
    plot.obj <- plot.obj + ggplot2::ggtitle(x)
    plot.obj <- plot.obj + ggplot2::xlab(x)
  } else {
    plot.obj <- ggplot2::ggplot() +
      annotate("text", x=4, y=25, size=4, label=paste("variable", x, "not modeled")) +
      ggplot2::theme_void() + ggplot2::theme(panel.border=ggplot2::element_rect(colour="black", fill=NA, linewidth=2))
  }
  plot.obj
}

#' @importFrom gridExtra arrangeGrob
plot.model.run <- function(model) {
  model.param.p.obj <- plot.model.param(model)
  model.perf.p.obj <- plot.data.perf(model$train.a.dt, model$train.b.dt, model$test.dt, model$upcoming.dt)
  strat.p.obj <- plot.strategies(model$test.dt)
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

#' @importFrom grid unit.c unit
padding <- function() grid::unit.c(grid::unit(2, "mm"), grid::unit(2, "mm"))

#' @importFrom gridExtra ttheme_default
table.theme <- function(fs) {
    gridExtra::ttheme_default(
    core=list(
      fg_params=list(fontsize=fs, just="left"),
      padding=padding()
    ),
    rowhead=list(
      fg_params=list(fontsize=fs, fontface="bold", just="left"),
      padding=padding()
    ),
    colhead=list(
      fg_params=list(fontsize=fs, fontface="bold", just="left"),
      padding=padding()
    )
  )
}

#' @importFrom grid gpar
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

#' @importFrom gridExtra tableGrob
#' @importFrom grid gpar grobTree rectGrob
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
  best.trees <- gbm.perf(model, plot.it=FALSE, method="test")
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

#' @importFrom gridExtra tableGrob
#' @importFrom grid gpar grobTree rectGrob
plot.data.perf <- function(train.a.dt, train.b.dt, test.dt, upcoming.dt) {
  date <- match_id <- weight <- null_dev <- model_dev <- offset_dev <- NULL
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
    round(sum(train.a.dt[, null_dev]) / sum(train.a.dt[, weight]), 4),
    round(sum(train.b.dt[, null_dev]) / sum(train.b.dt[, weight]), 4),
    round(sum(test.dt[, null_dev]) / sum(test.dt[, weight]), 4),
    NA
  )
  offset.dev <- c(
    round(sum(train.a.dt[, offset_dev]) / sum(train.a.dt[, weight]), 4),
    round(sum(train.b.dt[, offset_dev]) / sum(train.b.dt[, weight]), 4),
    round(sum(test.dt[, offset_dev]) / sum(test.dt[, weight]), 4),
    NA
  )
  model.dev <- c(
    round(sum(train.a.dt[, model_dev]) / sum(train.a.dt[, weight]), 4),
    round(sum(train.b.dt[, model_dev]) / sum(train.b.dt[, weight]), 4),
    round(sum(test.dt[, model_dev]) / sum(test.dt[, weight]), 4),
    NA
  )
  devs.dt <- data.frame(
    data=dts,
    min.date=min.date,
    max.date=max.date,
    records=records,
    matches=matches,
    null.dev=null.dev,
    offset.dev=offset.dev,
    model.dev=model.dev
  )
  setnames(devs.dt, colnames(devs.dt), gsub("\\.", "\n", colnames(devs.dt)))
  p.obj <- gridExtra::tableGrob(devs.dt, theme=table.theme(6), rows=NULL)
  p.obj <- colorise.tableGrob(p.obj, devs.dt, "gold", "gold3", 6)
  p.obj <- grid::grobTree(
      grid::rectGrob(gp=grid::gpar(fill="gold", lwd=2, col="black", alpha=0.5)),
      p.obj
  )
  p.obj
}

#' @importFrom gridExtra tableGrob
#' @importFrom grid gpar grobTree rectGrob
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

#' @import ggplot2
plot.model.perf <- function(model, train.a, train.b) {
  trees <- cv_rs <- cv <- x1 <- y1 <- x2 <- y2 <- NULL
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
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=trees, y=train.a), color="red", linewidth=2)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=trees, y=train.b), color="blue", linewidth=2)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x=trees, y=cv_rs), color="green", linewidth=2)
  best.y <- min(p.data[, train.b])
  best.x <- p.data[train.b == best.y, trees]
  df <- data.frame(x1=c(1, best.x), x2=c(best.x, best.x), y1=c(best.y, best.y), y2=c(best.y, -Inf))
  plot.obj <- plot.obj + ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, xend=x2, yend=y2), data=df, linetype="dashed")
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, linewidth=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("mean deviance on train.a, train.b and cv")
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name="train.a, train.b mean.deviance",
    sec.axis=ggplot2::sec_axis(~ rebase.y(p.data[, cv], .), name="cv mean.deviance")
  )
  plot.obj
}

#' @import ggplot2
#' @import data.table
plot.var.importance <- function(model) {
  sv <- rel.inf <- x <- var <- NULL
  p.dt <- data.table(summary(model, plotit=FALSE))
  p.dt[, sv := - rel.inf]
  setkey(p.dt, sv)
  p.dt[, x := seq(p.dt[, .N])]
  plot.obj <- ggplot2::ggplot(p.dt)
  plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x=x, y=rel.inf), stat="identity", color="yellow", fill="yellow", alpha=0.3)
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, linewidth=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("modeled variables relative influence")
  plot.obj <- plot.obj + ggplot2::ylab("relative influence")
  plot.obj <- plot.obj + ggplot2::xlab("modeled variable")
  breaks <- p.dt[, x]
  labels <- p.dt[, var]
  if(length(breaks) != length(labels)) {
    print(p.dt)
    print(breaks)
    print(labels)
    stop("length breaks labels differ")
  }
  plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks=breaks, labels=labels)
  plot.obj
}

#' @import ggplot2
#' @importFrom stats quantile
plot.decile.perf <- function(train.a.dt, train.b.dt, test.dt) {
  gbmp <- decile <- rn <- gain <- gain_wtd <- gain_all_wtd <- count <-
  weight <- dt <- gain_rs <- gain_wtd_rd <- gain_wtd_rs <- NULL
  # sort predictions into deciles
  setkey(train.a.dt, gbmp)
  setkey(train.b.dt, gbmp)
  setkey(test.dt, gbmp)
  if(train.a.dt[is.na(gbmp), .N] > 0) {
    print(train.a.dt[is.na(gbmp), ])
    stop("train a na model predictions")
  }
  if(train.b.dt[is.na(gbmp), .N] > 0) {
    print(train.b.dt[is.na(gbmp), ])
    stop("train b na model predictions")
  }
  if(test.dt[is.na(gbmp), .N] > 0) {
    print(test.dt[is.na(gbmp), ])
    stop("test na model predictions")
  }
  train.a.dt[, rn := seq(train.a.dt[, .N])]
  train.b.dt[, rn := seq(train.b.dt[, .N])]
  test.dt[, rn := seq(test.dt[, .N])]
  train.a.dt[, decile := as.numeric(as.character(cut(
    train.a.dt[, rn], breaks=quantile(train.a.dt[, rn],
    probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  train.b.dt[, decile := as.numeric(as.character(cut(
    train.b.dt[, rn], breaks=quantile(train.b.dt[, rn],
    probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  test.dt[, decile := as.numeric(as.character(cut(
    test.dt[, rn], breaks=quantile(test.dt[, rn],
    probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  summary.dt <- rbind(
    train.a.dt[, list(
      dt="train.a", dtc="red", gain=sum(gain), gain_wtd=sum(gain_all_wtd),
      count=.N, weight=sum(weight)), decile],
    train.b.dt[, list(
      dt="train.b", dtc="blue", gain=sum(gain), gain_wtd=sum(gain_all_wtd),
      count=.N, weight=sum(weight)), decile],
    test.dt[, list(
      dt="test", dtc="green", gain=sum(gain), gain_wtd=sum(gain_all_wtd),
      count=.N, weight=sum(weight)), decile]
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
  # plot.obj <- plot.obj + geom_point(aes(x=decile, y=gain_rs, group=dt, color=dt), stat="identity")
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.5),
    plot.title=ggplot2::element_text(hjust=0.5),
    panel.border=ggplot2::element_rect(colour="black", fill=NA, linewidth=2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("actual and predicted gain by predicted deciles")
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name="weight",
    sec.axis=ggplot2::sec_axis(~ rebase.y(c(summary.dt[, gain], summary.dt[, gain_wtd]), .), name="gain, gain_wtd")
  )
  plot.obj
}

#' @importFrom grid grid.rect gpar
grid.square <- quote({
  grid::grid.rect(x=0.25, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.25, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.25, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
  grid::grid.rect(x=0.75, y=0.75, width=0.50, height=0.50, gp=grid::gpar(lwd=5, col="black", fill=NA))
})

detailed.strat.data.table <- function(a.dt, recent.dt) {
  match_id <- ftr <- actr <- ip <- odds <- pred_odds <-
  actr_new <- strat_top_per_match <- pred_spread <-
  gain_top_per_match <- expected_return <- NULL
  a.thin.dt <- a.dt[strat_top_per_match > 0, list(
    match_id,
    div,
    ftr,
    actr,
    ip,
    odds=round(odds, 3),
    pred_odds=round(pred_odds, 3),
    pred_spread=round(pred_spread, 3),
    strat_top_per_match,
    gain_top_per_match=round(gain_top_per_match, 3),
    expected_return=round(er, 2)
    )][order(-expected_return)]
  if(a.thin.dt[, .N] > 16) a.thin.dt <- a.thin.dt[1:16, ]
  # check if updated recent fixtures has full time scores
  if(all((is.na(a.thin.dt[, actr])) | (a.thin.dt[, actr] == "NA"))) {
    setkey(a.thin.dt, match_id)
    setkey(recent.dt, match_id)
    a.thin.dt <- merge(a.thin.dt, recent.dt, all.x=TRUE, all.y=FALSE, by="match_id")[order(-pred_spread)]
    a.thin.dt[, actr := actr_new]
    a.thin.dt[, actr_new := NULL]
  }
  a.thin.dt <- rbind(
    a.thin.dt,
    data.table(
      match_id="total",
      strat_top_per_match=sum(a.thin.dt[, strat_top_per_match]),
      gain_top_per_match=sum(a.thin.dt[, gain_top_per_match]),
      expected_return=sum(a.thin.dt[, expected_return])
   ), fill=TRUE)
  setnames(a.thin.dt, colnames(a.thin.dt), gsub("_", "\n", colnames(a.thin.dt)))
  a.thin.dt
}

set_row_border <- function(obj, row, color) {
  # row + 1 because of header
    gtable::gtable_add_grob(obj, grobs=grid::rectGrob(gp=grid::gpar(fill=color, lwd=2, col=color, alpha=0.5)), t=(row+1.02), b=(row+1.98), l=1.02, r=(ncol(obj)+1))
}

#' @importFrom gridExtra tableGrob
#' @importFrom gtable gtable_add_grob
#' @importFrom grid gpar grobTree rectGrob textGrob
detailed.strat.gtable <- function(a.dt, recent.dt, aname) {
  actr <- NULL
  a.thin.dt <- detailed.strat.data.table(a.dt, recent.dt)
  p.obj <- gridExtra::tableGrob(a.thin.dt, theme=table.theme(16), rows=NULL)
  p.obj <- colorise.tableGrob(p.obj, a.thin.dt, "grey90", "grey95", 16)
  correct_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, ftr] == a.thin.dt[i, actr]) & (!a.thin.dt[i, actr] == "NA"))
  incorrect_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, ftr] != a.thin.dt[i, actr]) & (!a.thin.dt[i, actr] == "NA"))
  unknown_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, actr] == "NA") | is.na(a.thin.dt[i, actr]))
  p.obj <- set_row_border(p.obj, 0, "black")
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
      y=0.92,
    ),
    p.obj
  )
  p.obj
}

#' @importFrom grid grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange
plot.detailed.strategy <- function(test.dt, upcoming.dt, pngf, leagues=all.leagues) {
  # dload.current.year(quiet=TRUE)
  recent.dt <- get.recent.dt(leagues)
  p.obj.test <- detailed.strat.gtable(test.dt, recent.dt, "test")
  p.obj.upcoming <- detailed.strat.gtable(upcoming.dt, recent.dt, "upcoming")
  grDevices::png(pngf, width=800, height=600)
  gridExtra::grid.arrange(p.obj.upcoming)
  grDevices::dev.off()
  grid::grid.newpage(); grid::grid.draw(p.obj.test)
  grid::grid.newpage(); grid::grid.draw(p.obj.upcoming)
}

get_uvar_list <- function(
  x,
  train.dt,
  train.a.dt,
  train.b.dt,
  test.dt,
  model
) {
  list(
    univariate(train.a.dt, x),
    univariate(train.b.dt, x),
    univariate(test.dt, x),
    partial.plot(model, x, train.dt)
  )
}

#' @importFrom gridExtra grid.arrange
parallel.uvar <- quote({
  doParallel::registerDoParallel(1)
  library("foreach")
  foreach(i=1:nrow(m), .combine=rbind)
  uvar_list <- foreach(u=uvar) %dopar% get_uvar_list(u, train.dt, train.a.dt, train.b.dt, test.dt, model)
  for(plist in uvar_list) {
      gridExtra::grid.arrange(
      plist[[1]], plist[[2]], plist[[3]], plist[[4]],
      ncol=2
    )
    eval(grid.square)
  }
  doParallel::stopImplicitCluster()
})

#' @importFrom gridExtra grid.arrange
series.uvar <- quote({
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
})

#' @import ggplot2
plot.dist <- function(x, xlabel) {
  w <- (max(x) - min(x)) / 100
  g <- ggplot2::ggplot(data=NULL)
  g <- g + geom_histogram(ggplot2::aes(x=x), fill="yellow", alpha=0.3, binwidth=w)
  g <- g + xlab(xlabel) + ggplot2::ggtitle(paste("distribution of", xlabel))
  g
}

#' @importFrom gridExtra grid.arrange
plot.response.vars <- function(train.dt, test.dt, yvar) {
  y <- gbmp <- NULL
  gridExtra::grid.arrange(
    plot.dist(c(train.dt[, y], test.dt[, y]), yvar),
    plot.dist(c(train.dt[, gbmp], test.dt[, gbmp]), "model prediction"),
    nrow=2
  )
}

detailed.test.date.data.table <- function(a.dt) {
  strat_top_per_match<- gain_top_per_match <- expected_return <- NULL
  a.thin.dt <- a.dt[strat_top_per_match > 0, list(
    strat_top_per_match=round(sum(strat_top_per_match, 2)),
    gain_top_per_match=round(sum(gain_top_per_match), 3),
    expected_return=round(sum(er), 2)
    ), date][order(date)]
  if(a.thin.dt[, .N] > 16) a.thin.dt <- a.thin.dt[1:16, ]
  a.thin.dt[, date := as.character(date)]
  a.thin.dt <- rbind(
    a.thin.dt,
    data.table(
      date="total",
      strat_top_per_match=sum(a.thin.dt[, strat_top_per_match]),
      gain_top_per_match=sum(a.thin.dt[, gain_top_per_match]),
      expected_return=sum(a.thin.dt[, expected_return])
   ), fill=TRUE)
  setnames(a.thin.dt, colnames(a.thin.dt), gsub("_", "\n", colnames(a.thin.dt)))
  a.thin.dt
}

detailed.test.date.gtable <- function(a.dt) {
  actr <- NULL
  a.thin.dt <- detailed.test.date.data.table(a.dt)
  p.obj <- gridExtra::tableGrob(a.thin.dt, theme=table.theme(16), rows=NULL)
  p.obj <- colorise.tableGrob(p.obj, a.thin.dt, "grey90", "grey95", 16)
  # p.obj <- set_row_border(p.obj, 0, "black")
  for (i in 0:a.thin.dt[, .N]) {
    p.obj <- set_row_border(p.obj, i, "black")
  }
  p.obj <- grid::grobTree(
    grid::rectGrob(gp=grid::gpar(fill="grey90", lwd=0, col="black", alpha=0.5)),
    grid::textGrob(
      label=paste0("test", " strategy top per match for last ", (a.thin.dt[, .N] -1), " dates"),
      gp=grid::gpar(fontsize=16, fontface="bold", fill="black", col="black"),
      x=0.5,
      y=0.92,
    ),
    p.obj
  )
  p.obj
}

#' @importFrom grid grid.newpage grid.draw
test.dt.by.date <- function(test.dt) {
  p.obj.test <- detailed.test.date.gtable(test.dt)
  grid::grid.newpage(); grid::grid.draw(p.obj.test)
}

#' @importFrom gridExtra grid.arrange
plot.model <- function(model, adate, train.a.dt, train.b.dt, train.dt, test.dt, upcoming.dt, uvar, yvar, pdffile) {
  div <- NULL
  if(is.null(model)) {
    warning("attempting to plot null model")
    return(NULL)
  }
  grDevices::pdf(pdffile, h=7, w=14)
  gridExtra::grid.arrange(
    plot.model.run(model),
    plot.model.perf(model, train.a.dt, train.b.dt),
    plot.var.importance(model),
    plot.decile.perf(train.a.dt, train.b.dt, test.dt)
  )
  eval(grid.square)
  test.dt.by.date(test.dt)
  pngf <- file.path(gsub(".pdf", "_strategy.png", pdffile))
  plot.detailed.strategy(test.dt, upcoming.dt, pngf, unique(train.dt[, div]))
  plot.response.vars(train.dt, test.dt, yvar)
  # test parallelizing univar plots
  # seems to be no gain on my system
  # z <- Sys.time()
  # print(z)
  eval(series.uvar)
  # print(Sys.time() - z)
  grDevices::dev.off()
  paste("see model documentation in", pdffile)
}

