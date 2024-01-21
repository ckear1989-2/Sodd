
#' @export
plot.data.perf <- function(model) {
  date <- match_id <- weight <- null_dev <- model_dev <- offset_dev <- NULL
  dts <- c(
    "train.a",
    "train.b",
    "test",
    "upcoming"
  )
  min.date <- c(
    min(model$train.a.dt[, date]),
    min(model$train.b.dt[, date]),
    min(model$test.dt[, date]),
    min(model$upcoming.dt[, date])
  )
  max.date <- c(
    max(model$train.a.dt[, date]),
    max(model$train.b.dt[, date]),
    max(model$test.dt[, date]),
    max(model$upcoming.dt[, date])
  )
  records <- c(
    model$train.a.dt[, .N],
    model$train.b.dt[, .N],
    model$test.dt[, .N],
    model$upcoming.dt[, .N]
  )
  matches <- c(
    length(unique(model$train.a.dt[, match_id])),
    length(unique(model$train.b.dt[, match_id])),
    length(unique(model$test.dt[, match_id])),
    length(unique(model$upcoming.dt[, match_id]))
  )
  null.dev <- c(
    round(sum(model$train.a.dt[, null_dev]) / sum(model$train.a.dt[, weight]), 4),
    round(sum(model$train.b.dt[, null_dev]) / sum(model$train.b.dt[, weight]), 4),
    round(sum(model$test.dt[, null_dev]) / sum(model$test.dt[, weight]), 4),
    NA
  )
  offset.dev <- c(
    round(sum(model$train.a.dt[, offset_dev]) / sum(model$train.a.dt[, weight]), 4),
    round(sum(model$train.b.dt[, offset_dev]) / sum(model$train.b.dt[, weight]), 4),
    round(sum(model$test.dt[, offset_dev]) / sum(model$test.dt[, weight]), 4),
    NA
  )
  model.dev <- c(
    round(sum(model$train.a.dt[, model_dev]) / sum(model$train.a.dt[, weight]), 4),
    round(sum(model$train.b.dt[, model_dev]) / sum(model$train.b.dt[, weight]), 4),
    round(sum(model$test.dt[, model_dev]) / sum(model$test.dt[, weight]), 4),
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
  p.obj.options <- list(
    cols=colnames(devs.dt),
    rowcs=c("gold", "gold3"),
    fs=6,
    bg_fill="gold",
    bg_color="black",
    bg_alpha=0.5,
    bg_linewidth=2

  )
  p.obj <- pretty.gtable::pretty_gtable(devs.dt, p.obj.options)
  p.obj
}

var.name.check <- function(model) {
  # make sure variables needed for plotting are in data.tables
  stopifnot(all(c(model$yvar, model$wvar, model$pvar, model$uvar) %in% colnames(model$train.a.dt)))
  stopifnot(all(c(model$yvar, model$wvar, model$pvar, model$uvar) %in% colnames(model$train.b.dt)))
  stopifnot(all(c(model$yvar, model$wvar, model$pvar, model$uvar) %in% colnames(model$train.dt)))
  stopifnot(all(c(model$yvar, model$wvar, model$pvar, model$uvar) %in% colnames(model$test.dt)))
  stopifnot(all(c(model$yvar, model$wvar, model$pvar, model$uvar) %in% colnames(model$upcoming.dt)))
  # make sure variables temp names needed for plotting are not in data.tables
  stopifnot(!any(c("x", "y", "w", "p") %in% colnames(model$train.a.dt)))
  stopifnot(!any(c("x", "y", "w", "p") %in% colnames(model$train.b.dt)))
  stopifnot(!any(c("x", "y", "w", "p") %in% colnames(model$train.dt)))
  stopifnot(!any(c("x", "y", "w", "p") %in% colnames(model$test.dt)))
  stopifnot(!any(c("x", "y", "w", "p") %in% colnames(model$upcoming.dt)))
  TRUE
}

#' @import gbm.doc
#' @export
plot.model.run <- function(model) {
  var.name.check(model)
  model.param.p.obj <- gbm.doc::plot_model_param(model)
  model.perf.p.obj <- plot.data.perf(model)
  strat.p.obj <- plot.strategies(model$test.dt)
  gs <- list(model.param.p.obj, model.perf.p.obj, strat.p.obj)
  lay <- rbind(
    c(1, 2),
    c(3, 3)
  )
  gridExtra::arrangeGrob(grobs=gs, layout_matrix=lay)
}

#' @export
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
  strat.dt <- data.frame(t(data.frame(strategy=strategy, stake=stake, gain=gain, stake_wtd=stake_wtd, gain_wtd=gain_wtd)))
  p.obj.options <- list(
    rows=rownames(strat.dt),
    rowcs=c("grey90", "grey95"),
    fs=10,
    bg_fill="grey90",
    bg_color="black",
    bg_alpha=0.5,
    bg_linewidth=2
  )
  strat.p.obj <- pretty.gtable::pretty_gtable(strat.dt, p.obj.options)
  strat.p.obj
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
  train.a.dt[, rn := seq(train.a.dt[, .N])]
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

#' @importFrom gtable gtable_add_grob
detailed.strat.gtable <- function(a.dt, recent.dt, aname) {
  actr <- NULL
  a.thin.dt <- detailed.strat.data.table(a.dt, recent.dt)
  this.label <- paste0(aname, " strategy top per match topn=", (a.thin.dt[, .N] -1))
  if(aname == "upcoming" & isTRUE(get.sodd.force.upcoming()) & (min(a.dt[, date]) < Sys.Date())) {
    this.label <- paste0(this.label, "\nwarning: upcoming fixtures may be forced.")
  }
  correct_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, ftr] == a.thin.dt[i, actr]) & (!a.thin.dt[i, actr] == "NA"))
  incorrect_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, ftr] != a.thin.dt[i, actr]) & (!a.thin.dt[i, actr] == "NA"))
  unknown_preds <- sapply(1:a.thin.dt[, .N], function(i) (a.thin.dt[i, actr] == "NA") | is.na(a.thin.dt[i, actr]))
  rowcs <- c("grey95") # title in light grey
  for (i in 1:a.thin.dt[, .N]) {
    if (isTRUE(correct_preds[[i]])) rowcs <- append(rowcs, "green")
    if (isTRUE(incorrect_preds[[i]])) rowcs <- append(rowcs, "red")
    if (isTRUE(unknown_preds[[i]])) rowcs <- append(rowcs, "grey95")
  }
  p.obj.options <- list(
    fs=16,
    rowcs=rowcs,
    rows=NULL,
    cols=colnames(a.thin.dt),
    title=this.label
  )
  p.obj <- pretty.gtable::pretty_gtable(a.thin.dt, p.obj.options)
  p.obj
}

#' @importFrom grid grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange
plot.detailed.strategy <- function(model, pngf, leagues=all.leagues) {
  # dload.current.year(quiet=TRUE)
  recent.dt <- get.recent.dt(leagues)
  p.obj.test <- detailed.strat.gtable(model$test.dt, recent.dt, "test")
  p.obj.upcoming <- detailed.strat.gtable(model$upcoming.dt, recent.dt, "upcoming")
  grDevices::png(pngf, width=800, height=600)
    gridExtra::grid.arrange(p.obj.upcoming)
  grDevices::dev.off()
  grid::grid.newpage(); grid::grid.draw(p.obj.test)
  grid::grid.newpage(); grid::grid.draw(p.obj.upcoming)
}

#' @import gbm.doc
get_uvar_list <- function(
  x,
  model
) {
  list(
    gbm.doc::univariate(model$train.a.dt, x, model$yvar, model$pvar, model$wvar),
    gbm.doc::univariate(model$train.b.dt, x, model$yvar, model$pvar, model$wvar),
    gbm.doc::univariate(model$test.dt, x, model$yvar, model$pvar, model$wvar),
    gbm.doc::partial.plot(model, x, model$train.dt)
  )
}

#' @importFrom gridExtra grid.arrange
parallel.uvar <- quote({
  doParallel::registerDoParallel(1)
  library("foreach")
  foreach(i=1:nrow(m), .combine=rbind)
  uvar_list <- foreach(u=model$uvar) %dopar% get_uvar_list(u, model)
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
#' @import gbm.doc
series.uvar <- quote({
  for (x in model$uvar) {
      gridExtra::grid.arrange(
      gbm.doc::univariate(model$train.a.dt, x, model$yvar, model$pvar, model$wvar),
      gbm.doc::univariate(model$train.b.dt, x, model$yvar, model$pvar, model$wvar),
      gbm.doc::univariate(model$test.dt, x, model$yvar, model$pvar, model$wvar),
      gbm.doc::partial.plot(model, x, model$train.dt),
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
plot.response.vars <- function(train.dt, test.dt, yvar, pvar) {
  y <- gbmp <- NULL
  setnames(train.dt, yvar, "y")
  setnames(test.dt, yvar, "y")
  setnames(train.dt, pvar, "p")
  setnames(test.dt, pvar, "p")
  gridExtra::grid.arrange(
    plot.dist(c(train.dt[, y], test.dt[, y]), yvar),
    plot.dist(c(train.dt[, p], test.dt[, p]), "model prediction"),
    nrow=2
  )
  setnames(train.dt, "y", yvar)
  setnames(test.dt, "y", yvar)
  setnames(train.dt, "p", pvar)
  setnames(test.dt, "p", pvar)
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

#' @importFrom pretty.gtable pretty_gtable
detailed.test.date.gtable <- function(a.dt) {
  actr <- NULL
  a.thin.dt <- detailed.test.date.data.table(a.dt)
  p.obj.options <- list(
    fs=16,
    cols=colnames(a.thin.dt),
    rowcs=c("grey", "grey95"),
    title=paste0("test", " strategy top per match for last ", (a.thin.dt[, .N] -1), " dates")
  )
  p.obj <- pretty.gtable::pretty_gtable(a.thin.dt, p.obj.options)
  p.obj
}

#' @importFrom grid grid.newpage grid.draw
test.dt.by.date <- function(test.dt) {
  p.obj.test <- detailed.test.date.gtable(test.dt)
  grid::grid.newpage(); grid::grid.draw(p.obj.test)
}

#' @importFrom gridExtra grid.arrange
#' @import gbm.doc
plot.model <- function(model) {
  div <- NULL
  if(is.null(model)) {
    warning("attempting to plot null model")
    return(NULL)
  }
  grDevices::pdf(model$pdffile, h=7, w=14)
  gridExtra::grid.arrange(
    plot.model.run(model),
    gbm.doc::plot_model_perf(model),
    gbm.doc::plot_var_importance(model),
    plot.decile.perf(model$train.a.dt, model$train.b.dt, model$test.dt)
  )
  eval(grid.square)
  test.dt.by.date(model$test.dt)
  pngf <- file.path(gsub(".pdf", "_strategy.png", model$pdffile))
  plot.detailed.strategy(model, pngf, unique(model$train.dt[, div]))
  plot.response.vars(model$train.dt, model$test.dt, model$yvar, model$pvar)
  # test parallelizing univar plots
  # seems to be no gain on my system
  # z <- Sys.time()
  # print(z)
  eval(series.uvar)
  # print(Sys.time() - z)
  grDevices::dev.off()
  paste("see model documentation in", model$pdffile)
}

