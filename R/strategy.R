
#' import from stats quantile
#' @param a.dt data.table to calculate strategies on
calc.strategies <- function(a.dt) {
  ip <- ftr <- hometeam <- awayteam <- pred_spread <- match_id <- strat_all <-
  strat_fav <- strat_out <- strat_home <- strat_draw <- strat_away <-
  strat_top_pct_10 <- strat_top_pct_5 <- strat_top_pct_1 <-
  strat_top_per_match <- strat_all_wtd <- strat_fav_wtd <- strat_out_wtd <-
  strat_home_wtd <- strat_draw_wtd <- strat_away_wtd <- strat_top_pct_10_wtd <-
  strat_top_pct_5_wtd <- strat_top_pct_1_wtd <- strat_top_per_match_wtd <-
  gain_all <- gain_fav <- gain_out <- gain_home <- gain_draw <- gain_away <-
  gain_top_pct_10 <- gain_top_pct_5 <- gain_top_pct_1 <- gain_top_per_match <-
  gain_all_wtd <- gain_fav_wtd <- gain_out_wtd <- gain_home_wtd <-
  gain_draw_wtd <- gain_away_wtd <- gain_top_pct_10_wtd <- gain_top_pct_5_wtd <-
  gain_top_pct_1_wtd <- gain_top_per_match_wtd <- maxp <- match_fav_count <-
  match_out_count <- max_match_pred <- max_match_flag <- max_match_flag_count <-
  pred_spread_one_per_match <- rn <- pct_grp_10 <- pct_grp_5 <- pct_grp_1 <-
  act <- odds <- min_ip <- gain_strat <- NULL
  choose_one_result <- function(id, ftr) {
    if(is.package.available("TeachingDemos")) {
      suppressWarnings(TeachingDemos::char2seed(id))
    }
    r <- sample(as.character(ftr), 1)
    ifelse(ftr == r, 1, 0)
  }

  # bet on every result sum(gain)
  a.dt[, strat_all := 1]
  # bet on all favourites
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  a.dt[, maxp := max(ip), match_id]
  a.dt[, strat_fav := 0]
  a.dt[ip == maxp, strat_fav := 1]
  # handle ties
  a.dt[, match_fav_count := sum(strat_fav), match_id]
  a.dt[(strat_fav == 1) & (match_fav_count > 1), strat_fav := choose_one_result(match_id, ftr), match_id]
  # bet on all outsiders
  a.dt[, min_ip := min(ip), match_id]
  a.dt[, strat_out := 0]
  a.dt[ip == min_ip, strat_out := 1]
  # handle ties
  a.dt[, match_out_count := sum(strat_out), match_id]
  a.dt[(strat_out == 1) & (match_out_count > 1), strat_out := choose_one_result(match_id, ftr), match_id]
  # bet on all home
  a.dt[, strat_home := 0]
  a.dt[ftr == "H", strat_home := 1]
  # bet on all draw
  a.dt[, strat_draw := 0]
  a.dt[ftr == "D", strat_draw := 1]
  # bet on all home
  a.dt[, strat_away := 0]
  a.dt[ftr == "A", strat_away := 1]

  if(a.dt[is.na(pred_spread), .N] > 0) stop("missing spread")
  a.dt[, max_match_pred := max(pred_spread), match_id]
  # handle ties
  a.dt[, max_match_flag:= 0]
  a.dt[pred_spread == max_match_pred, max_match_flag := 1]
  a.dt[, max_match_flag_count := sum(max_match_flag), match_id]
  a.dt[(max_match_flag == 1) & (max_match_flag_count > 1), max_match_flag := choose_one_result(match_id, ftr), match_id]
  a.dt[, pred_spread_one_per_match := -1]
  a.dt[max_match_flag == 1, pred_spread_one_per_match := pred_spread]

  a.dt[, strat_top_per_match := 0]
  a.dt[max_match_flag == 1, strat_top_per_match := 1]
  # top n % predictions
  setkey(a.dt, pred_spread_one_per_match)
  a.dt[, rn := seq(a.dt[, .N])]
  a.dt[, pct_grp_10 := -1]
  a.dt[, pct_grp_5 := -1]
  a.dt[, pct_grp_1 := -1]
  q10 <- unlist(quantile(a.dt[max_match_flag == 1, rn], probs=seq(0, 1, by=0.1)))
  q5 <- unlist(quantile(a.dt[max_match_flag == 1, rn], probs=seq(0, 1, by=0.05)))
  q1 <- unlist(quantile(a.dt[max_match_flag == 1, rn], probs=seq(0, 1, by=0.01)))
  is.unique <- function(x) {
    length(x) == length(unique(x))
  }
  if(!is.unique(q10)) q10 <- seq(1, a.dt[, .N])
  if(!is.unique(q5)) q5 <- seq(1, a.dt[, .N])
  if(!is.unique(q1)) q1 <- seq(1, a.dt[, .N])
  names(q10) <- NULL
  names(q5) <- NULL
  names(q1) <- NULL
  pprint(a.dt[, .N, max_match_flag], verbosity=2)
  pprint(q10, verbosity=2)
  # pprint(q5, verbosity=2)
  # pprint(q1, verbosity=2)
  cat0n(1:length(q10))
  cat0n(1:(length(q10)-1))
  # q()
  a.dt[max_match_flag == 1,
    pct_grp_10 := as.numeric(as.character(cut(
    a.dt[max_match_flag == 1, rn],
    breaks=q10,
    include.lowest=TRUE,
    labels=1:(length(q10)-1))))
  ]
  a.dt[max_match_flag == 1,
    pct_grp_5 := as.numeric(as.character(cut(
      a.dt[max_match_flag == 1, rn],
      breaks=q5,
      include.lowest=TRUE,
      labels=1:(length(q5)-1))))
  ]
  a.dt[max_match_flag == 1,
    pct_grp_1 := as.numeric(as.character(cut(
      a.dt[max_match_flag == 1, rn],
      breaks=q1,
      include.lowest=TRUE,
      labels=1:(length(q1)-1))))
  ]
  a.dt[, strat_top_pct_10 := 0]
  a.dt[, strat_top_pct_5 := 0]
  a.dt[, strat_top_pct_1 := 0]
  a.dt[pct_grp_10 == 10, strat_top_pct_10 := 1]
  a.dt[pct_grp_5 == 20, strat_top_pct_5 := 1]
  a.dt[pct_grp_1 == 100, strat_top_pct_1 := 1]

  # weighted by dist of model prediction
  a.dt[, strat_all_wtd := round(rebase.y.sum(
    a.dt[, strat_all], a.dt[, pred_spread]), 2)]
  a.dt[, strat_fav_wtd := round(rebase.y.sum(
    a.dt[, strat_fav], a.dt[, pred_spread]), 2)]
  a.dt[, strat_out_wtd := round(rebase.y.sum(
    a.dt[, strat_out], a.dt[, pred_spread]), 2)]
  a.dt[, strat_home_wtd := round(rebase.y.sum(
    a.dt[, strat_home], a.dt[, pred_spread]), 2)]
  a.dt[, strat_draw_wtd := round(rebase.y.sum(
    a.dt[, strat_draw], a.dt[, pred_spread]), 2)]
  a.dt[, strat_away_wtd := round(rebase.y.sum(
    a.dt[, strat_away], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_pct_10_wtd := round(rebase.y.sum(
    a.dt[, strat_top_pct_10], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_pct_5_wtd := round(rebase.y.sum(
    a.dt[, strat_top_pct_5], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_pct_1_wtd := round(rebase.y.sum(
    a.dt[, strat_top_pct_1], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_per_match_wtd := round(rebase.y.sum(
    a.dt[, strat_top_per_match], a.dt[, pred_spread]), 2)]
  # calculate predicted gain
  calc.gain <- function(strat, a.dt) {
    setnames(a.dt, paste0("strat_", strat), "strat")
    a.dt[, gain_strat := 0]
    a.dt[act != -1, gain_strat := (strat * odds * act) - strat]
    setnames(a.dt, "strat", paste0("strat_", strat))
    setnames(a.dt, "gain_strat", paste0("gain_", strat))
    a.dt
  }
  for (strat in strats) a.dt <- calc.gain(strat, a.dt)
  a.dt
}

run.strategy <- function(train.dt, train.a.dt, train.b.dt, test.dt, upcoming.dt) {
  gain_strat <- NULL
  cat0n(rep("#", 30), "\nStrategies", verbosity=2)
  train.a.dt <- calc.strategies(train.a.dt)
  train.b.dt <- calc.strategies(train.b.dt)
  test.dt <- calc.strategies(test.dt)
  upcoming.dt <- calc.strategies(upcoming.dt)
  cat_strat <- function(strat) {
    setnames(test.dt, paste0("strat_", strat), "strat")
    setnames(test.dt, paste0("gain_", strat), "gain_strat")
    cat0n(strat, ",", sum(test.dt[, strat]), ",", sum(test.dt[, gain_strat]), verbosity=2)
    setnames(test.dt, "strat", paste0("strat_", strat))
    setnames(test.dt, "gain_strat", paste0("gain_", strat))
  }
  cat0n("strategy,stake,gain", verbosity=2)
  for(strat in strats) cat_strat(strat)
  train.a.dt[, y := NULL]
  train.b.dt[, y := NULL]
  train.dt[, y := NULL]
  test.dt[, y := NULL]
  upcoming.dt[, y := NULL]
}

