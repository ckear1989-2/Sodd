
source("utils/utils.R")
source("data_prep/constants.R")

calc.strategies <- function(a.dt) {

  # bet on every result sum(gain)
  a.dt[, strat_all := 1]
  # bet on all favourites
  if(a.dt[is.na(ip), .N] > 0) stop("missing ip")
  a.dt[, max_ip := max(ip), match_id]
  a.dt[, strat_fav := 0]
  a.dt[ip == max_ip, strat_fav := 1]
  # bet on all outsiders
  a.dt[, min_ip := min(ip), match_id]
  a.dt[, strat_out := 0]
  a.dt[ip == min_ip, strat_out := 1]
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
  a.dt[, max_i_match_pred := max(pred_spread), match_id]
  # handle ties
  a.dt[, max_i_match_flag:= 0]
  a.dt[pred_spread == max_i_match_pred, max_i_match_flag := 1]
  choose_one_result <- function(id, ftr) {
    suppressWarnings(TeachingDemos::char2seed(id))
    r <- sample(as.character(ftr), 1)
    ifelse(ftr == r, 1, 0)
  }
  a.dt[, max_i_match_flag_count := sum(max_i_match_flag), match_id]
  a.dt[(max_i_match_flag == 1) & (max_i_match_flag_count > 1), max_i_match_flag := choose_one_result(match_id, ftr), match_id]
  a.dt[, max_i_match_flag_count := sum(max_i_match_flag), match_id]
  a.dt[, pred_spread_one_per_match := -Inf]
  a.dt[max_i_match_flag == 1, pred_spread_one_per_match := pred_spread]

  a.dt[, strat_top_per_match := 0]
  a.dt[max_i_match_flag == 1, strat_top_per_match := 1]
  # top n % predictions
  setkey(a.dt, pred_spread_one_per_match)
  a.dt[, rn := seq(a.dt[, .N])]
  breaks_pct_grp_10 <- quantile(a.dt[, rn], probs=seq(0, 1, by=0.1))
  breaks_pct_grp_5 <- quantile(a.dt[, rn], probs=seq(0, 1, by=0.05))
  breaks_pct_grp_1 <- quantile(a.dt[, rn], probs=seq(0, 1, by=0.01))
  if(any(is.na(breaks_pct_grp_10))) {
    print(a.dt)
    stop("na breaks")
  }
  a.dt[, pct_grp_10 := cut(a.dt[, rn], breaks=quantile(a.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)]
  a.dt[, pct_grp_5 := cut(a.dt[, rn], breaks=quantile(a.dt[, rn], probs=seq(0, 1, by=0.05)), include.lowest=TRUE, labels=1:20)]
  a.dt[, pct_grp_1 := cut(a.dt[, rn], breaks=quantile(a.dt[, rn], probs=seq(0, 1, by=0.01)), include.lowest=TRUE, labels=1:100)]
  a.dt[, strat_top_pct_10 := 0]
  a.dt[, strat_top_pct_5 := 0]
  a.dt[, strat_top_pct_1 := 0]
  a.dt[pct_grp_10 == 10, strat_top_pct_10 := 1]
  a.dt[pct_grp_5 == 20, strat_top_pct_5 := 1]
  a.dt[pct_grp_1 == 100, strat_top_pct_1 := 1]

  # weighted by dist of model prediction
  a.dt[, strat_all_wtd := round(rebase.y.sum(a.dt[, strat_all], a.dt[, pred_spread]), 2)]
  a.dt[, strat_fav_wtd := round(rebase.y.sum(a.dt[, strat_fav], a.dt[, pred_spread]), 2)]
  a.dt[, strat_out_wtd := round(rebase.y.sum(a.dt[, strat_out], a.dt[, pred_spread]), 2)]
  a.dt[, strat_home_wtd := round(rebase.y.sum(a.dt[, strat_home], a.dt[, pred_spread]), 2)]
  a.dt[, strat_draw_wtd := round(rebase.y.sum(a.dt[, strat_draw], a.dt[, pred_spread]), 2)]
  a.dt[, strat_away_wtd := round(rebase.y.sum(a.dt[, strat_away], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_pct_10_wtd := round(rebase.y.sum(a.dt[, strat_top_pct_10], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_pct_5_wtd := round(rebase.y.sum(a.dt[, strat_top_pct_5], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_pct_1_wtd := round(rebase.y.sum(a.dt[, strat_top_pct_1], a.dt[, pred_spread]), 2)]
  a.dt[, strat_top_per_match_wtd := round(rebase.y.sum(a.dt[, strat_top_per_match], a.dt[, pred_spread]), 2)]
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

run.strategy <- function(train.a.dt, train.b.dt, test.dt, upcoming.dt) {
  resample::cat0n(rep("#", 30), "\nStrategies")
  train.a.dt <- calc.strategies(train.a.dt)
  train.b.dt <- calc.strategies(train.b.dt)
  test.dt <- calc.strategies(test.dt)
  upcoming.dt <- calc.strategies(upcoming.dt)
  cat_strat <- function(strat) {
    setnames(test.dt, paste0("strat_", strat), "strat")
    setnames(test.dt, paste0("gain_", strat), "gain_strat")
    resample::cat0n(strat, ",", sum(test.dt[, strat]), ",", sum(test.dt[, gain_strat]))
    setnames(test.dt, "strat", paste0("strat_", strat))
    setnames(test.dt, "gain_strat", paste0("gain_", strat))
  }
  resample::cat0n("strategy,stake,gain")
  for(strat in strats) cat_strat(strat)
}
