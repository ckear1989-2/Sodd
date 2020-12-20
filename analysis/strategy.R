
source("utils/utils.R")
source("data_prep/constants.R")

calc.strategies <- function(a.dt) {

  choose_one_result <- function(id, ftr) {
    suppressWarnings(TeachingDemos::char2seed(id))
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
  a.dt[max_match_flag == 1, pct_grp_10 := as.numeric(as.character(cut(a.dt[max_match_flag == 1, rn], breaks=quantile(a.dt[max_match_flag == 1, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)))]
  a.dt[max_match_flag == 1, pct_grp_5 := as.numeric(as.character(cut(a.dt[max_match_flag == 1, rn], breaks=quantile(a.dt[max_match_flag == 1, rn], probs=seq(0, 1, by=0.05)), include.lowest=TRUE, labels=1:20)))]
  a.dt[max_match_flag == 1, pct_grp_1 := as.numeric(as.character(cut(a.dt[max_match_flag == 1, rn], breaks=quantile(a.dt[max_match_flag == 1, rn], probs=seq(0, 1, by=0.01)), include.lowest=TRUE, labels=1:100)))]
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
