
source("utils/utils.R")

calc.strategies <- function(a.dt) {

  # bet on every result sum(gain)
  a.dt[, strat_all := 1]
  # bet on all favourites
  a.dt[, max_ip := max(ip), list(date, hometeam, awayteam)]
  a.dt[, strat_fav := 0]
  a.dt[ip == max_ip, strat_fav := 1]
  # bet on all outsiders
  a.dt[, min_ip := min(ip), list(date, hometeam, awayteam)]
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

  a.dt[, gain_all := (strat_all * odds * act) - strat_all]
  a.dt[, gain_fav := (strat_fav * odds * act) - strat_fav]
  a.dt[, gain_out := (strat_out * odds * act) - strat_out]
  a.dt[, gain_home := (strat_home * odds * act) - strat_home]
  a.dt[, gain_draw := (strat_draw * odds * act) - strat_draw]
  a.dt[, gain_away := (strat_away * odds * act) - strat_away]

  # top n % predictions
  a.dt[, max_match_pred := max(gbmp), match_id]
  a.dt[, max_match_flag:= 0]
  a.dt[gbmp == max_match_pred, max_match_flag := 1]
  a.dt[, gbmp_one_per_match := -Inf]
  a.dt[gbmp == max_match_pred, gbmp_one_per_match := gbmp]
  print(a.dt[, list(match_flag_count=sum(max_match_flag)), match_id][match_flag_count > 1, ])
  setkey(a.dt, gbmp_one_per_match)
  a.dt[, rn := seq(a.dt[, .N])]
  a.dt[, pct_grp_10 := cut(a.dt[, rn], breaks=quantile(a.dt[, rn], probs=seq(0, 1, by=0.1)), include.lowest=TRUE, labels=1:10)]
  a.dt[, pct_grp_5 := cut(a.dt[, rn], breaks=quantile(a.dt[, rn], probs=seq(0, 1, by=0.05)), include.lowest=TRUE, labels=1:20)]
  a.dt[, pct_grp_1 := cut(a.dt[, rn], breaks=quantile(a.dt[, rn], probs=seq(0, 1, by=0.01)), include.lowest=TRUE, labels=1:100)]
  a.dt[, strat_top_pct_10 := 0]
  a.dt[, strat_top_pct_5 := 0]
  a.dt[, strat_top_pct_1 := 0]
  a.dt[pct_grp_10 == 10, strat_top_pct_10 := 1]
  a.dt[pct_grp_5 == 20, strat_top_pct_5 := 1]
  a.dt[pct_grp_1 == 100, strat_top_pct_1 := 1]
  a.dt[, gain_top_pct_10 := (strat_top_pct_10 * odds * act) - strat_top_pct_10]
  a.dt[, gain_top_pct_5 := (strat_top_pct_5 * odds * act) - strat_top_pct_5]
  a.dt[, gain_top_pct_1 := (strat_top_pct_1 * odds * act) - strat_top_pct_1]

  # weighted by dist of gbmp
  a.dt[, strat_all_wtd := round(rebase.y.sum(a.dt[, strat_all], a.dt[, gbmp]), 2)]
  a.dt[, strat_fav_wtd := round(rebase.y.sum(a.dt[, strat_fav], a.dt[, gbmp]), 2)]
  a.dt[, strat_out_wtd := round(rebase.y.sum(a.dt[, strat_out], a.dt[, gbmp]), 2)]
  a.dt[, strat_home_wtd := round(rebase.y.sum(a.dt[, strat_home], a.dt[, gbmp]), 2)]
  a.dt[, strat_draw_wtd := round(rebase.y.sum(a.dt[, strat_draw], a.dt[, gbmp]), 2)]
  a.dt[, strat_away_wtd := round(rebase.y.sum(a.dt[, strat_away], a.dt[, gbmp]), 2)]
  a.dt[, strat_top_pct_10_wtd := round(rebase.y.sum(a.dt[, strat_top_pct_10], a.dt[, gbmp]), 2)]
  a.dt[, strat_top_pct_5_wtd := round(rebase.y.sum(a.dt[, strat_top_pct_5], a.dt[, gbmp]),2)]
  a.dt[, strat_top_pct_1_wtd := round(rebase.y.sum(a.dt[, strat_top_pct_1], a.dt[, gbmp]), 2)]

  a.dt[, gain_all_wtd  := (strat_all_wtd * odds * act) - strat_all_wtd]
  a.dt[, gain_fav_wtd  := (strat_fav_wtd * odds * act) - strat_fav_wtd]
  a.dt[, gain_out_wtd  := (strat_out_wtd * odds * act) - strat_out_wtd]
  a.dt[, gain_home_wtd  := (strat_home_wtd * odds * act) - strat_home_wtd]
  a.dt[, gain_draw_wtd  := (strat_draw_wtd * odds * act) - strat_draw_wtd]
  a.dt[, gain_away_wtd  := (strat_away_wtd * odds * act) - strat_away_wtd]
  a.dt[, gain_top_pct_10_wtd  := (strat_top_pct_10_wtd * odds * act) - strat_top_pct_10_wtd]
  a.dt[, gain_top_pct_5_wtd  := (strat_top_pct_5_wtd * odds * act) - strat_top_pct_5_wtd]
  a.dt[, gain_top_pct_1_wtd  := (strat_top_pct_1_wtd * odds * act) - strat_top_pct_1_wtd]

  a.dt
}

run.strategy <- function(train.a.dt, train.b.dt, test.dt, upcoming.dt) {
  cat0n(rep("#", 30), "\nStrategies")
  train.a.dt <- calc.strategies(train.a.dt)
  train.b.dt <- calc.strategies(train.b.dt)
  test.dt <- calc.strategies(test.dt)
  upcoming.dt <- calc.strategies(upcoming.dt)
  cat0n("strategy,stake,gain")
  cat0n("all_results,", test.dt[, .N], ",", sum(test.dt[, gain_all]))
  cat0n("all_fav,", sum(test.dt[, strat_fav]), ",", sum(test.dt[, gain_fav]))
  cat0n("all_out,", sum(test.dt[, strat_out]), ",", sum(test.dt[, gain_out]))
  cat0n("all_home,", sum(test.dt[, strat_home]), ",", sum(test.dt[, gain_home]))
  cat0n("all_draw,", sum(test.dt[, strat_draw]), ",", sum(test.dt[, gain_draw]))
  cat0n("all_away,", sum(test.dt[, strat_away]), ",", sum(test.dt[, gain_away]))
  cat0n("top_pct_10,", sum(test.dt[, strat_top_pct_10]), ",", sum(test.dt[, gain_top_pct_10]))
  cat0n("top_pct_5,", sum(test.dt[, strat_top_pct_5]), ",", sum(test.dt[, gain_top_pct_5]))
  cat0n("top_pct_1,", sum(test.dt[, strat_top_pct_1]), ",", sum(test.dt[, gain_top_pct_1]))
  cat0n("all_results_wtd,", sum(test.dt[, strat_all_wtd]), ",", sum(test.dt[, gain_all_wtd]))
  cat0n("all_fav_wtd,", sum(test.dt[, strat_fav_wtd]), ",", sum(test.dt[, gain_fav_wtd]))
  cat0n("all_out_wtd,", sum(test.dt[, strat_out_wtd]), ",", sum(test.dt[, gain_out_wtd]))
  cat0n("all_home_wtd,", sum(test.dt[, strat_home_wtd]), ",", sum(test.dt[, gain_home_wtd]))
  cat0n("all_draw_wtd,", sum(test.dt[, strat_draw_wtd]), ",", sum(test.dt[, gain_draw_wtd]))
  cat0n("all_away_wtd,", sum(test.dt[, strat_away_wtd]), ",", sum(test.dt[, gain_away_wtd]))
  cat0n("top_pct_10_wtd,", sum(test.dt[, strat_top_pct_10_wtd]), ",", sum(test.dt[, gain_top_pct_10_wtd]))
  cat0n("top_pct_5_wtd,", sum(test.dt[, strat_top_pct_5_wtd]), ",", sum(test.dt[, gain_top_pct_5_wtd]))
  cat0n("top_pct_1_wtd,", sum(test.dt[, strat_top_pct_1_wtd]), ",", sum(test.dt[, gain_top_pct_1_wtd]))
  print(test.dt[strat_top_pct_5_wtd > 0, ][, list(match_id, ftr, actr, act, ip, odds, strat_top_pct_5_wtd, gbmp, y, gain, spread, gain_top_pct_5_wtd)])
  print(upcoming.dt[strat_top_pct_5_wtd > 0, ][, list(match_id, ftr, actr, act, ip, odds, strat_top_pct_5_wtd, gbmp, y, gain, spread, gain_top_pct_5_wtd)])
}
