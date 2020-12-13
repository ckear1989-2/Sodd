
weight_from_season <- function(s) {
  o <- rep(1, length(s))
  o[s =="1011"] <- 0.1
  o[s =="1112"] <- 0.2
  o[s =="1213"] <- 0.3
  o[s =="1314"] <- 0.4
  o[s =="1415"] <- 0.5
  o[s =="1516"] <- 0.6
  o[s =="1617"] <- 0.7
  o[s =="1718"] <- 0.8
  o[s =="1819"] <- 0.9
  o[s =="1920"] <- 1.0
  o[s =="2021"] <- 1.1
  o
}

rebase.y <- function(y1, y2, nreturn=length(y2), verbose=FALSE) {
  # stretch y2 to range of y1
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
    print(all(c(min(new_y2b) == 0, max(new_y2b) == 1)))
    print(all(c(min(new_y2c) == 0, max(new_y2c) == range_y1)))
    print(all(c(min(new_y2) == min_y1, max(new_y2) == max_y1)))
  }
  new_y2[1:nreturn]
}

rebase.y.sum <- function(y1, y2) {
  # sum(new_y2) = sum(y1).  order maintained ignoring zeros
  if(all(y2 < 0)) y2 <- (1/abs(y2))
  sum_y1 <- sum(y1[!((y1 == 0) | (y2 == 0))])
  sum_y2 <- sum(y2[!((y1 == 0) | (y2 == 0))])
  new_y2 <- y2 * sum_y1 / sum_y2 # sums are same
  new_y2[(y1 == 0) | (y2 == 0)] <- 0
  new_y2
}

