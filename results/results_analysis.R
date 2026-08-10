#==================================================================
# script to compile results for the M paper
# EJ
# 20260709
#==================================================================

#------------------------------------------------------------------
# load
#------------------------------------------------------------------

library(data.table)
library(lattice)
load("results_dt.rda")

#------------------------------------------------------------------
# corcondance correlation coeficient
#------------------------------------------------------------------

ccc_dt <- results[, {
  vx <- var(mp); vy <- var(om)
  cxy <- cov(mp, om)
  mx <- mean(mp); my <- mean(om)
  .(ccc = round(2 * cxy / (vx + vy + (mx - my)^2), 2), ccc_thr = (2 * cxy / (vx + vy + (mx - my)^2))>=0.95, n = .N)
}, by = .(stock, metric, mp_name, om_name)]

xyplot(ccc~mp_name|stock*om_name, groups="metric", data=ccc_dt)

ccc_wide <- dcast(
  ccc_dt,
  stock + metric + mp_name ~ om_name,
  value.var = "ccc"
)

