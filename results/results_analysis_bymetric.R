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
# convergence (filter)
#------------------------------------------------------------------

conv.dt <- results[metric=="F",.(conv = 1 - mean(is.na(mp))), by = .(om_name, mp_name, stock)]
conv.dt[, wt_om := conv / sum(conv), by = .(stock, om_name)]
conv.dt[, wt_mp := conv / sum(conv), by = .(stock, mp_name)]

# remove non-convergence
to_remove <- results[metric == "F" & is.na(mp), .(iter, mp_name, om_name, stock)]
results <- results[!to_remove, on = .(iter, mp_name, om_name, stock)]

#------------------------------------------------------------------
# stats to compute regret
#------------------------------------------------------------------

results[, `:=`(lmp = log(mp), lom = log(om))]
# extension, weight bias and dispersion
wt <- 0.5

summ_metrics <- results[, {
  n   <- .N
  d   <- lmp - lom
  mx  <- mean(lmp)
  my  <- mean(lom)
  sxy <- mean((lmp - mx) * (lom - my))
  sx2 <- mean((lmp - mx)^2)
  sy2 <- mean((lom - my)^2)
  .(#n    = n,
    ccc  = 2 * sxy / (sx2 + sy2 + (mx - my)^2),
    mad  = mad(d),
    rmse = sqrt(mean(d^2)),
    l2n = sqrt(wt*median(d)^2 + (1-wt)*mad(d)^2),
    mae  = mean(abs(d)),
    mbe  = mean(d)
  )
}, by = .(stock, metric, om_name, mp_name)]

summ_metrics <- merge(summ_metrics, conv.dt)

#------------------------------------------------------------------
# question: most robust OM (decision) in face of misspecification (scenario)
#------------------------------------------------------------------

# minmax regret (kind of)
# regret is computed against the OM = MP situation
summ_metrics[, `:=`(regret = l2n - l2n[om_name == mp_name]), by = .(stock, metric, mp_name)]
summ_metrics <- summ_metrics[regret>0,]
summ_metrics_om <- summ_metrics

# decision with minmax regret
mmr <- summ_metrics[mp_name!=om_name,
  .(max_regret = max(regret)),
  by = .(stock, metric, om_name)][,
    .(decision = om_name[which.min(max_regret)], value = min(max_regret), stat = "l2n", rq = "OM", method="minmax", sensitivity="no"),
    by = .(stock, metric)]

regret.dt <- mmr

# decision with expected (weighted mean) regret
er <- summ_metrics[mp_name!=om_name,
  .(e_regret = sum(regret*wt_om)),
  by = .(stock, metric, om_name)][,
    .(decision = om_name[which.min(e_regret)], value = min(e_regret), stat = "l2n", rq = "OM", method="expected", sensitivity="no"),
    by = .(stock, metric)]

regret.dt <- rbind(regret.dt, er)

#------------------------------------------------------------------
# question: most robust MP (decision) to potential reality (scenario)
#------------------------------------------------------------------

# minmax regret (kind of)
# regret is computed against the OM = MP situation
summ_metrics[, `:=`(regret = l2n - l2n[om_name == mp_name]), by = .(stock, metric, om_name)]
summ_metrics <- summ_metrics[regret>0,]
summ_metrics_mp <- summ_metrics

# decision with min range regret
mmr <- summ_metrics[mp_name!=om_name,
  .(max_regret = max(regret)),
  by = .(stock, metric, mp_name)][,
    .(decision = mp_name[which.min(max_regret)], value = min(max_regret), stat = "l2n", rq = "MP", method="minmax", sensitivity="no"),
    by = .(stock, metric)]

regret.dt <- rbind(regret.dt, mmr)

# decision with expected (weighted mean) regret
er <- summ_metrics[mp_name!=om_name,
  .(e_regret = sum(regret*wt_mp)),
  by = .(stock, metric, om_name)][,
    .(decision = om_name[which.min(e_regret)], value = min(e_regret), stat = "l2n", rq = "MP", method="expected", sensitivity="no"),
    by = .(stock, metric)]

regret.dt <- rbind(regret.dt, er)

#------------------------------------------------------------------
# save
#------------------------------------------------------------------

summ_metrics_om[,rq:="OM"]
summ_metrics_mp[,rq:="MP"]
summ_metrics <- rbind(summ_metrics_mp, summ_metrics_om)
save(regret.dt, summ_metrics, file="regret_bymetric.RData")
write.csv(regret.dt, file="regret_bymetric.csv")
