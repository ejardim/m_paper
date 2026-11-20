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

conv.dt <- results[metric=="F",.(conv = mean(is.na(mp))), by = .(om_name, mp_name, stock)]

# remove non-convergence
to_remove <- results[metric == "F" & is.na(mp), .(iter, mp_name, om_name, stock)]
results <- results[!to_remove, on = .(iter, mp_name, om_name, stock)]

#------------------------------------------------------------------
# corcondance correlation coeficient & Co.
#------------------------------------------------------------------

results[, `:=`(lmp = log(mp), lom = log(om))]

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
    rmse = sqrt(mean(d^2))#,
#    mae  = mean(abs(d)),
#    mbe  = mean(d)
  )
}, by = .(stock, metric, om_name, mp_name)]

#------------------------------------------------------------------
# question: most robust OM (decision) in face of misspecification (scenario)
#------------------------------------------------------------------

# minmax regret (kind of)
# regret is computed against the OM = MP situation
summ_metrics[, `:=`(ccc_regret = ccc[om_name == mp_name] - ccc, rmse_regret = rmse - rmse[om_name == mp_name], mad_regret = mad - mad[om_name == mp_name]), by = .(stock, metric, mp_name)]

# decision with min range regret
ccc_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(ccc_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "ccc", rq = "OM", method="minmax", sensitivity="no"),
    by = .(stock)]

rmse_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(rmse_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "rmse", rq = "OM", method="minmax", sensitivity="no"),
    by = .(stock)]

mad_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(mad_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "mad", rq = "OM", method="minmax", sensitivity="no"),
    by = .(stock)]

regret.dt <- rbind(ccc_mmr, rmse_mmr, mad_mmr)

#------------------------------------------------------------------
# question: most robust MP (decision) to potential reality (scenario)
#------------------------------------------------------------------

# minmax regret (kind of)
# regret is computed against the OM = MP situation
summ_metrics[, `:=`(ccc_regret = ccc[om_name == mp_name] - ccc, rmse_regret = rmse - rmse[om_name == mp_name], mad_regret = mad - mad[om_name == mp_name]), by = .(stock, metric, om_name)]

# decision with min range regret
ccc_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(ccc_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "ccc", rq = "MP", method="minmax", sensitivity="no"),
    by = .(stock)]

rmse_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(rmse_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "rmse", rq = "MP", method="minmax", sensitivity="no"),
    by = .(stock)]

mad_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(mad_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "mad", rq = "MP", method="minmax", sensitivity="no"),
    by = .(stock)]

regret.dt <- rbind(regret.dt, ccc_mmr, rmse_mmr, mad_mmr)

#------------------------------------------------------------------
# save
#------------------------------------------------------------------

regret_minmax.dt <- regret.dt
save(regret_minmax.dt, file="regret_minmax.RData")
