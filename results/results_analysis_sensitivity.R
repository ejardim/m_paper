#==================================================================
# script to compile results for the M paper
# sensitivity to outliers
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
# outliers (iters outside 3 IQR stock status for om==mp)
#------------------------------------------------------------------

same <- results[metric=="F", lmp:=log(mp)]
by_cols <- c("om_name", "mp_name", "stock")

lims <- same[, {
  q <- quantile(lmp, c(0.25, 0.75), na.rm = TRUE)
  .(lo = q[1] - 3 * diff(q), hi = q[2] + 3 * diff(q))
}, by = by_cols]

out.dt <- same[lims, on = by_cols][lmp < lo | lmp > hi]

# how many by OM (only hake ...)
out.dt[, .(n_iters = uniqueN(iter)), by = by_cols]

# final dt
results <- results[!out.dt[, .(stock, om_name, iter)], on = .(stock, om_name, iter)]

#------------------------------------------------------------------
# concordance correlation coeficient & Co.
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
  .(rng_regret = max(ccc_regret) - min(ccc_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "ccc", rq = "OM", method="minrng", sensitivity="yes"),
    by = .(stock)]

rmse_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(rmse_regret) - min(rmse_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "rmse", rq = "OM", method="minrng", sensitivity="yes"),
    by = .(stock)]

mad_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(mad_regret) - min(mad_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "mad", rq = "OM", method="minrng", sensitivity="yes"),
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
  .(rng_regret = max(ccc_regret) - min(ccc_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "ccc", rq = "MP", method="minrng", sensitivity="yes"),
    by = .(stock)]

rmse_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(rmse_regret) - min(rmse_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "rmse", rq = "MP", method="minrng", sensitivity="yes"),
    by = .(stock)]

mad_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(mad_regret)-min(mad_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "mad", rq = "MP", method="minrng", sensitivity="yes"),
    by = .(stock)]

regret.dt <- rbind(regret.dt, ccc_mmr, rmse_mmr, mad_mmr)

#------------------------------------------------------------------
# save
#------------------------------------------------------------------

regret_sensitivity.dt <- regret.dt
save(regret_sensitivity.dt, file="regret_sensitivity.RData")

