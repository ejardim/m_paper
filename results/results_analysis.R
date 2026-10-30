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
summ_metrics_om <- summ_metrics

# decision with min range regret
ccc_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(ccc_regret) - min(ccc_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "ccc", rq = "OM", method="minrng", sensitivity="no"),
    by = .(stock)]

rmse_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(rmse_regret) - min(rmse_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "rmse", rq = "OM", method="minrng", sensitivity="no"),
    by = .(stock)]

mad_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(mad_regret) - min(mad_regret)),
  by = .(stock, om_name)][,
    .(decision = om_name[which.min(rng_regret)], value = min(rng_regret), stat = "mad", rq = "OM", method="minrng", sensitivity="no"),
    by = .(stock)]

regret.dt <- rbind(ccc_mmr, rmse_mmr, mad_mmr)

#------------------------------------------------------------------
# question: most robust MP (decision) to potential reality (scenario)
#------------------------------------------------------------------

# minmax regret (kind of)
# regret is computed against the OM = MP situation
summ_metrics[, `:=`(ccc_regret = ccc[om_name == mp_name] - ccc, rmse_regret = rmse - rmse[om_name == mp_name], mad_regret = mad - mad[om_name == mp_name]), by = .(stock, metric, om_name)]
summ_metrics_mp <- summ_metrics

# decision with min range regret
ccc_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(ccc_regret) - min(ccc_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "ccc", rq = "MP", method="minrng", sensitivity="no"),
    by = .(stock)]

rmse_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(rmse_regret) - min(rmse_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "rmse", rq = "MP", method="minrng", sensitivity="no"),
    by = .(stock)]

mad_mmr <- summ_metrics[mp_name!=om_name & metric!="S",
  .(rng_regret = max(mad_regret)-min(mad_regret)),
  by = .(stock, mp_name)][,
    .(decision = mp_name[which.min(rng_regret)], value = min(rng_regret), stat = "mad", rq = "MP", method="minrng", sensitivity="no"),
    by = .(stock)]

regret.dt <- rbind(regret.dt, ccc_mmr, rmse_mmr, mad_mmr)

#------------------------------------------------------------------
# plots
#------------------------------------------------------------------

pab <- function(x, y, ...) {
    # Plot the points
    panel.xyplot(x, y, ...)

    # Add the abline (a=0, b=1)
    panel.abline(a = 0, b = 1, lty = 2, lwd=2, col = "red")
}

# hke1567
png("hke1567_recruiment.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="hke1567" & iter!=114), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
dev.off()

png("hke1567_ssb.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
dev.off()

png("hke1567_fishingmortality.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
dev.off()

png("hke1567_exploitation.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploited level (B/Bspr.20)")
dev.off()

# nep06
png("nep06_recruiment.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
dev.off()

png("nep06_ssb.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
dev.off()

png("nep06_fishingmortality.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
dev.off()

png("nep06_exploitation.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploited level (B/Bspr.20)")
dev.off()

# pil8c9a
png("pil8c9a_recruiment.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
dev.off()

png("pil8c9a_ssb.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
dev.off()

png("pil8c9a_fishingmortality.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
dev.off()

png("pil8c9a_exploitation.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploited level (B/Bspr.20)")
dev.off()

#------------------------------------------------------------------
# save
#------------------------------------------------------------------

summ_metrics_om[,rq:="OM"]
summ_metrics_mp[,rq:="MP"]
summ_metrics <- rbind(summ_metrics_mp, summ_metrics_om)
save(regret.dt, summ_metrics, file="regret.RData")


