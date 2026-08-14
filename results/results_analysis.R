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
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
dev.off()

png("hke1567_ssb.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
dev.off()

png("hke1567_fishingmortality.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
dev.off()

png("hke1567_exploitation.png", 1000, 1000)
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
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
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
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
xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
dev.off()

#------------------------------------------------------------------
# corcondance correlation coeficient
#------------------------------------------------------------------

summ_metrics <- results[om_name != mp_name, {
  d <- mp - om
  vx <- var(mp); vy <- var(om)
  cxy <- cov(mp, om)
  ccc_val <- 2 * cxy / (vx + vy + (mean(mp) - mean(om))^2)
  .(ccc = round(ccc_val, 2),
    ccc_thr = ccc_val >= 0.99,
    mad = mad(d),
    rmse = sqrt(mean(d^2)),
    mae = mean(abs(d)))}, by = .(stock, metric, om_name, mp_name)]

xyplot(ccc~mp_name|stock*om_name, groups="metric", data=summ_metrics)

dcast(summ_metrics, om_name + stock + metric ~mp_name, value.var = "mae")
dcast(summ_metrics, stock + metric + om_name ~mp_name, value.var = "ccc")


minmax_dt <-

summ_metrics[, .SD[which.max(ccc)], by = .(stock, metric)]
summ_metrics[stock=="hke1567" & metric=="B", .SD[which.min(ccc)], by = .(om_name)]


minmax_dt[, .SD[which.min(rmse)], by = .(stock, metric)]
