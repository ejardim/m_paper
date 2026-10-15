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
results.orig <- results

#------------------------------------------------------------------
# plots (before cleaning)
#------------------------------------------------------------------
#
# pab <- function(x, y, ...) {
#     # Plot the points
#     panel.xyplot(x, y, ...)
#
#     # Add the abline (a=0, b=1)
#     panel.abline(a = 0, b = 1, lty = 2, lwd=2, col = "red")
# }
#
# # hke1567
# png("hke1567_recruiment.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="hke1567" & iter!=114), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
# dev.off()
#
# png("hke1567_ssb.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
# dev.off()
#
# png("hke1567_fishingmortality.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
# dev.off()
#
# png("hke1567_exploitation.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="hke1567"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
# dev.off()
#
# # nep06
# png("nep06_recruiment.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
# dev.off()
#
# png("nep06_ssb.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
# dev.off()
#
# png("nep06_fishingmortality.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
# dev.off()
#
# png("nep06_exploitation.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="nep06"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
# dev.off()
#
# # pil8c9a
# png("pil8c9a_recruiment.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="R" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
# dev.off()
#
# png("pil8c9a_ssb.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="B" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
# dev.off()
#
# png("pil8c9a_fishingmortality.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="F" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
# dev.off()
#
# png("pil8c9a_exploitation.png", 1000, 1000)
# xyplot(log(mp)~log(om)|mp_name*om_name, data=subset(results, metric=="S" & stock=="pil8c9a"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
# dev.off()

#------------------------------------------------------------------
# convergence (filter)
#------------------------------------------------------------------

conv.dt <- results[metric=="S",.(conv = mean(is.na(mp))), by = .(om_name, mp_name, stock)]

# remove non-convergence
to_remove <- results[metric == "S" & is.na(mp), .(iter, mp_name, om_name, stock)]
results <- results[!to_remove, on = .(iter, mp_name, om_name, stock)]

#------------------------------------------------------------------
# outliers (iters outside 7.5 IQR stock status for om==mp)
#------------------------------------------------------------------

same <- results[om_name == mp_name & metric=="S", lmp:=log(mp)]
by_cols <- c("om_name", "stock")

lims <- same[, {
  q <- quantile(lmp, c(0.25, 0.75), na.rm = TRUE)
  .(lo = q[1] - 5 * diff(q), hi = q[2] + 5 * diff(q))
}, by = by_cols]

out.dt <- same[lims, on = by_cols][lmp < lo | lmp > hi]

# how many by OM (only hake ...)
out.dt[, .(n_iters = uniqueN(iter)), by = by_cols]

# final dt
results <- results[!out.dt[, .(stock, om_name, iter)], on = .(stock, om_name, iter)]

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

results[, `:=`(lmp = log(mp), lom = log(om))]

summ_metrics <- results[complete.cases(mp, om), {
  n   <- .N
  d   <- lmp - lom
  mx  <- mean(lmp)
  my  <- mean(lom)
  sxy <- mean((lmp - mx) * (lom - my))
  sx2 <- mean((lmp - mx)^2)
  sy2 <- mean((lom - my)^2)
  .(n    = n,
    ccc  = round(2 * sxy / (sx2 + sy2 + (mx - my)^2), 2),
    mad  = round(mad(d), 2),
    rmse = round(sqrt(mean(d^2)), 2),
    mae  = round(mean(abs(d)), 2))
}, by = .(stock, metric, om_name, mp_name)]

dcast(summ_metrics, stock + metric + om_name ~mp_name, value.var = "mad")

dcast(summ_metrics, stock + metric + om_name ~mp_name, value.var = "ccc")

dcast(summ_metrics, stock + metric + om_name ~mp_name, value.var = "rmse")

dcast(summ_metrics, stock + metric + om_name ~mp_name, value.var = "mae")





