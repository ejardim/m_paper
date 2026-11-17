rm(list = ls())
library(ggplot2);theme_set(theme_bw())
library(data.table)
load("results_dt.rda")

# Remove non-converged
run_id <- paste(results$stock, results$om_name, results$mp_name, results$iter)
failed <- run_id[results$metric == "F" & is.na(results$mp)]
results <- results[!(run_id %in% failed), ]
length(unique(failed))

summary(results)

d0 <- results[results$metric != "S", ]
d0$d <- log(d0$mp / d0$om)

# bias and spread metric
rr <- aggregate(d ~ stock + metric + om_name + mp_name, data = d0,
                FUN = function(d) sqrt(median(d)^2 + mad(d)^2))
names(rr)[names(rr) == "d"] <- "l2norm"

ggplot(rr, aes(x = mp_name, y = om_name, fill = l2norm)) +
  geom_tile(color = "white") +
  geom_tile(data = rr[rr$om_name == rr$mp_name, ], fill = NA, color = "black", linewidth = 0.8) +
  geom_text(aes(label = round(l2norm, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "firebrick",
                      limits = c(0, log(3)), oob = scales::squish, name = "l2norm") +
  facet_wrap(~ stock + metric, scales = "free", ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# regret as the min by om or mp
# min not always occure when mp = om
# regret must be non negative
# om=mp isnt't used for regret analysis)
rr <- rr[rr$om_name!=rr$mp_name,]

# mp as decision, om as scenario
rr$regret_om <- rr$l2norm - ave(rr$l2norm, rr$stock, rr$om_name, FUN = min)
wr <- aggregate(regret_om ~ stock + mp_name, data = rr, FUN = max)
mmr_mp <- do.call(rbind,
  lapply(split(wr, list(wr$stock)),
    function(x) x[which.min(x$regret_om),]))
mmr_mp

# om as decision, mp as scenario
rr$regret_mp <- rr$l2norm - ave(rr$l2norm, rr$stock, rr$mp_name, FUN = min)
wr <- aggregate(regret_mp ~ stock + om_name, data = rr, FUN = max)
mmr_om <- do.call(rbind,
  lapply(split(wr, list(wr$stock), drop = TRUE),
    function(x) x[which.min(x$regret_mp), ]))
mmr_om

# # bias only and comparison
# bias <- aggregate(d ~ stock + metric + om_name + mp_name, data = d0, FUN = median)
# names(bias)[names(bias) == "d"] <- "bias"
# bias$bias_pct <- round(100 * (exp(bias$bias) - 1), 1)
#
# ggplot(bias, aes(x = mp_name, y = om_name, fill = bias)) +
#   geom_tile(color = "white") +
#   geom_tile(data = bias[bias$om_name == bias$mp_name, ], fill = NA, color = "black", linewidth = 0.8) +
#   geom_text(aes(label = bias_pct), size = 3) +
#   scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", midpoint = 0,
#                        limits = c(-log(3), log(3)), oob = scales::squish, name = "log bias") +
#   facet_wrap(~ stock + metric, scales = "free", ncol = 3) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
# bias$abias <- abs(bias$bias)
# bias$regret <- bias$abias - ave(bias$abias, bias$stock, bias$metric, bias$om_name, FUN = min)
#
# wr <- aggregate(regret ~ stock + metric + mp_name, data = bias, FUN = max)
# mmr_bias <- do.call(rbind, lapply(split(wr, list(wr$stock, wr$metric), drop = TRUE),
#                                   function(x) x[which.min(x$regret), ]))
# rownames(mmr_bias) <- NULL
# mmr_bias
# rownames(mmr) <- NULL
# mmr
# # only hake in B is different


# ALT: bias and spread metric
rr <- aggregate(d ~ stock + metric + om_name + mp_name, data = d0,
                FUN = function(d) sqrt(median(d)^2 + mad(d)^2))
names(rr)[names(rr) == "d"] <- "l2norm"

# regret as ratio to om=mp
ref <- rr[rr$om_name == rr$mp_name,]
names(ref)[names(ref) == "l2norm"] <- "ref_l2norm"

# mp as decision om as scenarios
rr_mp <- merge(rr, ref, by = c("stock", "metric", "om_name"))
rr_mp$regret <- rr_mp$l2norm / rr_mp$ref_l2norm
rr_mp <- rr_mp[rr_mp$om_name != rr_mp$mp_name.x,]

wr <- aggregate(regret ~ stock + mp_name.x, data = rr_mp, FUN = function(x) max(x) - min(x))
mmrs_mp <- do.call(rbind,
  lapply(split(wr, list(wr$stock)),
    function(x) x[which.min(x$regret), ]))
mmrs_mp

# om as decision mp as scenarios
rr_om <- merge(rr, ref, by = c("stock", "metric", "mp_name"))
rr_om$regret <- rr_om$l2norm / rr_om$ref_l2norm
rr_om <- rr_mp[rr_om$om_name.x != rr_om$mp_name,]

wr <- aggregate(regret ~ stock + om_name, data = rr_om, FUN = function(x) max(x) - min(x))
mmrs_om <- do.call(rbind,
  lapply(split(wr, list(wr$stock)),
    function(x) x[which.min(x$regret), ]))
mmrs_om
