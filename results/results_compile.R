#==================================================================
# script to compile results for the M paper
# EJ
# 20260709
#==================================================================

#------------------------------------------------------------------
# load
#------------------------------------------------------------------

library(FLa4a)
library(data.table)
library(FLBRP)

#------------------------------------------------------------------
# hke17
#------------------------------------------------------------------

load("../hke1567/results.RData")

lst0 <- lapply(output.all, function(x){

  obj0 <- lapply(x, rec)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  rec <- subset(obj0, obj0$qname!="om")
  rec$metric <- "R"

  obj0 <- lapply(x, ssb)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  b <- subset(obj0, obj0$qname!="om")
  b$metric <- "B"

  obj0 <- lapply(x, fbar)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  f <- subset(obj0, obj0$qname!="om")
  f$metric <- "F"

  rp <- refpts(FLBRP())[1]
  dimnames(rp)$refpt <- "spr.20"

  obj0 <- lapply(x, function(y){
    flrp <- FLBRP(y)
    refpts(flrp) <- rp
    rfp <- refpts(brp(flrp))[,"ssb"]
 	yearMeans(ssb(y)[,ac(2021:2023)])/rfp
  })
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  s <- subset(obj0, obj0$qname!="om")
  s$metric <- "S"

  rbind(rec,b,f,s)
})

# build dataframe with results, needs check if code above changes
v0 <- rep(names(lst0), c(lapply(lst0, nrow)))
results.df <- do.call(rbind, lst0)
results.df$om_name <- v0
results.df <- results.df[,-c(3,4,5)]
names(results.df)[c(4,5)] <- c("mp","mp_name")
levels(results.df$mp_name) <- c("Current","ChenWatanabe", "Then", "Lorenz", "Brodziak", "OM")
results.df$mp_name <- as.character(results.df$mp_name)
results.df$stock <- "hke1567"
results <- data.table(results.df)
dt0 <- results[year %between% c(2021, 2023),
               .(om = median(om, na.rm = TRUE), mp = median(mp, na.rm = TRUE)),
               by = .(stock, metric, mp_name, om_name, iter)]
results <- rbind(dt0, results[metric=="S", c("stock", "metric", "mp_name", "om_name", "om", "mp", "iter")])

#------------------------------------------------------------------
# nep06
#------------------------------------------------------------------

load("../nep06/results.RData")

lst0 <- lapply(output.all, function(x){

  obj0 <- lapply(x, rec)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  rec <- subset(obj0, obj0$qname!="om")
  rec$metric <- "R"

  obj0 <- lapply(x, ssb)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  b <- subset(obj0, obj0$qname!="om")
  b$metric <- "B"

  obj0 <- lapply(x, fbar)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  f <- subset(obj0, obj0$qname!="om")
  f$metric <- "F"

  rp <- refpts(FLBRP())[1]
  dimnames(rp)$refpt <- "spr.20"

  obj0 <- lapply(x, function(y){
    flrp <- FLBRP(y)
    refpts(flrp) <- rp
    rfp <- refpts(brp(flrp))[,"ssb"]
 	yearMeans(ssb(y)[,ac(2022:2024)])/rfp
  })
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  s <- subset(obj0, obj0$qname!="om")
  s$metric <- "S"

  rbind(rec,b,f,s)
})

# build dataframe with results, needs check if code above changes
v0 <- rep(names(lst0), c(lapply(lst0, nrow)))
results.df <- do.call(rbind, lst0)
results.df$om_name <- v0
results.df <- results.df[,-c(3,4,5)]
names(results.df)[c(4,5)] <- c("mp","mp_name")
levels(results.df$mp_name) <- c("Current", "Then", "Lorenz", "Brodziak", "OM")
results.df$mp_name <- as.character(results.df$mp_name)
results.df$stock <- "nep06"
dt0 <- data.table(results.df)
dt0 <- dt0[year %between% c(2022, 2024),
               .(om = median(om, na.rm = TRUE), mp = median(mp, na.rm = TRUE)),
               by = .(stock, metric, mp_name, om_name, iter)]
results <- rbind(dt0, results)
results <- rbind(results, subset(results.df, metric=="S")[, c("stock", "metric", "mp_name", "om_name", "om", "mp", "iter")])

#------------------------------------------------------------------
# pil8c9a
#------------------------------------------------------------------

load("../pil8c9a/results.RData")

lst0 <- lapply(output.all, function(x){

  obj0 <- lapply(x, rec)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  rec <- subset(obj0, obj0$qname!="om")
  rec$metric <- "R"

  obj0 <- lapply(x, ssb)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  b <- subset(obj0, obj0$qname!="om")
  b$metric <- "B"

  obj0 <- lapply(x, fbar)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  f <- subset(obj0, obj0$qname!="om")
  f$metric <- "F"

  rp <- refpts(FLBRP())[1]
  dimnames(rp)$refpt <- "spr.20"

  obj0 <- lapply(x, function(y){
    flrp <- FLBRP(y)
    refpts(flrp) <- rp
    rfp <- refpts(brp(flrp))[,"ssb"]
 	yearMeans(ssb(y)[,ac(2023:2025)])/rfp
  })
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  s <- subset(obj0, obj0$qname!="om")
  s$metric <- "S"

  rbind(rec,b,f,s)
})

# build dataframe with results, needs check if code asbove changes
v0 <- rep(names(lst0), c(lapply(lst0, nrow)))
results.df <- do.call(rbind, lst0)
results.df$om_name <- v0
results.df <- results.df[,-c(3,4,5)]
names(results.df)[c(4,5)] <- c("mp","mp_name")
levels(results.df$mp_name) <- c("Current", "Charnov", "Then", "Lorenz", "Brodziak", "OM")
results.df$mp_name <- as.character(results.df$mp_name)
results.df$stock <- "pil8c9a"
dt0 <- data.table(results.df)
dt0 <- dt0[year %between% c(2023, 2025),
               .(om = median(om, na.rm = TRUE), mp = median(mp, na.rm = TRUE)),
               by = .(stock, metric, mp_name, om_name, iter)]
results <- rbind(dt0, results, subset(results.df, metric=="S")[, c("stock", "metric", "mp_name", "om_name", "om", "mp", "iter")])

save(results, file="results_dt.rda")
