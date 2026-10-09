#==================================================================
# script for M analysis
# EJ
# 20251027
#==================================================================

#------------------------------------------------------------------
# load & set up
#------------------------------------------------------------------

library(FLa4a)
library(ggplotFL)
library(reshape2)
library(knitr)
library(FLBRP)
# stock
load("pilStock.RData")
pil.stk <- pil.stock
ages  <- as.numeric(dimnames(m(pil.stk))$age)
years <- as.numeric(dimnames(m(pil.stk))$year)
rng <- range(pil.stk)
# weights at age
wa <- yearMeans(stock.wt(pil.stk))
# index
load("toFLRpilFLindices.RData")
pil.idx <- indices

# acoustic index in April, no recruits
range(pil.idx$AcousticNumberAtAge)[c("startf", "endf")] <- c(3/12, 4/12)
pil.idx$AcousticNumberAtAge <- trim(pil.idx$AcousticNumberAtAge, age=1:6)

# DEPM biomass index for SSB in january
nms <- dimnames(pil.idx$DEPM)
nms[[1]] <- "all"
names(nms)[1] <- "age"
nms -> dimnames(pil.idx$DEPM)
pil.idx$DEPM <- FLIndexBiomass(index=index(pil.idx$DEPM))
# age range the biomass refers to and time of survey
range(pil.idx$DEPM)[c("min", "max", "startf", "endf")] <- c(1, rng["max"], 0, 1/12)

# recruitment index in October
names(dimnames(pil.idx$recruits))[1] <- "age"
range(pil.idx$recruits)[c("startf", "endf")] <- c(9/12, 10/12)

# remove index 2, it creates a mess in the fit. All refits fail
pil.idx <- pil.idx[-2]

#------------------------------------------------------------------
# growth model and parameters
#------------------------------------------------------------------

#From ICES WKPELA 2017
k <- 0.44
linf <- 23.2
t0 <- 0

# from Taylor (1958)
maxage <- t0 + 3/k

#Derived from values in WKPELA 2017
l50 <- 15.32

a <- 0.00677
b <- 3.0325
lmin <- 5

#VBGF function Bertalanffy Growth Function (VBGF):
pil.vb <- a4aGr(
  grMod=~linf*(1-exp(-k*(t-t0))),
  grInvMod=~t0-1/k*log(1-len/linf),
  params=FLPar(linf=linf, k=k, t0=t0, units=c("cm","year-1","year"))
)

# ages
ages <- 0:6
# length at age
la <- c(11.4,15.3,18.1,19.9,21.1,21.8,22.8)
#la <- c(predict(pil.vb, t=ages+0.5))
# weight at age
wa <- a/1000*la^b

#------------------------------------------------------------------
# M models
#------------------------------------------------------------------
m.ch <- m.t <- m.l <- m.b <- m.c <- m(pil.stk)

# current model 0.7*Gislason (like)
m.c[] <- c(0.98,0.61, 0.47, 0.40, 0.36, 0.35, 0.32)

# constant M, Then, A. Y., Hoenig, J. M., Hall, N. G., & Hewitt, D. A. (2015)
m.t[] <- 4.899 * maxage^(-0.916)

# weight based Lorenz
m.l[] <- 3 * (wa*1000)^(-0.288)

# charnov
m.ch[] <- k * (linf / la)^1.5

# length based Brodziak Brodziak, J., Ianelli, J., Lorenzen, K., & Method, R. D. (2011)
m.b[] <- k * l50/la     # código EJ

# plot
df0 <- FLQuants(list(Brodziak=m.b, "Current"=m.c, Lorenz=m.l, Then=m.t, Charnov=m.ch))
df0 <- lapply(df0, "[", j=1)
df0 <- do.call(cbind, df0)
df0 <- melt(df0, c("age", "model"))
df0$age <- df0$age-0.5
png("mmodels.png", 800, 800)
xyplot(value ~ age, groups = model, data = df0, type = "b",
  ylab = "M", auto.key = list(space = "top", columns = 5),
  par.settings = list(
    superpose.symbol = list(pch = 19),
    superpose.line = list(lwd = 2)
  )
)
dev.off()

#------------------------------------------------------------------
# Set up a4a stock assessment model #------------------------------------------------------------------

# models
fmod <- ~s(age, k = 5) + s(year, k = 24) + ti(age, year, k = c(5, 24))
srmod <- ~factor(replace(year, year<1997, 1997))

# number of iters
it <- 250

# random seed
rs <- 001199

# number of cores
cores <- 5

#------------------------------------------------------------------
# OM = current
#------------------------------------------------------------------

## Operating model
om.c <- pil.stk
fit <- simulate(sca(om.c, pil.idx, fmodel=fmod, srmodel=srmod), it, seed=rs, obserror=TRUE)

# update stock
om.c <- om.c+fit

## OEM
stk.oem <- om.c
idx.oem <- pil.idx
for(i in 1:length(idx.oem)) index(idx.oem[[i]]) <- index(fit)[[i]]

## MP

### current
stk.c <- stk.oem
m(stk.c)[] <- m.c

### charnov
stk.ch <- stk.oem
m(stk.ch)[] <- m.ch

### Then
stk.t <- stk.oem
m(stk.t)[] <- m.t

### Lorenz
stk.l <- stk.oem
m(stk.l)[] <- m.l

### Brodziak
stk.b <- stk.oem
m(stk.b)[] <- m.b

### call sca in parallel
stks <- FLStocks(c=stk.c, ch=stk.ch, t=stk.t, l=stk.l, b=stk.b)
idxs <- list(c=idx.oem, ch=idx.oem, t=idx.oem, l=idx.oem, b=idx.oem)
fits <- scas(stks, idxs, fmodel=list(fmod), srmodel=list(srmod), workers=cores)

### output (add om)
output.c <- stks + fits
output.c <- FLStocks(c(output.c, FLStocks(om=om.c)))

#------------------------------------------------------------------
# OM = charnov
#------------------------------------------------------------------

## Operating model
om.ch <- pil.stk
m(om.ch)[] <- m.ch
fit <- simulate(sca(om.ch, pil.idx, fmodel=fmod, srmodel=srmod), it, seed=rs, obserror=TRUE)

# update stock
om.ch <- om.ch+fit

## OEM (no need to update idx.oem)
stk.oem <- om.ch

## MP

### current
stk.c <- stk.oem
m(stk.c)[] <- m.c

### charnov
stk.ch <- stk.oem
m(stk.ch)[] <- m.ch

### Then
stk.t <- stk.oem
m(stk.t)[] <- m.t

### Lorenz
stk.l <- stk.oem
m(stk.l)[] <- m.l

### Brodziak
stk.b <- stk.oem
m(stk.b)[] <- m.b

### call sca in parallel
stks <- FLStocks(c=stk.c, ch=stk.ch, t=stk.t, l=stk.l, b=stk.b)
idxs <- list(c=idx.oem, cw=idx.oem, t=idx.oem, l=idx.oem, b=idx.oem)
fits <- scas(stks, idxs, fmodel=list(fmod), srmodel=list(srmod), workers=cores)

### output
output.ch <- stks + fits
output.ch <- FLStocks(c(output.ch, FLStocks(om=om.ch)))

#------------------------------------------------------------------
# OM = then
#------------------------------------------------------------------

## Operating model
om.t <- pil.stk
m(om.t)[] <- m.t
fit <- simulate(sca(om.t, pil.idx, fmodel=fmod, srmodel=srmod), it, seed=rs, obserror=TRUE)

# update stock
om.t <- om.t+fit

## OEM (no need to update idx.oem)
stk.oem <- om.t
catch.n(stk.oem) <- replaceZeros(catch.n(stk.oem))

## MP

### current
stk.c <- stk.oem
m(stk.c)[] <- m.c

### charnov
stk.ch <- stk.oem
m(stk.ch)[] <- m.ch

### Then
stk.t <- stk.oem
m(stk.t)[] <- m.t

### Lorenz
stk.l <- stk.oem
m(stk.l)[] <- m.l

### Brodziak
stk.b <- stk.oem
m(stk.b)[] <- m.b

### call sca in parallel
stks <- FLStocks(c=stk.c, ch=stk.ch, t=stk.t, l=stk.l, b=stk.b)
idxs <- list(c=idx.oem, cw=idx.oem, t=idx.oem, l=idx.oem, b=idx.oem)
fits <- scas(stks, idxs, fmodel=list(fmod), srmodel=list(srmod), workers=cores)

### output
output.t <- stks + fits
output.t <- FLStocks(c(output.t, FLStocks(om=om.t)))

#------------------------------------------------------------------
# OM = lorenz
#------------------------------------------------------------------

## Operating model
om.l <- pil.stk
m(om.l)[] <- m.l
fit <- simulate(sca(om.l, pil.idx, fmodel=fmod, srmodel=srmod), it, seed=rs, obserror=TRUE)

# update stock
om.l <- om.l+fit

## OEM (no need to update idx.oem)
stk.oem <- om.l

## MP

### current
stk.c <- stk.oem
m(stk.c)[] <- m.c

### charnov
stk.ch <- stk.oem
m(stk.ch)[] <- m.ch

### Then
stk.t <- stk.oem
m(stk.t)[] <- m.t

### Lorenz
stk.l <- stk.oem
m(stk.l)[] <- m.l

### Brodziak
stk.b <- stk.oem
m(stk.b)[] <- m.b

### call sca in parallel
stks <- FLStocks(c=stk.c, ch=stk.ch, t=stk.t, l=stk.l, b=stk.b)
idxs <- list(c=idx.oem, cw=idx.oem, t=idx.oem, l=idx.oem, b=idx.oem)
fits <- scas(stks, idxs, fmodel=list(fmod), srmodel=list(srmod), workers=cores)

### output
output.l <- stks + fits
output.l <- FLStocks(c(output.l, FLStocks(om=om.l)))

#------------------------------------------------------------------
# OM = brodziak
#------------------------------------------------------------------

## Operating model
om.b <- pil.stk
m(om.b)[] <- m.b
fit <- simulate(sca(om.b, pil.idx, fmodel=fmod, srmodel=srmod), it, seed=rs, obserror=TRUE)

# update stock
om.b <- om.b+fit

## OEM (no need to update idx.oem)
stk.oem <- om.b

## MP

### current
stk.c <- stk.oem
m(stk.c)[] <- m.c

### charnov
stk.ch <- stk.oem
m(stk.ch)[] <- m.ch

### Then
stk.t <- stk.oem
m(stk.t)[] <- m.t

### Lorenz
stk.l <- stk.oem
m(stk.l)[] <- m.l

### Brodziak
stk.b <- stk.oem
m(stk.b)[] <- m.b

### call sca in parallel
stks <- FLStocks(c=stk.c, ch=stk.ch, t=stk.t, l=stk.l, b=stk.b)
idxs <- list(c=idx.oem, cw=idx.oem, t=idx.oem, l=idx.oem, b=idx.oem)
fits <- scas(stks, idxs, fmodel=list(fmod), srmodel=list(srmod), workers=cores)

### output
output.b <- stks + fits
output.b <- FLStocks(c(output.b, FLStocks(om=om.b)))

#------------------------------------------------------------------
# Plots of scenarios
#------------------------------------------------------------------

panel_labels <- c("c" = "Current", "ch" = "Charnov", "t" = "Then", "l" = "Lorenz", "b" = "Brodziak", "om" = "Operating Model", "Rec" = "Recruitment", "SB" = "Spawning Stock Biomass", "C" = "Catches", "F" = "Fishing mortality")

plot(output.c, probs=c(0.9,0.9,0.5,0.1,0.1)) +
  facet_grid(qname ~ stock, scales = "free_y", labeller = as_labeller(panel_labels)) +
  theme(legend.position = "none")

plot(output.ch, probs=c(0.9,0.9,0.5,0.1,0.1)) +
  facet_grid(qname ~ stock, scales = "free_y", labeller = as_labeller(panel_labels)) +
  theme(legend.position = "none")

plot(output.t, probs=c(0.9,0.9,0.5,0.1,0.1)) +
  facet_grid(qname ~ stock, scales = "free_y", labeller = as_labeller(panel_labels)) +
  theme(legend.position = "none")

plot(output.l, probs=c(0.9,0.9,0.5,0.1,0.1)) +
  facet_grid(qname ~ stock, scales = "free_y", labeller = as_labeller(panel_labels)) +
  theme(legend.position = "none")

plot(output.b, probs=c(0.9,0.9,0.5,0.1,0.1)) +
  facet_grid(qname ~ stock, scales = "free_y", labeller = as_labeller(panel_labels)) +
  theme(legend.position = "none")

#------------------------------------------------------------------
# Check
#------------------------------------------------------------------

flqs0 <- lapply(output.c, "fbar")
df0 <- data.frame(om="c", metric="f", data=c(flqs0$c/flqs0$om))
flqs0 <- lapply(output.c, "ssb")
df0 <- rbind(df0, data.frame(om="c", metric="ssb", data=c(flqs0$c/flqs0$om)))
flqs0 <- lapply(output.c, "rec")
df0 <- rbind(df0, data.frame(om="c", metric="rec", data=c(flqs0$c/flqs0$om)))

flqs0 <- lapply(output.ch, "fbar")
df0 <- rbind(df0, data.frame(om="cw", metric="f", data=c(flqs0$ch/flqs0$om)))
flqs0 <- lapply(output.ch, "ssb")
df0 <- rbind(df0, data.frame(om="cw", metric="ssb", data=c(flqs0$ch/flqs0$om)))
flqs0 <- lapply(output.ch, "rec")
df0 <- rbind(df0, data.frame(om="cw", metric="rec", data=c(flqs0$ch/flqs0$om)))

flqs0 <- lapply(output.t, "fbar")
df0 <- rbind(df0, data.frame(om="t", metric="f", data=c(flqs0$t/flqs0$om)))
flqs0 <- lapply(output.t, "ssb")
df0 <- rbind(df0, data.frame(om="t", metric="ssb", data=c(flqs0$t/flqs0$om)))
flqs0 <- lapply(output.t, "rec")
df0 <- rbind(df0, data.frame(om="t", metric="rec", data=c(flqs0$t/flqs0$om)))

flqs0 <- lapply(output.b, "fbar")
df0 <- rbind(df0, data.frame(om="b", metric="f", data=c(flqs0$b/flqs0$om)))
flqs0 <- lapply(output.b, "ssb")
df0 <- rbind(df0, data.frame(om="b", metric="ssb", data=c(flqs0$b/flqs0$om)))
flqs0 <- lapply(output.b, "rec")
df0 <- rbind(df0, data.frame(om="b", metric="rec", data=c(flqs0$b/flqs0$om)))

flqs0 <- lapply(output.l, "fbar")
df0 <- rbind(df0, data.frame(om="l", metric="f", data=c(flqs0$l/flqs0$om)))
flqs0 <- lapply(output.l, "ssb")
df0 <- rbind(df0, data.frame(om="l", metric="ssb", data=c(flqs0$l/flqs0$om)))
flqs0 <- lapply(output.l, "rec")
df0 <- rbind(df0, data.frame(om="l", metric="rec", data=c(flqs0$l/flqs0$om)))

histogram(~data|om*metric, data=df0)

checkbar.df <- with(df0, tapply(data, list(om,metric), mean))
checksd.df <- with(df0, tapply(data, list(om,metric), sd))
checkmax.df <- with(df0, tapply(data, list(om,metric), max))
checkmin.df <- with(df0, tapply(data, list(om,metric), min))


kable(checkbar.df, digit=3, caption="Mean ratio between EM of OM and OM metrics")

kable(checksd.df, digit=3, caption="Standard deviation of ratio between EM of OM and OM metrics")

kable(checkmax.df, digit=3, caption="Max ratio between EM of OM and OM metrics")

kable(checkmin.df, digit=3, caption="Min ratio between EM of OM and OM metrics")

#------------------------------------------------------------------
# Results and tables
#------------------------------------------------------------------

output.all <- list("Current" = output.c[c("ch", "t","l","b","om")],
  "Charnov" = output.ch[c("c", "t","l","b","om")],
  "Then"= output.t[c("c", "ch","l","b","om")],
  "Lorenz"= output.l[c("c", "ch", "t","b","om")],
  "Brodziak"= output.b[c("c", "ch", "t","l","om")]
  )

lst0 <- lapply(output.all, function(x){
  # This is not the most efficient code ...
  obj0 <- lapply(x, rec)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  #rec=with(subset(obj0, obj0$qname!="om"), sqrt(mean(data-om)^2))
  # try relative error?
  rec <- with(subset(obj0, obj0$qname!="om"), mad(data-om))

  obj0 <- lapply(x, ssb)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  #ssb=with(subset(obj0, obj0$qname!="om"), sqrt(mean((data-om)^2))
  ssb <- with(subset(obj0, obj0$qname!="om"), mad(data-om))

  obj0 <- lapply(x, fbar)
  obj0 <- as.data.frame(obj0)
  obj0$om <- obj0[obj0$qname=="om","data"]
  #fbar=with(subset(obj0, obj0$qname!="om"), sqrt(mean((data-om)^2))
  fbar <- with(subset(obj0, obj0$qname!="om"), mad(data-om))

  c(rec, ssb, fbar)
})

results_om <- do.call(cbind, lst0)
results_om[] <- t(apply(results_om, 1, function(x) x/max(x)))
rownames(results_om) <- c("recruitment", "ssb", "f")

kable(results_om, digit=2, caption="MAD for each OM divided my the maximum to
simplify the comparison")


#------------------------------------------------------------------
# Results and tables (alternative)
#------------------------------------------------------------------

output.all <- list("Current" = output.c, "Charnov" = output.ch, "Then"=
output.t, "Lorenz"= output.l, "Brodziak"= output.b)

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

  obj0 <- lapply(x, function(y){
		fbar(y)[,"2023"]/refpts(brp(FLBRP(y)))["spr.30","harvest"]
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
levels(results.df$mp_name) <- c("Current","Charnov", "Then", "Lorenz", "Brodziak", "OM")
results.df$mp_name <- as.character(results.df$mp_name)

pab <- function(x, y, ...) {
    # Plot the points
    panel.xyplot(x, y, ...)

    # Add the abline (a=0, b=1)
    panel.abline(a = 0, b = 1, lty = 2, lwd=2, col = "red")
}

png("recruiment.png", 1000, 1000)
xyplot(mp~om|mp_name*om_name, data=subset(results.df, metric=="R"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Recruitment")
dev.off()

png("ssb.png", 1000, 1000)
xyplot(mp~om|mp_name*om_name, data=subset(results.df, metric=="B"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="SSB")
dev.off()

png("fishingmortality.png", 1000, 1000)
xyplot(mp~om|mp_name*om_name, data=subset(results.df, metric=="F"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Fishing mortality")
dev.off()

png("exploitation.png", 1000, 1000)
xyplot(mp~om|mp_name*om_name, data=subset(results.df, metric=="S"), panel=pab, scales="free", pch=19, cex=0.3, col="gray50", as.table=TRUE, main="Exploitation levels (F/Fmsy)")
dev.off()

# kobe quadrant?

save(checkbar.df,checkmax.df,checkmin.df,checksd.df,m.b,m.c,m.l,m.t,output.all, file="results.RData")
