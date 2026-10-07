#--------------------------------------------------------------------
#--------------------------------------------------------------------
library(FLa4a)
load("input_data.RData")
pil.idx <- pil.idx[-2]
it <- 10
rs <- 001199

#--------------------------------------------------------------------
#--------------------------------------------------------------------

fmod <- ~s(age, k = 5) + s(year, k = 24) + ti(age, year, k = c(5, 24))
srmod <- ~factor(replace(year, year<1997, 1997))

## fit
fit0 <- sca(pil.stk, pil.idx, fmodel=fmod, srmodel=srmod)
fits <- simulate(fit0, it, seed=rs, obserror=TRUE)

## OEM
stk.oem <- pil.stk + fits
idx.oem <- pil.idx
for(i in 1:length(idx.oem)) index(idx.oem[[i]]) <- index(fits)[[i]]

## refit
refit <- sca(stk.oem, idx.oem, fmodel=fmod, srmodel=srmod)
mean(fitSumm(refit)["convergence",]==1)

refit <- sca(stk.oem, idx.oem, fmodel=fmod, srmodel=srmod, fit="MP")
plot(FLStocks(orig=pil.stk + fit0, refit=pil.stk+refit))


