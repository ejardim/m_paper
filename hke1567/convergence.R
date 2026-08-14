#------------------------------------------------------------------------------
# official
#------------------------------------------------------------------------------

fmod <- ~s(age, k = 4) + s(year, k = 8) + te(age, year, k = c(3, 10))
qmod <- list(~I(1/(1 + exp(-age))))
## Operating model
om.c <- hke.stk
fit_of0 <- sca(om.c, hke.idx, fmodel=fmod, qmodel=qmod)
fit <- simulate(fit_of0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- hke.idx
index(idx.oem) <- index(fit)[[1]]
idx.oem <- FLIndices(idx=idx.oem)

fit_of <- sca(stk.oem, idx.oem, fmodel=fmod, qmodel=qmod)
mean(fitSumm(fit_of)["convergence",]==1)
# [1] 0.978

#------------------------------------------------------------------------------
# ti
#------------------------------------------------------------------------------

fmod <- ~  s(age, k = 5)  + s(year, k = 8) + ti(age, year, k = c(5, 8))
## Operating model
om.c <- hke.stk
fit_ti0 <- sca(om.c, hke.idx, fmodel=fmod)
fit <- simulate(fit_ti0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- hke.idx
index(idx.oem) <- index(fit)[[1]]
idx.oem <- FLIndices(idx=idx.oem)

fit_ti <- sca(stk.oem, idx.oem, fmodel=fmod)
mean(fitSumm(fit_ti)["convergence",]==1)
# [1] 0.058

#------------------------------------------------------------------------------
# default
#------------------------------------------------------------------------------

## Operating model
om.c <- hke.stk
fit_de0 <- sca(om.c, hke.idx)
fit <- simulate(fit_de0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- hke.idx
index(idx.oem) <- index(fit)[[1]]
idx.oem <- FLIndices(idx=idx.oem)

fit_de <- sca(stk.oem, idx.oem)
mean(fitSumm(fit_de)["convergence",]==1)
# [1] 0.278

#------------------------------------------------------------------------------
# output
#------------------------------------------------------------------------------

plot(FLStocks(of=hke.stk+fit_of0, ti=hke.stk+fit_ti0, obs=hke.stk))
save(fit_de, fit_of, fit_ti, fit_de0, fit_of0, fit_ti0, hke.stk, hke.idx, file="conv_tests.RData")


