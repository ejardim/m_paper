#------------------------------------------------------------------------------
# official
#------------------------------------------------------------------------------

fmod <- ~factor(age) + factor(year)
qmod <- list(~factor(replace(age, age > 4, 4)))
srmod <- ~s(year, k = 4)
## Operating model
om.c <- nep.stk
fit_of0 <- sca(om.c, nep.idx, fmodel=fmod, qmodel=qmod, srmodel = srmod)
fit <- simulate(fit_of0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- nep.idx
index(idx.oem)[[1]] <- index(fit)[[1]]

fit_of <- sca(stk.oem, idx.oem, fmodel=fmod, qmodel=qmod)
mean(fitSumm(fit_of)["convergence",]==1)
# [1] 0.546

#------------------------------------------------------------------------------
# ti
#------------------------------------------------------------------------------

fmod <- ~  s(age, k = 6)  + s(year, k = 8) + ti(age, year, k = c(6, 8))
## Operating model
om.c <- nep.stk
fit_ti0 <- sca(om.c, nep.idx, fmodel=fmod, srmodel = srmod)
fit <- simulate(fit_ti0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- nep.idx
index(idx.oem)[[1]] <- index(fit)[[1]]

fit_ti <- sca(stk.oem, idx.oem, fmodel=fmod, srmodel = srmod)
mean(fitSumm(fit_ti)["convergence",]==1)
# [1] 0.024

#------------------------------------------------------------------------------
# default
#------------------------------------------------------------------------------

## Operating model
om.c <- nep.stk
fit_de0 <- sca(om.c, nep.idx)
fit <- simulate(fit_de0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- nep.idx
index(idx.oem)[[1]] <- index(fit)[[1]]

fit_de <- sca(stk.oem, idx.oem)
mean(fitSumm(fit_de)["convergence",]==1)
# [1] 0.048

#------------------------------------------------------------------------------
# output
#------------------------------------------------------------------------------

plot(FLStocks(of=nep.stk+fit_of0, ti=nep.stk+fit_ti0, obs=nep.stk))
save(fit_de, fit_of, fit_ti, fit_de0, fit_of0, fit_ti0, nep.stk, nep.idx, file="conv_tests.RData")


