pil.idx <- pil.idx[-2]

#------------------------------------------------------------------------------
# official
#------------------------------------------------------------------------------

fmod <- ~s(age, k = 4) + s(year, k = 21) #+ te(age, year, k = c(3, 5))
srmod <- ~factor(replace(year, year<1997, 1997))
## Operating model
om.c <- pil.stk
fit_of0 <- sca(om.c, pil.idx, fmodel=fmod, srmodel=srmod)
fit <- simulate(fit_of0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- pil.idx
for(i in 1:length(idx.oem)) index(idx.oem[[i]]) <- index(fit)[[i]]

fit_of <- sca(stk.oem, idx.oem, fmodel=fmod, srmodel=srmod)
mean(fitSumm(fit_of)["convergence",]==1)
# [1] 0.998

#------------------------------------------------------------------------------
# ti
#------------------------------------------------------------------------------

fmod <- ~  s(age, k = 5)  + s(year, k = 24) + ti(age, year, k = c(5, 24))
## Operating model
om.c <- pil.stk
fit_ti0 <- sca(om.c, pil.idx, fmodel=fmod, srmodel=srmod)
fit <- simulate(fit_ti0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- pil.idx
for(i in 1:length(idx.oem)) index(idx.oem[[i]]) <- index(fit)[[i]]

fit_ti <- sca(stk.oem, idx.oem, fmodel=fmod, srmodel=srmod)
mean(fitSumm(fit_ti)["convergence",]==1)
# [1] 0.956

#------------------------------------------------------------------------------
# default
#------------------------------------------------------------------------------

## Operating model
om.c <- pil.stk
fit_de0 <- sca(om.c, pil.idx, srmodel=srmod)
fit <- simulate(fit_de0, it, seed=rs)

# update stock
om.c <- om.c + fit

## OEM
stk.oem <- om.c
idx.oem <- pil.idx
for(i in 1:length(idx.oem)) index(idx.oem[[i]]) <- index(fit)[[i]]

fit_de <- sca(stk.oem, idx.oem, srmodel=srmod)
mean(fitSumm(fit_de)["convergence",]==1)
# [1] 0.974

#------------------------------------------------------------------------------
# output
#------------------------------------------------------------------------------

plot(FLStocks(of=pil.stk+fit_of0, ti=pil.stk+fit_ti0, obs=pil.stk))
save(fit_de, fit_of, fit_ti, fit_de0, fit_of0, fit_ti0, pil.stk, pil.idx, file="conv_tests.RData")


