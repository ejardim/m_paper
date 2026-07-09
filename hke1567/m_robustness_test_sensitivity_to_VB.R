#==================================================================
# script for M analysis - sensitivity to VB parameters
# EJ
# 20251029
#==================================================================

#------------------------------------------------------------------
# load & set up
#------------------------------------------------------------------

library(FLa4a)
library(ggplotFL)
library(reshape2)
library(knitr)
# stock
load("../data/HKE_1_5_6_7_stk_input_assess.Rdata")
ages  <- as.numeric(dimnames(m(hke.stk))$age)
years <- as.numeric(dimnames(m(hke.stk))$year)
rng <- range(hke.stk)
# weights at age
wa <- yearMeans(stock.wt(hke.stk))
# index
load("../data/FLIndices_0.RData") # medits data fixed
hke.idx <- trim(idx[[1]], age=0:4, year=years)
# number of iters
it <- 250
# number of cores
cores <- 16

#------------------------------------------------------------------
# growth model and parameters
#------------------------------------------------------------------

# reference values for the stock
# k <- 0.178 (0.2)
# linf <- 110
# lmin <- 5
# l50 <- 29 (30)
base_scenario <- data.frame(k=0.2, linf=110, lmin=5, l50=30, scenario = "0.2:110:5:30")

t0 <- -0.005
k <- seq(0.1, 0.5, 0.1)
linf <- seq(80, 120, 10)
a <- 0.00677
b <- 3.0325
lmin <- seq(0,15,5)
l50 <- seq(20,40,5)
maxage <- t0-1/k*log(1-(linf-0.5)/linf)
wa <- yearMeans(stock.wt(hke.stk))
fact_design <- expand.grid(k=k, linf=linf, lmin=lmin, l50=l50)

#------------------------------------------------------------------
# M models
#------------------------------------------------------------------
lst0 <- split(fact_design, fact_design)
lst0 <- lapply(lst0, function(x){

    k <- x$k
    linf <- x$linf
    lmin <- x$lmin
    l50 <- x$l50
    maxage <- t0-1/k*log(1-(linf-0.5)/linf)

    #VBGF function Bertalanffy Growth Function (VBGF):
    hke1567.vb <- a4aGr(
        grMod=~linf*(1-exp(-k*(t-t0))),
        grInvMod=~t0-1/k*log(1-len/linf),
        params=FLPar(linf=linf, k=k, t0=t0, units=c("cm","year-1","year"))
    )
    m0 <- matrix(NA, ncol=length(ages), nrow=3, dimnames=list(model=c("cw", "t", "b"), age=ac(ages)))

    mChen_sim <- FLModelSim(model=~k / (1 - exp(-k * (age - t0))),
        params=FLPar(k=k, t0 = t0))
    m0[1,] <- predict(mChen_sim, age=ages+0.5)


    mThen_sim <- FLModelSim(model=~4.899 * max_age^(-0.916),
        params=FLPar(max_age=maxage))
    m0[2,] <- predict(mThen_sim)

    mBrod_sim <- FLModelSim(model=~k*l50/len, params=FLPar(l50=l50, k=k))
    mBrod <- FLQuant(dimnames=list(len=lmin:linf))
    mBrod[] <- predict(mBrod_sim, len=lmin:linf+0.5)
    mBrod <- l2a(mBrod, hke1567.vb, stat="mean")
    m0[3,ac(ages) %in% dimnames(mBrod)$age] <- mBrod[dimnames(mBrod)$age %in% ac(ages)]
    df0 <- melt(m0)
    df0$k <- k
    df0$linf <- linf
    df0$lmin <- lmin
    df0$l50 <- l50
    df0

})

m_sensitivity_vb <- do.call("rbind", lst0)
m_sensitivity_vb$scenario <- apply(m_sensitivity_vb[,4:7], 1, paste, collapse=":")

#------------------------------------------------------------------
# plots
#------------------------------------------------------------------

# varying all
p1 <- ggplot(m_sensitivity_vb, aes(x = age, y = value, group = interaction(k, linf, lmin, l50))) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "M value by model",
    x = "Age",
    y = "Value",
  )

# varying k
df0 <- subset(m_sensitivity_vb, linf==110 & lmin==5 & l50==30)

p2 <- ggplot(df0, aes(x = age, y = value, group = interaction(k, linf, lmin, l50), color=k)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "M value by model for fixed linf=110, lmin=5 and l50=30; k varying",
    x = "Age",
    y = "Value",
  )

# varying linf
df0 <- subset(m_sensitivity_vb, k==0.2 & lmin==5 & l50==30)
p3 <- ggplot(df0, aes(x = age, y = value, group = interaction(k, linf, lmin, l50), color=linf)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "M value by model for fixed k=0.2, lmin=5 and l50=30; linf varying",
    x = "Age",
    y = "Value",
  )


# varying lmin
df0 <- subset(m_sensitivity_vb, k==0.2 & linf==110 & l50==30)
p4 <- ggplot(df0, aes(x = age, y = value, group = interaction(k, linf, lmin, l50), color=lmin)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "M value by model for fixed k=0.2, linf=110 and l50=30; lmin varying",
    x = "Age",
    y = "Value",
  )

# varying l50
df0 <- subset(m_sensitivity_vb, k==0.2 & linf==110 & lmin==5)
p5 <- ggplot(df0, aes(x = age, y = value, group = interaction(k, linf, lmin, l50), color=l50)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "M value by model for fixed k=0.2, linf=110 and lmin=5; l50 varying",
    x = "Age",
    y = "Value",
  )


sensitivity_plot <- ggarrange(
  p1, p2, p3, p4, p5,
  labels = "AUTO",
  ncol = 1,
  nrow = 5,
  common.legend = FALSE,
  legend = "right"
)

ggsave("sensitivity_plot.png",
       plot = sensitivity_plot,
       width = 10,
       height = 10,
       units = "in",
       dpi = 300)
