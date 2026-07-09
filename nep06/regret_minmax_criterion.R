#idea for performance criterion, to enable decision making:
#minimax and regret calculation for both F and SSB MAD:
#How wrong was the assessment when truth = X and I assumed Y?”
#Do we prefer a model that SOMETIMES looks excellent but occasionally disastrous?
#or a model that is NEVER the best but also never catastrophic?
#and F and SSB analysis together: Is the same M robust for both? Or does management risk (F) 
#and status risk (SSB) point to different models?

###########################
#define RMSE or MAD matrix#
###########################
#Quadratic penalty, Punishes occasional large errors heavily, Highlights 
#“catastrophic misspecification”. Preferred in our analysis since we want the impact
#of extremes?
rmse_fun <- function(mp, om) {
  sqrt(mean((mp - om)^2))
}

#or MAD?
#Linear penalty, Less sensitive to rare catastrophic outcomes, Measures average deviation
mad_fun <- function(mp, om) {
  mean(abs(mp - om))
}

####################################################################################
#Read data from the fit OMs and management procedures (assuming different M models)#
####################################################################################
#FLStocks with c,b,l and t M assumptions and the simulated "true" OM, c is currently
#the assumed right one
load("output_c.RData")
load("output_b.RData")
load("output_l.RData")
load("output.t.RData")

OMs <- list(
  Current   = output.c,
  Then      = output.t,
  Lorenz    = output.l,
  Brodziak  = output.b
)

MP_names <- c("c","t","l","b")

#for F
loss_F <- matrix(NA, nrow=4, ncol=4)
rownames(loss_F) <- names(OMs)
colnames(loss_F) <- c("Current","Then","Lorenz","Brodziak")

for(i in seq_along(OMs)) {
  om_obj <- OMs[[i]]
  om_F   <- as.numeric(fbar(om_obj$om))
  
  for(j in seq_along(MP_names)) {
    mp_F <- as.numeric(fbar(om_obj[[ MP_names[j] ]]))
    loss_F[i,j] <- mad_fun(mp_F, om_F)
  }
}

#for SBB
loss_SSB <- matrix(NA, nrow=4, ncol=4)
rownames(loss_SSB) <- names(OMs)
colnames(loss_SSB) <- c("Current","Then","Lorenz","Brodziak")

for(i in seq_along(OMs)) {
  
  om_obj <- OMs[[i]]
  om_SSB <- as.numeric(ssb(om_obj$om))
  
  for(j in seq_along(OMs)) {
    mp_SSB <- as.numeric(ssb(om_obj[[ MP_names[j] ]]))
    loss_SSB[i,j] <- mad_fun(mp_SSB, om_SSB)
  }
}

apply(loss_SSB, 1, which.min)
#Current     Then   Lorenz Brodziak 
#1        1        3        3 
#Current and Then M produce similar SSB structure.
#Lorenz and Brodziak produce similar SSB structure.--> two distinct model types

apply(loss_SSB, 2, max)
#Current     Then   Lorenz Brodziak 
#2507.328 2502.447 2327.100 2552.328 
#robustness decision is driven entirely by the Lorenz scenario being extreme.




#########
#Minimax#
#########
#For F
#Choose the M model whose worst-case MAD(F) is smallest.
worst_case_F <- apply(loss_F, 2, max)
minimax_F <- names(which.min(worst_case_F))

#Choose the M model that never performs much worse than the best possible model in hindsight.
best_per_om_F <- apply(loss_F, 1, min)
regret_F <- sweep(loss_F, 1, best_per_om_F, "-")
max_regret_F <- apply(regret_F, 2, max)
minimax_regret_F <- names(which.min(max_regret_F))

##SSB
#Choose the M model whose worst-case MAD(SSB) is smallest.
worst_case_SSB <- apply(loss_SSB, 2, max)
minimax_SSB <- names(which.min(worst_case_SSB))

#Choose the M model that never performs much worse than the best possible model in hindsight.
best_per_om_SSB <- apply(loss_SSB, 1, min)
regret_SSB <- sweep(loss_SSB, 1, best_per_om_SSB, "-")
max_regret_SSB <- apply(regret_SSB, 2, max)
minimax_regret_SSB <- names(which.min(max_regret_SSB))


loss_F
worst_case_F
minimax_F
max_regret_F
minimax_regret_F
